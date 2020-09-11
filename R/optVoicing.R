# optVoicing.R
# =============================================================================
# Daniel Duran
# Albert-Ludwigs-Universität Freiburg, Germany
# daniel.duran@germanistik.uni-freiburg.de
# http://simphon.net/
#
# =============================================================================
#                                                                INITIALIZATION
# =============================================================================
rm(list = ls())
library('tidyverse')
library('textgRid')
library('lme4')

set.seed(42)

KONFIG <- list(isWindows      = str_detect(Sys.getenv('OS'), pattern = "(W|w)indows") | str_detect(Sys.getenv('SystemRoot'), pattern = "(W|w)indows"),
               trainTestRatio = 0.85,  # SET TO NA IN ORDER TO USE ALL DATA FOR OPTIMIZATION
               trainMinimum   = 5,     # THE ABSOLUTE MINIMUM OF REQUIRED TOKENS PER LABEL
               runTest        = FALSE, # ONLY IF trainTestRatio IS NOT NA
               # SKIPPING THE TEST SET MAY BE USEFUL IF YOU WANT/HAVE TO RESTART THE OPTIMIZATION REPEATEDLY

               # PRAAT PARAMETERS:
               pitch_floor_range          = c( 30, 180, 180-30),
               pitch_ceiling_range        = c(350, 950, 950-350),
               silence_threshold_range    = c(0.01, 0.75, 0.75-0.01),
               voicing_threshold_range    = c(0.20, 0.9, 0.9-0.2),
               voiced_unvoiced_cost_range = c(0.01, 0.55, 0.55-0.01),
               phone_tier                 = "PHO",
               phone_segments             = "p,t,k,b,d,g,<V>",

               # SEARCH PARAMETERS:
               start               = "random", # available options: "default", "random", "previous"
               RANDOM_LOW          = 1e-8,
               RANDOM_HIGH         = 1 - 1e-8,
               ROUND_HZ_PARAMETERS = TRUE,
               parameterName_index = list(pitch_floor=1, pitch_ceiling=2, silence_threshold=3, voicing_threshold=4, voiced_unvoiced_cost=5),
               DEFAULT_ERROR       = 30,
               verbose             = FALSE,

               # SETTINGS FOR THE NELDER-MEAD FUNCTION:
               #   DEFAULTS -> SEE PACKAGE DOCUMENTATION FOR lme4
               #   - maxfun  (default 10000) maximum number of function evaluations (you may want to set this to a very small number first to test the script!)
               #   - FtolAbs (default 1e-5)  absolute tolerance on change in function values
               #   - XtolRel (default 1e-7)  relative tolerance on change in parameter values
               NelderMead       = list(verbose = 2, ftolabs = 1e-5, xtorel = 1e-7, max_fun = 999),
               NM_RESULT_PREFIX = "nm_result",

               # ADJUST THESE PATHS:
               PRAAT.EXE      = '/usr/bin/praat',
               PRAAT.SCRIPT   = '/path/to/PP2020.git/Praat/Voicing/voice-Advanced.praat',
               outputDir      = '/home/dd/Scratch/Optimization/Nelder-Mead',
               files.csv      = '/home/dd/Scratch/Optimization/files.csv'
)


# -----------------------------------------------------------------------------
# CHECKING CONFIGURATION:

if(is.na(KONFIG$trainTestRatio)){
  KONFIG$doTrainTest <- FALSE
} else {
  if((KONFIG$trainTestRatio <= 0 || KONFIG$trainTestRatio >= 1)){
    stop("trainTestRatio must be in range (0,1)")
  }
  KONFIG$doTrainTest <- TRUE
}

# =============================================================================
#                                                     TEXTGRID HELPER FUNCTIONS
# =============================================================================


importTextGridToTibble <- function(textGridFile, add.interval.number=TRUE, fix.encoding=TRUE, add.global.time=TRUE) {
  stopifnot(file.exists(textGridFile))
  tg <- tryCatch(
    TextGrid(textGridFile),
    error=function(cond) {
      warning(gettextf("[import: %s]\n  %s", textGridFile, cond))
      return(NULL)
    },
    warning=function(cond) {
      warning(gettextf("[import: %s]\n  %s", textGridFile, cond))
      return(NULL)
    },
    finally={}
  )
  if(is.null(tg)) {
    return(NULL)
  }
  if(add.global.time) {
    tg.tbl <- tibble(tmin=tg@startTime,
                     tmax=tg@endTime,
                     text=NA_character_,
                     tier=NA_character_)
    if(add.interval.number){
      tg.tbl$interval <- 0
    }
  } else {
    tg.tbl <- tibble()
  }
  tgTierNames <- names(tg)
  for(tx in tgTierNames) {
    tier <- tg[[tx]]
    N <- length(tier@labels)
    if(N>0){
      if( class(tier) == "IntervalTier" ){
        tmp.tbl <- tibble(tmin=tier@startTimes,
                          tmax=tier@endTimes,
                          text=tier@labels,
                          tier=tier@name)
      } else {
        tmp.tbl <- tibble(tmin=tier@times,
                          tmax=NA,
                          text=tier@labels,
                          tier=tier@name)
      }
      if(add.interval.number){
        tmp.tbl$interval <- 1:N
      }
      tg.tbl <- rbind(tg.tbl,tmp.tbl)
      rm(tmp.tbl)
    }
  }
  rm(tg, tgTierNames, tx)
  if(fix.encoding) {
    # FIX ENCODING
    # ATTENTION: THIS SEEMS TO BE NECESSARY ON WINDOWS SYSTEMS
    tg.tbl$text <- iconv(iconv(tg.tbl$text, from="UTF-8", to = "Windows-1252"), to="UTF-8")
  }
  return(tg.tbl)
}



# -----------------------------------------------------------------------------
# RETURNS A NAMED LIST
get_interval <- function(tg, tier.name, t=NA, i.num=NA) {
  if(is.na(t)){
    return( as.list(filter(tg, tier==tier.name, interval==i.num)) )
  } else {
    return( as.list(filter(tg, tier==tier.name, tmin<=t & tmax>t)) )
  }
}

# -----------------------------------------------------------------------------
get_point_at <- function(tg, point.tier, t) {
  return( as.list(filter(tg, tier==point.tier, tmin==t)) )
}


# =============================================================================
#
# =============================================================================

#' @param gold.files.tbl A tibble holding the data from the file <KONFIG$files.csv>
#'
load_gold_voice_annotations <- function(gold.files.tbl,
                                        target.tier = "PHO",
                                        target.segments = c("p","t","k","b","d","g","<V>"),
                                        voice.tier="voice", confidence.tier="confidence",
                                        voiced.label="V", squared.confidence = TRUE,
                                        train.test.ratio = NA
) {

  gold <- tibble(index    = integer(),
                 interval = integer(),
                 label    = character(),
                 duration = double(),
                 voiced   = double(),
                 unvoiced = double(),
                 conf.v   = double(),
                 conf.u   = double()  )

  for(iRow in 1:nrow(gold.files.tbl)){

    goldTG   <- importTextGridToTibble(gold.files.tbl[iRow,]$gold.voice.file)
    gold.pho <- filter(goldTG, tier==target.tier, text %in% target.segments)

    for(tx in 1:nrow(gold.pho)){

      tMin         <- gold.pho[tx,]$tmin
      intervalGold <- get_interval(tg = goldTG, tier.name=voice.tier, t = tMin)

      if(is.na(intervalGold$text) || str_length(intervalGold$text)==0){
        next()
      } else {

        tMax    <- gold.pho[tx,]$tmax
        dur     <- tMax - tMin

        v_dur  <- 0.0
        subDurations   <- c()
        subConfidences <- c()
        subVoiced      <- c()

        currentStart <- tMin

        while(intervalGold$tmax <= tMax) {
          subDur <- intervalGold$tmax - currentStart
          isVoice <- FALSE
          if(intervalGold$text==voiced.label) {
            v_dur <- v_dur + subDur
            isVoice <- TRUE
          }
          subDurations <- c(subDurations, subDur)
          subVoiced    <- c(subVoiced, isVoice)
          konf <- get_point_at(tg=goldTG, point.tier = confidence.tier, t=currentStart)$text
          if(length(konf) > 0){
            subConfidences <- c(subConfidences, as.numeric(konf)  )
          } else {
            subConfidences <- c(subConfidences, NA  )
          }
          intervalGold <- get_interval(tg = goldTG, tier.name=voice.tier, t = intervalGold$tmax)
          currentStart <- intervalGold$tmin
        }

        if(currentStart < tMax) {
          warning(gettextf("Voice label extending beyond phone segment? [%s]: [%s] %.4f > %.4f in %s\n",
                           gold.pho[tx,]$text, intervalGold$text, intervalGold$tmax, tMax,
                           gold.files.tbl[iRow,]$gold.voice.file ))
          subDur <- tMax - currentStart
          if(intervalGold$text==voiced.label) {
            v_dur  <- v_dur + subDur
          }
          subDurations   <- c(subDurations, subDur)
          konf <- get_point_at(tg=goldTG, point.tier = confidence.tier, t=currentStart)$text
          if(length(konf) > 0){
            subConfidences <- c(subConfidences, as.numeric(konf)  )
          } else {
            subConfidences <- c(subConfidences, NA  )
          }
        }

        if(v_dur>0) {
          v_perc <- v_dur / dur
        } else {
          v_perc <- 0.0
        }

        subConf_v <- subConfidences[subVoiced]
        subConf_u <- subConfidences[!subVoiced]

        subDur_v  <- subDurations[subVoiced]
        subDur_u  <- subDurations[!subVoiced]

        if(length(subConf_v)>0) {
          conf_v <- weighted.mean(x=subConf_v, w=(subDur_v / dur))
        } else {
          conf_v <- NA
        }
        if(length(subConf_u)>0) {
          conf_u <- weighted.mean(x=subConf_u, w=(subDur_u / dur))
        } else {
          conf_u <- NA
        }

        gold <- add_row(gold,
                        index    = iRow,
                        interval = gold.pho[tx,]$interval,
                        label    = gold.pho[tx,]$text,
                        duration = dur,
                        voiced   = v_perc,
                        unvoiced = 1.0-v_perc,
                        conf.v   = conf_v,
                        conf.u   = conf_u  )
      }# ENDIF
    }#ENDFOR tx
  }#ENDFOR iRow

  if(squared.confidence) {
    # USE SQUARED CONFIDENCE VALUES:
    gold$conf.v <- gold$conf.v ^2
    gold$conf.u <- gold$conf.u ^2
  }

  if(is.na(train.test.ratio)){
    gold$train <- TRUE
  } else {
    gold$train <- FALSE
    allRooms   <- sort(unique(DATAFILES$room))
    allLabels  <- sort(unique(gold$label))
    for(xRoom in allRooms) {
      roomIndex <- which(DATAFILES$room == xRoom)
      for(lx in allLabels){
        labelIndex <- which(gold$label == lx & gold$index %in% roomIndex)
        M <- length(labelIndex)
        if(M==0){
          warning(gettextf("No [%s] labels for room <%s>", lx, xRoom))
          next()
        }
        N <- ceiling(M*train.test.ratio)
        gold[ sample(labelIndex, size = if_else(N==M, N-1, N)), ]$train <- TRUE
      }
    }
  }


  return(gold)
}

# =============================================================================
#                                                      PRAAT AND SEARCH HELPERS
# =============================================================================

DEFAULT_VOICEADVANCED <- list(time.step            = 0.0,
                              pitch.floor          = 75,
                              max.candidates       = 15,
                              very.accurate        = TRUE,
                              silence.threshold    = 0.03,
                              voicing.threshold    = 0.45,
                              octave.cost          = 0.01,
                              octave.jump.cost     = 0.35,
                              voiced.unvoiced.cost = 0.14,
                              pitch.ceiling        = 600,
                              max.period.factor    = 1.3,
                              max.amplitude.factor = 1.6,
                              verbose              = FALSE,
                              write.log.file       = TRUE   )


# -----------------------------------------------------------------------------
#                                                                      SACALING
min_max_norm <- function(val, val.range, reverse=FALSE) {
  if(reverse){
    return( (val * val.range[3]) + val.range[1] )
  } else {
    return( (val - val.range[1]) / val.range[3] )
  }
}


# -----------------------------------------------------------------------------
#                                                   HELPER FUNCTION FOR FILE IO

format_room <- function(room) {
  room <- str_replace_all(room, "\\s+", "_")
  room <- str_replace_all(room, "\\.+", "_")
  return(room)
}


# -----------------------------------------------------------------------------

find_previous_optima <- function(result.dir, all.rooms, default.p0, file.pattern) {

  p0List <- vector("list", length = length(all.rooms))
  names(p0List) <- all.rooms

  for(xRoom in all.rooms) {

    roomDir  <- file.path(result.dir, format_room(xRoom))

    allNMresults <- dir(roomDir, full.names = TRUE, pattern = file.pattern)

    if(length(allNMresults)==0) {
      warning(gettextf("No Nelder-Mead results for room <%s> - Using default parameters", xRoom))
      p0List[[xRoom]] <- default.p0
      next()
    }

    roomOpt  <- Inf
    optPar   <- NA

    for(iNM in 1:length(allNMresults)) {# iNM=1

      nm.result <- readRDS(allNMresults[iNM])

      if(is.null("nm.result")) {
        warning(gettextf("No Nelder-Mead results found in [%s]", allNMresults[iNM]))
        next()
      }

      if( nm.result$fval < roomOpt) {
        roomOpt  <- nm.result$fval
        optPar   <- nm.result$par
      }
      rm(nm.result)
    }
    p0List[[xRoom]] <- optPar
  }
  return(p0List)
}


# -----------------------------------------------------------------------------
# THIS IS BAD PROGRAMMING STYLE (USING GLOBAL VARIABLES)
# see: opt_fun
get_parameters_from_NMx <- function(nm.x) {
  if(KONFIG$ROUND_HZ_PARAMETERS){
    p <- list(pitch_floor          = round(min_max_norm(nm.x[KONFIG$parameterName_index$pitch_floor],    KONFIG$pitch_floor_range, reverse=TRUE)),
              pitch_ceiling        = round(min_max_norm(nm.x[KONFIG$parameterName_index$pitch_ceiling],  KONFIG$pitch_ceiling_range, reverse=TRUE)),
              silence_threshold    = min_max_norm(nm.x[KONFIG$parameterName_index$silence_threshold],    KONFIG$silence_threshold_range, reverse=TRUE),
              voicing_threshold    = min_max_norm(nm.x[KONFIG$parameterName_index$voicing_threshold],    KONFIG$voicing_threshold_range, reverse=TRUE),
              voiced_unvoiced_cost = min_max_norm(nm.x[KONFIG$parameterName_index$voiced_unvoiced_cost], KONFIG$voiced_unvoiced_cost_range, reverse=TRUE) )
  } else {
    p <- list(pitch_floor          = min_max_norm(nm.x[KONFIG$parameterName_index$pitch_floor],          KONFIG$pitch_floor_range, reverse=TRUE),
              pitch_ceiling        = min_max_norm(nm.x[KONFIG$parameterName_index$pitch_ceiling],        KONFIG$pitch_ceiling_range, reverse=TRUE),
              silence_threshold    = min_max_norm(nm.x[KONFIG$parameterName_index$silence_threshold],    KONFIG$silence_threshold_range, reverse=TRUE),
              voicing_threshold    = min_max_norm(nm.x[KONFIG$parameterName_index$voicing_threshold],    KONFIG$voicing_threshold_range, reverse=TRUE),
              voiced_unvoiced_cost = min_max_norm(nm.x[KONFIG$parameterName_index$voiced_unvoiced_cost], KONFIG$voiced_unvoiced_cost_range, reverse=TRUE) )
  }
  return(p)
}


get_parameters_from_NMresult <- function(nm.result) {
  return(get_parameters_from_NMx(nm.result$par))
}



# -----------------------------------------------------------------------------
# FUNCTION "praat_voiceAdvanced"
# RUNS PRAAT SCRIPT "Voicing/voice-Advanced.praat" AND RETURNS THE VOICING TABLE

praat_voiceAdvanced <- function (sound.file, textGrid.file, target.tier, target.segments, output.tsv.file,
                                 praat.exe, praat.script,
                                 is.Windows,
                                 time.step            = 0.0,
                                 pitch.floor          = 75,
                                 max.candidates       = 15,
                                 very.accurate        = TRUE,
                                 silence.threshold    = 0.03,
                                 voicing.threshold    = 0.45,
                                 octave.cost          = 0.01,
                                 octave.jump.cost     = 0.35,
                                 voiced.unvoiced.cost = 0.14,
                                 pitch.ceiling        = 600,
                                 max.period.factor    = 1.3,
                                 max.amplitude.factor = 1.6,
                                 verbose              = FALSE,
                                 write.log.file       = TRUE  )
{
  stopifnot(file.exists(praat.exe),
            file.exists(praat.script),
            file.exists(sound.file),
            file.exists(textGrid.file) )

  if(round(pitch.floor)!=pitch.floor) {
    warning(gettextf("using rounded (integer) value for pitch.floor=%f with %s", pitch.floor, sound.file))
  }
  if(round(max.candidates)!=max.candidates) {
    warning(gettextf("using rounded (integer) value for max.candidates=%f with %s", max.candidates, sound.file))
  }
  if(round(pitch.ceiling)!=pitch.ceiling) {
    warning(gettextf("using rounded (integer) value for pitch.ceiling=%f with %s", pitch.ceiling, sound.file))
  }

  # CREATE A TEMPORARY PRAAT SCRIPT WHICH WILL RUN THE VOICING EXTRACTION SCRIPT WITH THE PROVIDED PARAMETERS:
  # (BECAUSE WINDOWS DOES NOT LIKE THE EXECUTION OF shell() WITH ARGUMENTS FOR THE SCRIPT;
  # AS A WORKAROUND WE WRITE ALL ARGUMENTS TO A TEMPORARY SCRIPT, WHICH IS THEN CALLED
  # WITHOUT ANY ADDITIONALY ARGUMENTS AND DELETED AFTERWARDS)
  praatScriptArgs <- gettextf("\"%s\", \"%s\", \"%s\", \"%s\", \"%s\", 0, %f, %d, %d, %d, %f, %f, %f, %f, %f, %d, %f, %f, %d, %d",
                              normalizePath(sound.file), normalizePath(textGrid.file),
                              target.tier, target.segments,
                              normalizePath(output.tsv.file, mustWork = FALSE),
                              time.step, round(pitch.floor), round(max.candidates), as.integer(very.accurate),
                              silence.threshold, voicing.threshold, octave.cost, octave.jump.cost, voiced.unvoiced.cost, round(pitch.ceiling),
                              max.period.factor, max.amplitude.factor, as.integer(verbose), as.integer(write.log.file)   )

  tmp.praat.script <- tempfile(fileext = ".praat")

  tmp.praat.script.content <- c("# TEMPORARY PRAAT SCRIPT")

  tmp.praat.script.content <- c(tmp.praat.script.content,
                                gettextf("runScript: \"%s\", %s", normalizePath(praat.script), praatScriptArgs) )

  dateiVerbindung <- file(tmp.praat.script)
  writeLines(tmp.praat.script.content, dateiVerbindung)
  close(dateiVerbindung)
  if(!file.exists(tmp.praat.script)) {
    warning(gettextf("Could not create temp script %s", tmp.praat.script), immediate. = TRUE)
  }

  # CHANGE TO TEMPORARY DIRECTORY:
  currentWD <- getwd()
  setwd(tempdir())

  # RUN PRAAT SCRIPT:
  kom <- gettextf("\"%s\" --run %s", normalizePath(praat.exe), basename(tmp.praat.script))
  if(is.Windows) {
    shell(kom)
  } else {
    system(kom)
  }

  if(file.exists(output.tsv.file)) {
    suppressMessages( voice.tbl <- read_tsv(output.tsv.file) )
  } else {
    warning(gettextf("[praat_voiceAdvanced] No Praat output found at %s", output.tsv.file), immediate. = TRUE)
    voice.tbl <- NULL
  }

  file.remove(tmp.praat.script)

  setwd(currentWD)
  return(voice.tbl)
}




# =============================================================================
#
# =============================================================================

evaluate_voice <- function(praat.tsv,     # path to Praat output file
                           gold.index,    # file index of gold annotations
                           default.error,
                           normalize.error=FALSE,
                           train = TRUE
) {

  if(train){
    gold <-  filter(GOLDVOICE, index == gold.index, train == TRUE)
  } else {
    gold <-  filter(GOLDVOICE, index == gold.index, train == FALSE)
  }

  fehlerV <- 0
  fehlerU <- 0
  for(rx in 1:nrow(gold)) {
    pred <- filter(praat.tsv, interval==gold[rx,]$interval)
    if(nrow(pred)>0) {
      if(is.na(pred[1,]$voiced)){
        warning(gettextf("PRAAT RESULT IS NA FOR %s: %.0f [%s]", DATAFILES[gold[rx,]$index,]$gold.voice.file, gold[rx,]$interval, gold[rx,]$lab), immediate. = TRUE)
        fv <- default.error
        fu <- default.error
      } else {
        if(is.na(gold[rx,]$conf.v)){
          fv <- 0
        } else {
          fv <- (abs(gold[rx,]$voiced - pred[1,]$voiced) * gold[rx,]$conf.v)
        }
        if(is.na(gold[rx,]$conf.u)){
          fu <- 0
        } else {
          fu <- (abs(gold[rx,]$unvoiced - pred[1,]$unvoiced) * gold[rx,]$conf.u)
        }
      }
      fehlerV <- fehlerV + fv
      fehlerU <- fehlerU + fu
    } else {
      warning(gettextf("NO PRAAT RESULT FOR %s: %.0f [%s]", DATAFILES[gold[rx,]$index,]$gold.voice.file, gold[rx,]$interval, gold[rx,]$lab), immediate. = TRUE)
      fehlerV <- fehlerV + default.error
      fehlerU <- fehlerU + default.error
    }
  }
  if(normalize.error) {
    fehlerV <- fehlerV / (nrow(gold) * default.error)
    fehlerU <- fehlerU / (nrow(gold) * default.error)
  }
  return( fehlerV + fehlerU )
}





# =============================================================================
#                                                      FUNCTION TO BE OPTIMIZED
# =============================================================================
# THIS IS BAD PROGRAMMING STYLE (USING GLOBAL VARIABLES)
# EVERYTHING NEEDS TO HAPPEN INSIDE THIS FUNCTION
# INPUT:  A NUMERIC VECTOR (THE PARAMETERS FOR PRAAT; MAPPED TO 0...1)
# OUTPUT: A SCALAR (THE ERROR TERM, THIS SHOULD BE MINIMIZED)

opt_fun <- function(x) {

  fehler <- 0

  # DETERMINE PARAMETER VALUES FOR PRAAT
  praatPar <- get_parameters_from_NMx(x)

  prevX <- which(OPT_PREVIOUS.tbl$pitch_floor==praatPar$pitch_floor &
                   OPT_PREVIOUS.tbl$pitch_ceiling==praatPar$pitch_ceiling &
                   OPT_PREVIOUS.tbl$silence_threshold==praatPar$silence_threshold &
                   OPT_PREVIOUS.tbl$voicing_threshold==praatPar$voicing_threshold &
                   OPT_PREVIOUS.tbl$voiced_unvoiced_cost==praatPar$voiced_unvoiced_cost)

  if(length(prevX)>0) {
    OPT_PREVIOUS.tbl[prevX,]$num.calls <-  OPT_PREVIOUS.tbl[prevX,]$num.calls + 1
    fehler <- OPT_PREVIOUS.tbl[prevX,]$error.term
    if(OPT_VERBOSE) cat(gettextf("FUN x=[%8.6f, %8.6f, %8.6f, %8.6f, %8.6f] -> RETURNING CACHED VALUE = %.6e\n", x[1], x[2], x[3], x[4], x[5], fehler))
    return(fehler)
  }

  if(OPT_VERBOSE) cat(gettextf("FUN x=[%8.6f, %8.6f, %8.6f, %8.6f, %8.6f] ", x[1], x[2], x[3], x[4], x[5]))

  # RUN PRAAT
  for(ix in 1:length(OPT_DATAINDEX)){# ix=1

    if(OPT_VERBOSE) cat('+')

    iFile   <- DATAFILES[OPT_DATAINDEX[ix],]$gold.voice.file
    wavFile <- DATAFILES[OPT_DATAINDEX[ix],]$audio.file
    tmp.tsv <- tempfile(fileext = ".tsv")

    tmp.tbl <- NULL

    suppressWarnings (
      tmp.tbl <- praat_voiceAdvanced(sound.file = wavFile, textGrid.file = iFile, target.tier = KONFIG$phone_tier, target.segments = KONFIG$phone_segments, output.tsv.file = tmp.tsv,
                                     pitch.floor = praatPar$pitch_floor,
                                     pitch.ceiling = praatPar$pitch_ceiling,
                                     silence.threshold = praatPar$silence_threshold,
                                     voicing.threshold = praatPar$voicing_threshold,
                                     voiced.unvoiced.cost = praatPar$voiced_unvoiced_cost,
                                     write.log.file = FALSE,
                                     praat.exe = KONFIG$PRAAT.EXE,
                                     praat.script = KONFIG$PRAAT.SCRIPT,
                                     is.Windows = KONFIG$isWindows)
    )


    if(is.null(tmp.tbl) || nrow(tmp.tbl)==0) {
      warning(gettextf("No Praat results for %s", iFile))
      #TODO SOME DEFAULT ERROR VALUE SHOULD BE ADDED HERE (DEPENDING ON THE NUMBER OF DATA POINTS)
    } else {
      if(OPT_VERBOSE) cat('.')

      # EVALUATE ACCURACY OF PRAAT'S VOICING ANALYSIS
      f <- evaluate_voice(praat.tsv=tmp.tbl, gold.index = OPT_DATAINDEX[ix], default.error=KONFIG$DEFAULT_ERROR, train = TRUE)
      if(is.na(f)) {
        warning(gettextf("ERROR IS NA FOR %s", iFile))
        f <- 0 # THIS SHOULD ACTUALLY BE A LARGE VALUE, SINCE WE ARE MINIMIZING FOR f!
      }
      fehler <- fehler + f
    }

    file.remove(tmp.tsv)
  }

  # EXPORT CURRENTLY EXPANDED SEARCH PARAMETERS:
  OPT_PREVIOUS.tbl <- add_row(OPT_PREVIOUS.tbl,
                              pitch_floor=praatPar$pitch_floor ,
                              pitch_ceiling=praatPar$pitch_ceiling ,
                              silence_threshold=praatPar$silence_threshold ,
                              voicing_threshold=praatPar$voicing_threshold ,
                              voiced_unvoiced_cost=praatPar$voiced_unvoiced_cost,
                              num.calls=1,
                              error.term=fehler )

  # RETURN ERROR
  if(OPT_VERBOSE) cat(gettextf(" VALUE (VOICE ERROR) = %.6e\n", fehler))
  return(fehler)
}


# =============================================================================
#
# =============================================================================

plot_and_save_results <- function(nm.result, outputDir, room, prev.par.file)
{
  stempel <- format(Sys.time(), format="%Y-%m-%d+%H%M%S")
  outFile <- file.path(outputDir, paste0("nm_results_", stempel, ".rds"))

  cat(gettextf("NELDER-MEAD RESULTS (ROOM %s), %s:\n", room, stempel))

  saveRDS(object = nm.result, file = outFile)

  cat(gettextf("  OPTIMAL    x=[%f, %f, %f, %f, %f]\n", nm.result$par[1], nm.result$par[2], nm.result$par[3], nm.result$par[4], nm.result$par[5] ))
  cat(gettextf("  OPTIMAL fval=%f\n", nm.result$fval))
  cat(gettextf("  CONVERGENCE =(%d) %s\n", nm.result$convergence, nm.result$message))

  optimalParameters <- get_parameters_from_NMresult(nm.result)
  cat(gettextf("  PARAMETERS  =[%8.6f, %8.6f, %8.6f, %8.6f, %8.6f]\n", optimalParameters[1], optimalParameters[2], optimalParameters[3], optimalParameters[4], optimalParameters[5]))

  prev_pars.tbl <- readRDS(prev.par.file)
  cat(gettextf("Number of parameter combinations: %4d\nTotal number of function calls:   %4d\n", nrow(prev_pars.tbl), sum(prev_pars.tbl$num.calls)   ))
}



# -----------------------------------------------------------------------------

backup_File <- function(f, suffix=NULL, warn=TRUE) {
  ok <- TRUE
  if(file.exists(f)) {
    if(is.null(suffix)){
      suffix <- format(file.mtime(f), format="%Y-%m-%d+%H%M%S")
    }
    pLoc <- str_locate(f, "\\.")
    newFile <- str_c( str_sub(f, end=pLoc[1]-1), '_',  suffix,  str_sub(f, start=pLoc[1]) )
    ok <- file.copy(from=f, to=newFile, copy.date = TRUE )
  } else {
    if(warn){
      warning(gettextf("File not found: %s", f))
    }
    ok <- FALSE
  }
  return(ok)
}



# =============================================================================
#
# =============================================================================

cat("PREPARING GOLD DATA...\n")

DATAFILES <- read_csv(KONFIG$files.csv)
GOLDVOICE <- load_gold_voice_annotations(gold.files.tbl = DATAFILES, train.test.ratio = ifelse(KONFIG$doTrainTest, KONFIG$trainTestRatio, NA))

cat("INITIALIZING OUTPUT DIRECTORIES...\n")
allRooms <- sort(unique(DATAFILES$room))
for(xRoom in allRooms) {
  dir.create(path = file.path(KONFIG$outputDir, format_room(xRoom)), recursive = TRUE)
}


cat("INITIALIZING VECTOR x0...\n")
# INITIAL PARAMETERS = PRAAT DEFAULTS
praat_p0 <- c(min_max_norm(DEFAULT_VOICEADVANCED$pitch.floor, KONFIG$pitch_floor_range),
              min_max_norm(DEFAULT_VOICEADVANCED$pitch.ceiling, KONFIG$pitch_ceiling_range),
              min_max_norm(DEFAULT_VOICEADVANCED$silence.threshold, KONFIG$silence_threshold_range),
              min_max_norm(DEFAULT_VOICEADVANCED$voicing.threshold, KONFIG$voicing_threshold_range),
              min_max_norm(DEFAULT_VOICEADVANCED$voiced.unvoiced.cost, KONFIG$voiced_unvoiced_cost_range)  )

if(KONFIG$start == "default") {
  cat("STARTING WITH DEFAULT VECTOR x0\n")
  p0 <- praat_p0
  p0List <- NULL
} else if(KONFIG$start == "random") {
  cat("STARTING WITH RANDOM VECTOR x0\n")
  p0 <- runif(length(praat_p0), min = KONFIG$RANDOM_LOW, max = KONFIG$RANDOM_HIGH)
  p0List <- NULL
} else if(KONFIG$start == "previous") {
  cat("STARTING WITH PREVIOUSLY GENERATED VECTOR x0\n")
  p0 <- NULL
  p0List <- find_previous_optima(KONFIG$outputDir, all.rooms=allRooms, default.p0=praat_p0, file.pattern = paste0(KONFIG$NM_RESULT_PREFIX, ".+\\.rds"))
} else {
  stop(gettextf("Unknown parameter in KONFIG$start: \"%s\"", KONFIG$start))
}


# -----------------------------------------------------------------------------
#                                                  RUN NELDER-MEAD OPTIMIZATION
# -----------------------------------------------------------------------------
cat("RUNNING NELDER-MEAD OPTIMIZATION FOR ALL ROOMS...\n")

OPT_VERBOSE <- KONFIG$verbose

for(iRoom in 1:length(allRooms)) {# iRoom = 1
  cat(gettextf("OPTIMIZING FOR ROOM: <%s>\n", allRooms[iRoom]))

  OPT_DATAINDEX <- which(DATAFILES$room == allRooms[iRoom])

  if(length(OPT_DATAINDEX)==0) {
    warning(gettextf("No training data for room <%s>?", allRooms[iRoom]))
    next()
  }

  if(!is.null(p0List)) {
    p0 <- p0List[[ allRooms[iRoom] ]]
  }# ELSE all rooms use the same p0 (either default or random)

  outDir <- file.path(KONFIG$outputDir, format_room(allRooms[iRoom]))

  OPT_PREVIOUS_PARAMETERS_FILE <- file.path(outDir, "prev_pars_tbl.rds")

  if(file.exists(OPT_PREVIOUS_PARAMETERS_FILE)) {
    OPT_PREVIOUS.tbl <- readRDS(OPT_PREVIOUS_PARAMETERS_FILE)
  } else {
    OPT_PREVIOUS.tbl <- tibble(pitch_floor=numeric(),
                               pitch_ceiling=numeric(),
                               silence_threshold=numeric(),
                               voicing_threshold=numeric(),
                               voiced_unvoiced_cost=numeric(),
                               num.calls=integer(),
                               error.term=numeric())
  }

  nm.result <- Nelder_Mead(opt_fun, p0,
                           lower=rep(0,length(p0)), upper=rep(1,length(p0)),
                           control=list(maxfun=KONFIG$NelderMead$max_fun, verbose=KONFIG$NelderMead$verbose, FtolAbs=KONFIG$NelderMead$ftolabs, XtolRel=KONFIG$NelderMead$xtorel))

  saveRDS(OPT_PREVIOUS.tbl, file = OPT_PREVIOUS_PARAMETERS_FILE)

  plot_and_save_results(nm.result, outputDir=outDir, room=allRooms[iRoom], prev.par.file=OPT_PREVIOUS_PARAMETERS_FILE )

}#ENDFOR iRoom

# -----------------------------------------------------------------------------
# FINALLY: COLLECT OPTIMAL PARAMETERS

optimalParFile  <- file.path(KONFIG$outputDir, "p0List.rds")
cat(gettextf("SAVING OPTIMAL PARAMETERS IN OBJECT p0List TO FILE %s\n", optimalParFile))

p0List <- find_previous_optima(KONFIG$outputDir, all.rooms=allRooms, default.p0=praat_p0, file.pattern = paste0(KONFIG$NM_RESULT_PREFIX, ".+\\.rds"))
if(file.exists(optimalParFile)) {
  backup_File(optimalParFile)
}
saveRDS(p0List, file=optimalParFile)

warning("UNTESTED CODE FOLLOWING...\n")


cat("EXPORTING OPTIMAL PARAMETERS TO CSV FILE...\n")
praatParams.tbl <- tibble(room = character(),
                          pitch_floor = double(),
                          pitch_ceiling = double(),
                          silence_threshold = double(),
                          voicing_threshold = double(),
                          voiced_unvoiced_cost = double() )
rooms <- names(p0List)
for(ix in 1:length(p0List)){

  p0 <- get_parameters_from_NMx(p0List[[ix]])

  praatParams.tbl <- add_row(praatParams.tbl,
                             room = rooms[ix],
                             pitch_floor = p0$pitch_floor,
                             pitch_ceiling = p0$pitch_ceiling,
                             silence_threshold = p0$silence_threshold,
                             voicing_threshold = p0$voicing_threshold,
                             voiced_unvoiced_cost = p0$voiced_unvoiced_cost
  )
}
write_csv(praatParams.tbl, path = file.path(KONFIG$outputDir, "praatParams.csv"))



# =============================================================================
#
# =============================================================================

if(KONFIG$doTrainTest) {
  if(KONFIG$runTest) {
    cat("RUNNING PRAAT ON TEST SET...\n")

    DATAFILES$test.N    <- NA_integer_
    DATAFILES$error.opt <- NA_real_
    DATAFILES$error.def <- NA_real_

    for(ix in 1:nrow(DATAFILES)) {
      xRoom <- DATAFILES[ix,]$room

      tmp.tbl <- filter(GOLDVOICE, index == ix, train == FALSE)
      DATAFILES[ix,]$test.N <- nrow(tmp.tbl)
      if(nrow(tmp.tbl)==0) {
        warning(gettextf("No \"test\" items in %s?", DATAFILES[ix,]$gold.voice.file))
        rm(tmp.tbl)
        next()
      }
      rm(tmp.tbl)

      praatPar <- as.list( praatParams.tbl[praatParams.tbl$room == xRoom,] )

      iFile   <- DATAFILES[ix,]$gold.voice.file
      wavFile <- DATAFILES[ix,]$audio.file

      # USE OPTIMIZED PARAMETERS:
      tmp.tsv <- tempfile(fileext = ".tsv")
      suppressWarnings (
        tmp.tbl <- praat_voiceAdvanced(sound.file = wavFile, textGrid.file = iFile, target.tier = KONFIG$phone_tier, target.segments = KONFIG$phone_segments, output.tsv.file = tmp.tsv,
                                       pitch.floor = praatPar$pitch_floor,
                                       pitch.ceiling = praatPar$pitch_ceiling,
                                       silence.threshold = praatPar$silence_threshold,
                                       voicing.threshold = praatPar$voicing_threshold,
                                       voiced.unvoiced.cost = praatPar$voiced_unvoiced_cost,
                                       write.log.file = FALSE,
                                       praat.exe = KONFIG$PRAAT.EXE,
                                       praat.script = KONFIG$PRAAT.SCRIPT,
                                       is.Windows = KONFIG$isWindows)
      )
      DATAFILES[ix,]$error.opt <- evaluate_voice(praat.tsv=tmp.tbl, gold.index = ix, default.error=KONFIG$DEFAULT_ERROR, train = FALSE)
      rm(tmp.tsv, tmp.tbl)

      # USE DEFAULT PARAMETERS:
      tmp.tsv <- tempfile(fileext = ".tsv")
      suppressWarnings (
        tmp.tbl <- praat_voiceAdvanced(sound.file = wavFile, textGrid.file = iFile, target.tier = KONFIG$phone_tier, target.segments = KONFIG$phone_segments, output.tsv.file = tmp.tsv,
                                       pitch.floor = DEFAULT_VOICEADVANCED$pitch.floor,
                                       pitch.ceiling = DEFAULT_VOICEADVANCED$pitch.ceiling,
                                       silence.threshold = DEFAULT_VOICEADVANCED$silence.threshold,
                                       voicing.threshold = DEFAULT_VOICEADVANCED$voicing.threshold,
                                       voiced.unvoiced.cost = DEFAULT_VOICEADVANCED$voiced.unvoiced.cost,
                                       write.log.file = FALSE,
                                       praat.exe = KONFIG$PRAAT.EXE,
                                       praat.script = KONFIG$PRAAT.SCRIPT,
                                       is.Windows = KONFIG$isWindows)
      )
      DATAFILES[ix,]$error.def <- evaluate_voice(praat.tsv=tmp.tbl, gold.index = ix, default.error=KONFIG$DEFAULT_ERROR, train = FALSE)
      rm(tmp.tsv, tmp.tbl)
    }
  } else {
    warning("SKIPPING EVALUATION ON TEST SET!", immediate. = TRUE)
  }
}

# =============================================================================
cat("EXPORTING TEST RESULTS...\n")

stempel <- format(Sys.time(), format="%Y-%m-%d+%H%M%S")
outFile <- file.path(KONFIG$outputDir, paste0("test_results_tbl_", stempel, ".csv"))
write_csv(select(DATAFILES, gold.voice.file, room, test.N, error.opt, error.def), path = outFile)

cat("ALL DONE. BYE!\n")
