# optFormants.R
# =============================================================================
# Daniel Duran
# Albert-Ludwigs-Universität Freiburg, Germany
# daniel.duran@germanistik.uni-freiburg.de
# http://simphon.net/
#
# =============================================================================
#                                                                INITIALIZATION
# =============================================================================

library('foreach')
library('parallel')
library('tidyverse')
library('textgRid')

KONFIG <- list(isWindows      = str_detect(Sys.getenv('OS'), pattern = "(W|w)indows") | str_detect(Sys.getenv('SystemRoot'), pattern = "(W|w)indows"),
               use.cores    = ifelse(detectCores()>1, detectCores()-1, 1),
               targetLabels = c('i:', 'y:', 'a:', 'u:'), # NOTE: EXPECTING FOUR LABELS FOR CORNER VOWELS /i/, /y/, /a/ AND /u/ IN THAT ORDER!
               targetTier   = "PHO",
               chunkTier    = "SEG",
               tgSuffix     = ".TextGrid",
               praat        = list(exe               = NULL,
                                   script            = NULL,
                                   chunk.margin      = 0.0,
                                   max.formants      = 5,
                                   window.length     = 0.025,
                                   pre.emph          = 50,
                                   extraction.points = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
               ),
               praatColumns = list(formants   = c("F1","F2","F3"),  # NOTE: EXPECTING AT LEAST THREE FORMANTS F1, F2 AND F3, IN THAT ORDER
                                   bandwidths = c("b1","b2","b3")), # THIS MUST BE CONSISTENT WITH THE PRAAT OUTPUT FILES
               defaultHz    = list(m=5000, f=5500),
               searchGrid   = list(min=4500, max=6000, step=100),
               updateGrid   = list(margin=90, step=10),
               quantizeDigits = 2,
               maxUpdates     = 10,
               # ADJUST THESE PATHS:
               PRAAT.EXE      = '/usr/bin/praat',
               PRAAT.SCRIPT   = '/path/to/PP2020.git/Praat/extractFormantsChunked.praat',
               input.csv      = '/path/to/PP2020.git/formantFiles.csv',
               outputDir      = '/home/dd/Scratch/PP2020Vokale/'

)

# -----------------------------------------------------------------------------
#
# -----------------------------------------------------------------------------

if(KONFIG$isWindows){
  library('doSNOW')
} else {
  library('doMC')
  registerDoMC(KONFIG$use.cores)
}

KONFIG$praatDir <- file.path(KONFIG$outputDir, "Praat")

dir.create(KONFIG$praatDir, recursive = TRUE, showWarnings = FALSE)

KONFIG$rds <- list(inputFiles  = file.path(KONFIG$outputDir, "inputFiles.rds") )

for(it in 1:KONFIG$maxUpdates) {
  KONFIG$rds[[ gettextf("searchGrid%d", it) ]]    <- file.path(KONFIG$outputDir, gettextf("searchGrid%d.rds",it))
  KONFIG$rds[[ gettextf("formants%d", it) ]]      <- file.path(KONFIG$outputDir, gettextf("formants%d.rds",it))
  KONFIG$rds[[ gettextf("tokens%d", it) ]]        <- file.path(KONFIG$outputDir, gettextf("tokens%d.rds",it))
  KONFIG$rds[[ gettextf("trackFeatures%d", it) ]] <- file.path(KONFIG$outputDir, gettextf("trackFeatures%d.rds",it))
  KONFIG$rds[[ gettextf("spaceFeatures%d", it) ]] <- file.path(KONFIG$outputDir, gettextf("spaceFeatures%d.rds",it))
  KONFIG$rds[[ gettextf("lingFeatures%d", it) ]]  <- file.path(KONFIG$outputDir, gettextf("lingFeatures%d.rds",it))
}

# =============================================================================
#                                                              HELPER FUNCTIONS
# =============================================================================
# -----------------------------------------------------------------------------
#                               HELPER FUNCTION: IMPORT TEXTGRID FILE TO TIBBLE
# -----------------------------------------------------------------------------
importTextGridToTibble <- function(textGridFile, file.id, fix.encoding)
{
  if(is.null(textGridFile) || str_length(textGridFile)==0) {
    warning("No textGridFile specified: returning NULL")
    return(NULL)
  }
  tg <- tryCatch(
    TextGrid(textGridFile),
    error=function(cond) {
      warning(gettextf("[importTextGridToTibble( %s )]\n  %s", textGridFile, cond))
      return(NULL)
    },
    warning=function(cond) {
      warning(gettextf("[importTextGridToTibble( %s )]\n  %s", textGridFile, cond))
      return(NULL)
    },
    finally={}
  )
  if(is.null(tg)) {
    return(NULL)
  }
  # INITIALIZE TABLE WITH GLOBAL TIME AND INTERVAL NUMBER
  tg.tbl <- tibble(tmin=tg@startTime,
                   tmax=tg@endTime,
                   text=NA_character_,
                   tier=NA_character_,
                   interval=0)

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
      tmp.tbl$interval <- 1:N
      tg.tbl <- rbind(tg.tbl,tmp.tbl)
    }
  }
  if(fix.encoding) {
    tg.tbl$text <- iconv(iconv(tg.tbl$text, from="UTF-8", to = "Windows-1252"), to="UTF-8")
  }
  tg.tbl$id <- file.id
  return(tg.tbl)
}


# -----------------------------------------------------------------------------

exportToRds <- function(tbl, f) {
  if(file.exists(f)) {
    suffix <- format(file.mtime(f), format="%Y-%m-%d+%H%M%S")
    pLoc <- str_locate(f, "\\.")
    newFile <- str_c( str_sub(f, end=pLoc[1]-1), '_',  suffix,  str_sub(f, start=pLoc[1]) )
    file.copy(from=f, to=newFile, copy.date = TRUE )
  }
  saveRDS(object = tbl, file = f)
}


# -----------------------------------------------------------------------------

extractFormants <- function(textGridFile, soundFile, maxHertz, outputDir, konf)
{
  # CHECK INPUT PARAMETERS
  if(!file.exists(textGridFile)) {
    warning(gettextf("Input file not found: %s\nReturning FALSE", textGridFile))
    return(FALSE)
  }
  if(!file.exists(soundFile)) {
    warning(gettextf("Input file not found: %s\nReturning FALSE", soundFile))
    return(FALSE)
  }

  label_csv <- str_flatten(konf$targetLabels, collapse=",")

  praatScriptArgs <- gettextf("\"%s\", \"%s\", 1, \"%s\", \"%s\", \"%s\", %f, %d, %d, %f, %d, \"%s\", \"%s\", 0, 0, %d",
                              normalizePath(textGridFile),
                              normalizePath(soundFile),
                              label_csv,
                              konf$targetTier,
                              konf$chunkTier,
                              konf$praat$chunk.margin,
                              konf$praat$max.formants,
                              maxHertz,
                              konf$praat$window.length,
                              konf$praat$pre.emph,
                              str_flatten(konf$praat$extraction.points, collapse=","),
                              normalizePath(outputDir),
                              as.integer(FALSE) )

  tmp.praat.script <- tempfile(fileext = ".praat")

  tmp.praat.script.content <- c("# TEMPORARY PRAAT SCRIPT",
                                gettextf("runScript: \"%s\", %s", normalizePath(konf$PRAAT.SCRIPT), praatScriptArgs))

  dateiVerbindung <- file(tmp.praat.script, encoding = "UTF-8")
  writeLines(tmp.praat.script.content, dateiVerbindung)
  close(dateiVerbindung)
  if(!file.exists(tmp.praat.script)) {
    warning(gettextf("[praat_extractFormantsChunked] Could not create temp script %s\nReturning FALSE", tmp.praat.script))
    return(FALSE)
  }

  # CHANGE TO TEMPORARY DIRECTORY:
  currentWD <- getwd()
  setwd(tempdir())

  # RUN PRAAT SCRIPT:
  if(konf$isWindows) {
    shell(gettextf("\"%s\" --run %s", normalizePath(konf$PRAAT.EXE), basename(tmp.praat.script)))
  } else {
    system(gettextf("\"%s\" --run %s", normalizePath(konf$PRAAT.EXE), basename(tmp.praat.script)))
  }
  #TODO CHECK IF OUTPUT FILE WAS GENERATED?

  file.remove(tmp.praat.script)

  setwd(currentWD)
  return(TRUE)
}

# -----------------------------------------------------------------------------
#
executeGridSearch <- function(inputFiles, searchGrid, konf, verbose = TRUE)
{
  allOK <- TRUE
  # INITIALIZE OUTPUT DIRECTORIES:
  allMaxHz <- unique(unlist(searchGrid, use.names = FALSE, recursive = TRUE))
  for(xHz in allMaxHz){
    ausgabe <- file.path(konf$praatDir, as.character(xHz))
    if(!dir.exists(ausgabe)){
      dir.create(ausgabe, recursive=TRUE)
    }
  }#ENDFOR xHz

  for(xr in 1:nrow(inputFiles))
  {
    if(verbose) cat(gettextf("+ %s: %s ", inputFiles[xr,]$speaker, inputFiles[xr,]$base))
    allMaxHz <- searchGrid[[inputFiles[xr,]$speaker]]
    for(xHz in allMaxHz)
    {
      ausgabe <- file.path(konf$praatDir, as.character(xHz))
      # CHECK IF FILE EXISTS:
      ausgabeCSV <- file.path(ausgabe, gettextf("%s_Formants.csv", inputFiles[xr,]$base))
      if(! file.exists(ausgabeCSV) )
      {
        ok <- extractFormants(textGridFile = inputFiles[xr,]$textGrid, soundFile = inputFiles[xr,]$audio, konf = konf, maxHertz = xHz, outputDir = ausgabe)
        if(ok && file.exists(ausgabeCSV)) {
          if(verbose) cat(".")
        } else {
          warning(gettextf("Praat failed on %s with %d Hz", inputFiles[xr,]$base, xHz))
          allOK <- FALSE
        }
      }
    }
    if(verbose) cat(" done.\n")
  }
  return(allOK)
}

# -----------------------------------------------------------------------------
#                                               HELPER FUNCTION: FORMANT IMPORT
# -----------------------------------------------------------------------------

importFormants <- function(inputFiles, searchGrid, konf, verbose = TRUE, NA.string = "--undefined--")
{
  formants <- tibble()
  for(xr in 1:nrow(inputFiles))
  {
    if(verbose) cat(gettextf("+ %s: %s --", inputFiles[xr,]$speaker, inputFiles[xr,]$base))
    z <- 0
    allMaxHz <- searchGrid[[inputFiles[xr,]$speaker]]
    for(xHz in allMaxHz)
    {
      praatOutput <- file.path(konf$praatDir, as.character(xHz), gettextf("%s_Formants.csv", inputFiles[xr,]$base))
      if(!file.exists(praatOutput)) {
        warning(gettextf("Expected Praat output not found: %s", praatOutput))
        next()
      }
      suppressMessages( tmp <- read_csv(praatOutput, na = NA.string) %>%
                          mutate(speaker=inputFiles[xr,]$speaker, maxHz=xHz, fileID=xr, segID = str_c(xr, xHz, phoSeg, sep = "+")) )
      if(nrow(tmp)==0) {
        warning(gettextf("No formant data in: %s", praatOutput))
        next()
      }
      formants <- rbind(formants, tmp)
      z <- z +1
    }
    if(verbose) cat(gettextf(" Found %d Praat output files.\n", z))
  }
  return(formants)
}


# -----------------------------------------------------------------------------
#                                                       HELPER FUNCTION: TOKENS
# -----------------------------------------------------------------------------
#
get_tokens <- function(formants, konf, verbose=TRUE)
{
  allSegmentIDs <- unique(formants$segID)
  num_Segments  <- length(allSegmentIDs)
  num_points    <- length(konf$praat$extraction.points)

  # >>>> PARALLEL >>>> >>>> >>>>
  tokens.tbl <- foreach(xs = 1:num_Segments, .combine = rbind, .packages=c("tibble")) %dopar% {# xs=1

    if(verbose){
      if(xs > 0 && xs %% 1000 == 0) {
        cat(gettextf(".  [%05.2f%%]\n", (xs/num_Segments)*100.0 ))
      } else if(xs %% 50 == 0) {
        cat(".")
      }
    }

    seg.tbl <- formants[ formants$segID == allSegmentIDs[xs], ]

    if(nrow(seg.tbl)!=num_points) {
      warning(gettextf("Segment %s [%s] has %d rows (expected: %d)", allSegmentIDs[xs], seg.tbl[1,]$label, nrow(seg.tbl), num_points), immediate. = TRUE)
    }

    m <- apply(seg.tbl[,konf$praatColumns$formants], 2, FUN = median, na.rm = TRUE)

    out.tbl <- tibble(speaker = seg.tbl[1,]$speaker,
                      fileID  = seg.tbl[1,]$fileID,
                      segID   = seg.tbl[1,]$segID,
                      maxHz   = seg.tbl[1,]$maxHz,
                      F1      = m[1],
                      F2      = m[2],
                      F3      = m[3],
                      label   = seg.tbl[1,]$label)

    out.tbl

  }#END FOREACH
  # <<<< PARALLEL <<<< <<<< <<<<

  # }#ENDFOR xs
  if(verbose) cat("\n")

  return(tokens.tbl)
}


# -----------------------------------------------------------------------------
#                                                  HELPER FUNCTION: LABEL CHECK
# -----------------------------------------------------------------------------

check_formant_labels <- function(formants, konf, iteration)
{
  x <- which(! formants$label %in% konf$targetLabels)
  if(length(x)>0) {
    outFile <- file.path(konf$outputDir, gettextf("error_non-target_segments_%d.csv", iteration))
    out.tbl <- formants[x,]
    out.tbl$base <-inputFiles.tbl[ out.tbl$fileID, ]$base
    if(konf$isWindows) {
      write_excel_csv2(out.tbl, outFile)
    } else {
      write_csv(out.tbl, outFile)
    }
    stop("There are non-target labels in the formants table!")
  }
}





# -----------------------------------------------------------------------------
#                                               HELPER FUNCTION: TRACK FEATURES
# -----------------------------------------------------------------------------
#
compute_track_features <- function(formants, konf, verbose = TRUE, min.track.length=5)
{
  # INITIALIZE PARALLEL PROCESSING:
  if(konf$isWindows){# using "doSNOW"
    cl<-makeCluster(konf$use.cores)
    registerDoSNOW(cl)
  }# on linux we use "doMC", registered globally -> no need to make a cluster

  allSegIDs <- sort(unique(formants$segID))

  num_Seg <- length(allSegIDs)
  num_Formants <- length(konf$praatColumns$formants)
  num_Points   <- length(konf$praat$extraction.points)

  # >>>> PARALLEL >>>> >>>> >>>>
  feature.tbl <- foreach(xs = 1:num_Seg, .combine = rbind, .packages=c("tibble")) %dopar% {# xs=1

    if(verbose){
      if(xs > 0 && xs %% 2000 == 0) {
        cat(gettextf(".  [%05.2f%%]\n", (xs/num_Seg)*100.0 ))
      } else if(xs %% 200 == 0) {
        cat(".")
      }
    }

    segmentID <- allSegIDs[xs]

    tmp.tbl <- formants[formants$segID == segmentID, ]

    tmp.out <- tibble(fileID = integer(num_Formants),
                      segID     = character(num_Formants),
                      formant   = integer(num_Formants),
                      f_range   = double(num_Formants), # distance between min and max value of the formant track -> should be small
                      f_delta   = double(num_Formants), # sum of sample deltas                                    -> should be small (we optimize on monophthongs!)
                      f_turns   = double(num_Formants), # number of direction turns in formant track              -> should be small (we optimize on monophthongs!)
                      b_range   = double(num_Formants), # distance between min and max value of the bandwidth     -> should be small
                      #bandwidth = double(num_Formants), # mean formant bandwidth within a track                   -> should be small
                      na.count  = integer(num_Formants) # the number of NA values in the formant track            -> should be small
    )

    for(xf in 1:num_Formants){

      fmt <- konf$praatColumns$formants[xf]
      f_track <- unlist(tmp.tbl[,fmt], use.names = FALSE)
      f_track <- f_track[is.finite(f_track)]

      tmp.out[xf,]$fileID    <- tmp.tbl[1,]$fileID
      tmp.out[xf,]$segID     <- segmentID
      tmp.out[xf,]$formant   <- xf

      if(length(f_track) < min.track.length){
        tmp.out[xf,]$f_range   <- NA
        tmp.out[xf,]$f_delta   <- NA
        tmp.out[xf,]$f_turns   <- NA
        tmp.out[xf,]$b_range   <- NA
        #tmp.out[xf,]$bandwidth <- NA
        tmp.out[xf,]$na.count  <- NA
      } else {
        bwd <- konf$praatColumns$bandwidths[xf]
        b_track <- unlist(tmp.tbl[,bwd], use.names = FALSE)
        b_track <- b_track[is.finite(f_track)]
        f_delta <- diff( f_track )
        formant_range <- max(f_track) - min(f_track)
        bandw_range   <- max(b_track) - min(b_track)
        delta         <- sum(abs(f_delta))
        tmp.out[xf,]$f_range   <- formant_range * formant_range # SQUARED
        tmp.out[xf,]$f_delta   <- delta * delta # SQUARED
        tmp.out[xf,]$f_turns   <- sum(diff(sign(f_delta))!=0)
        tmp.out[xf,]$b_range   <- bandw_range * bandw_range # SQUARED
        #tmp.out[xf,]$bandwidth <- mean(b_track)
        tmp.out[xf,]$na.count  <- num_Points - length(f_track)
      }
    }#ENDFOR xf

    tmp.out

  }#END FOREACH
  # <<<< PARALLEL <<<< <<<< <<<<

  if(konf$isWindows){
    stopCluster(cl)
  }
  if(verbose) {
    cat("\n")
  }
  return(feature.tbl)
}




# -----------------------------------------------------------------------------
#                                                 HELPER FUNCTION: TRACK SCORES
# -----------------------------------------------------------------------------
#
score_track_features <- function(features_list, inputFiles, konf, iteration, verbose=TRUE)
{
  # INITIALIZE PARALLEL PROCESSING:
  if(konf$isWindows){# using "doSNOW"
    cl<-makeCluster(konf$use.cores)
    registerDoSNOW(cl)
  }# on linux we use "doMC"

  trackLength <- length(konf$praat$extraction.points)
  allFormants <- 1:length(konf$praatColumns$formants)
  allSpeakers <- sort(unique(inputFiles$speaker))
  score.tbl   <- tibble()

  features <- tibble()
  for(it in 1:iteration) {
    features <- rbind(features, features_list[[it]])
  }

  for(spk in allSpeakers){

    if(verbose) cat(gettextf(" scoring: %s\n", spk))

    spkFiles     <- which(inputFiles$speaker==spk)
    num_spkFiles <- length(spkFiles)

    ergebnis <- foreach(xid = 1:num_spkFiles, .combine = rbind, .packages=c("tibble")) %dopar% {# xid=1

      tmp.tbl <- features[features$fileID == spkFiles[xid],]

      M <- nrow(tmp.tbl)

      tmp.out <- tibble(segID       = character(M),
                        formant     = integer(M),
                        f_range_score = double(M),
                        f_delta_score = double(M),
                        f_turns_score = double(M),
                        b_range_score = double(M),
                        #bandw_score   = double(M),
                        na_score      = double(M)
      )

      for(xf in 1:length(allFormants)){# xf=1
        flags <- tmp.tbl$formant == allFormants[xf]
        max_fRange <- max( tmp.tbl[flags,]$f_range, na.rm = TRUE)
        max_fDelta <- max( tmp.tbl[flags,]$f_delta, na.rm = TRUE)
        max_bRange <- max( tmp.tbl[flags,]$b_range, na.rm = TRUE)
        maxBandw   <- max( tmp.tbl[flags,]$bandwidth, na.rm = TRUE)

        x <- which(!is.finite(tmp.tbl$f_range))
        if(length(x)>0){
          tmp.tbl[ x, ]$f_range <- max_fRange
        }
        x <- which(!is.finite(tmp.tbl$f_delta))
        if(length(x)>0){
          tmp.tbl[ x, ]$f_delta <- max_fDelta
        }
        x <- which(!is.finite(tmp.tbl$f_turns))
        if(length(x)>0){
          tmp.tbl[ x, ]$f_turns <- trackLength
        }
        x <- which(!is.finite(tmp.tbl$b_range))
        if(length(x)>0){
          tmp.tbl[ x, ]$b_range <- max_bRange
        }
        x <- which(!is.finite(tmp.tbl$bandwidth))
        if(length(x)>0){
          tmp.tbl[ x, ]$bandwidth <- maxBandw
        }
        x <- which(!is.finite(tmp.tbl$na.count))
        if(length(x)>0){
          tmp.tbl[ x, ]$na.count <- trackLength
        }

        tmp.out[flags,]$segID         <- tmp.tbl[flags,]$segID
        tmp.out[flags,]$formant       <- allFormants[xf]
        tmp.out[flags,]$f_range_score <- 1.0 - (tmp.tbl[flags,]$f_range / max_fRange)
        tmp.out[flags,]$f_delta_score <- 1.0 - (tmp.tbl[flags,]$f_delta / max_fDelta)
        tmp.out[flags,]$f_turns_score <- 1.0 - (tmp.tbl[flags,]$f_turns / trackLength)
        tmp.out[flags,]$b_range_score <- 1.0 - (tmp.tbl[flags,]$b_range / max_bRange)
        #tmp.out[flags,]$bandw_score   <- 1.0 - (tmp.tbl[flags,]$bandwidth / maxBandw)
        tmp.out[flags,]$na_score      <- 1.0 - (tmp.tbl[flags,]$na.count / trackLength)
      }

      tmp.out

    }#END FOREACH

    if(nrow(ergebnis)==0) {
      stop(gettextf("No track scores for %s?", spk))
    }
    score.tbl <- rbind(score.tbl, ergebnis)
  }

  if(konf$isWindows){
    stopCluster(cl)
  }
  return(score.tbl)
}


# -----------------------------------------------------------------------------
#                                               HELPER FUNCTION: TRACK FEATURES
# -----------------------------------------------------------------------------
#
compute_space_features <- function(formants, inputFiles, konf, min.token.num = 5, verbose = TRUE)
{
  # INITIALIZE PARALLEL PROCESSING:
  if(konf$isWindows){# using "doSNOW"
    cl<-makeCluster(konf$use.cores)
    registerDoSNOW(cl)
  }

  num_Files    <- nrow(inputFiles)
  num_Labels   <- length(konf$targetLabels)
  num_Formants <- length(konf$praatColumns$formants)

  q1 <- 1/4
  q3 <- 3/4
  qq <- c(q1, q3)

  # >>>> PARALLEL >>>> >>>> >>>>
  feature.tbl <- foreach(xFileNum = 1:num_Files, .combine = rbind, .packages=c("tibble")) %dopar% {# xFileNum=1

    if(verbose){
      if(xFileNum > 0 && xFileNum %% 1000 == 0) {
        cat(gettextf(".  [%05.2f%%]\n", (xFileNum/num_Files)*100.0 ))
      } else if(xFileNum %% 10 == 0) {
        cat(".")
      }
    }

    tmp.tbl <- formants[ formants$fileID == xFileNum,  ]

    if(nrow(tmp.tbl)==0) {
      K <- 0
    } else {
      spk       <- tmp.tbl[1,]$speaker
      allMaxHz  <- sort(unique(tmp.tbl$maxHz))
      num_maxHz <- length(allMaxHz)
      rm(tmp.tbl)
      K <- num_Labels * num_Formants * num_maxHz
    }

    tmp.out <- tibble(speaker  = character(K),
                      fileID   = character(K),
                      maxHz    = double(K),
                      label    = character(K),
                      n_total  = integer(K), # number of tokens with maxHz
                      n        = integer(K), # number of tokens with this maxHz and label
                      formant  = integer(K), # index i of formant F_i
                      range    = double(K),  # range of Hz values                                 -> should be small
                      sd       = double(K),  # standard deviation of Hz values                    -> should be small
                      m_margin = integer(K), # number of overlapping tokens within max-min margin -> should be small
                      q_margin = integer(K), # squared number of outliers                         -> should be small
                      dispers  = double(K)   # distance from the vowel space center               -> should be large
    )

    if(K>0){
      xk <- 1

      for(xh in 1:num_maxHz) {# xh=1

        fileFormants.tbl <- formants[ formants$fileID == xFileNum & formants$maxHz == allMaxHz[xh],  ]

        zentrum <- NULL # THE CENTER OF THE VOWEL SPACE (ONLY IF ALL TARGET LABELS ARE PRESENT)
        if(length(unique(fileFormants.tbl$label))==num_Labels) {
          zentrum <- colMeans(fileFormants.tbl[,konf$praatColumns$formants], na.rm = TRUE)
        }

        for(xl in 1:num_Labels){

          lab_flags   <- fileFormants.tbl$label == konf$targetLabels[xl]
          lab_indices <- which(lab_flags)

          for(xf in 1:num_Formants){

            tmp.out[xk,]$speaker <- spk
            tmp.out[xk,]$fileID  <- xFileNum
            tmp.out[xk,]$maxHz   <- allMaxHz[xh]
            tmp.out[xk,]$label   <- konf$targetLabels[xl]
            tmp.out[xk,]$n_total <- nrow(fileFormants.tbl)
            tmp.out[xk,]$n       <- length(lab_indices)
            tmp.out[xk,]$formant <- xf

            if(length(lab_indices) < min.token.num) {
              tmp.out[xk,]$range    <- NA_real_
              tmp.out[xk,]$sd       <- NA_real_
              tmp.out[xk,]$m_margin <- NA_integer_
              tmp.out[xk,]$q_margin <- NA_integer_
              tmp.out[xk,]$dispers  <- NA_real_

            } else {
              fCol      <- konf$praatColumns$formants[xf]
              spalte    <- fileFormants.tbl[lab_indices,fCol]

              five <- fivenum(unlist(spalte), na.rm = TRUE)
              min_diese <- five[1]
              max_diese <- five[5]

              tmp.out[xk,]$range <- (max_diese - min_diese) * (max_diese - min_diese) # SQUARED

              tmp.out[xk,]$sd <- apply(spalte, 2, FUN = sd, na.rm=TRUE)

              margin <- nrow( fileFormants.tbl[ fileFormants.tbl[,fCol] >= min_diese &
                                                  fileFormants.tbl[,fCol] <= max_diese &
                                                  !lab_flags, ] ) # alle Token in range die _nicht_ zur aktuellen Kategorie gehören!
              tmp.out[xk,]$m_margin <- margin * margin # SQUARED


              margin <- nrow( fileFormants.tbl[ (fileFormants.tbl[,fCol] <= five[2] | fileFormants.tbl[,fCol] >= five[4]) & lab_flags, ] )
              tmp.out[xk,]$q_margin <- margin * margin # SQUARED

              rm(margin)

              if(is.null(zentrum)){
                tmp.out[xk,]$dispers <- NA_real_
              } else {
                dispersion <- abs(colMeans(spalte, na.rm = TRUE) - zentrum[xf])
                tmp.out[xk,]$dispers <- dispersion * dispersion # SQUARED
                # alternatively we could compute the euclidean distance in F1xF2xF3 space
                # to the center of the vowel space - but this would be more expensive
              }
            }
            xk <- xk + 1
          }#ENDFOR xf
        }#ENDFOR xl
      }#ENDFOR xh
    }#ENDIF k>0

    tmp.out
  }

  if(konf$isWindows){
    stopCluster(cl)
  }
  return(feature.tbl)
}

# -----------------------------------------------------------------------------
#                                               HELPER FUNCTION: TRACK FEATURES
# -----------------------------------------------------------------------------
#
score_space_featurs <- function(features_list, inputFiles, konf, iteration, verbose=TRUE)
{

  # INITIALIZE PARALLEL PROCESSING:
  if(konf$isWindows){# using "doSNOW"
    cl<-makeCluster(konf$use.cores)
    registerDoSNOW(cl)
  }# on linux we use "doMC"

  features <- tibble()
  for(it in 1:iteration) {
    features <- rbind(features, features_list[[it]])
  }

  # COMPUTE SCORE FOR EACH FILE+LABEL+FORMANT

  num_Files    <- nrow(inputFiles)
  num_Formants <- length(konf$praatColumns$formants)

  score.tbl <- foreach(xFile = 1:num_Files, .combine = rbind, .packages=c("tibble")) %dopar% {# xFile=1

    fileFlags  <- features$fileID == xFile
    tmp.tbl    <- features[fileFlags, ]
    all_maxHz  <- unique(tmp.tbl$maxHz)
    rm(tmp.tbl)

    out.tbl <- tibble()

    for(xf in 1:num_Formants){# xf=1

      tmp.tbl <- features[fileFlags & features$formant==xf, ]
      suppressWarnings( maxRange <- max(tmp.tbl$range, na.rm = TRUE) )
      suppressWarnings( maxSD    <- max(tmp.tbl$sd, na.rm = TRUE) )
      suppressWarnings( maxDisp  <- max(tmp.tbl$dispers, na.rm = TRUE) )
      rm(tmp.tbl)

      for(xl in konf$targetLabels) {

        tmp.tbl <- features[fileFlags & features$formant==xf & features$label==xl, ]
        tmp.out <- tibble(speaker=tmp.tbl$speaker, fileID=tmp.tbl$fileID, maxHz=tmp.tbl$maxHz, label=tmp.tbl$label, formant=tmp.tbl$formant,
                          range_score=0.0, sd_score=0.0, m_margin_score=0.0, q_margin_score=0.0, dispers_score=0.0 )

        if(is.finite(maxRange)) {
          index <- is.finite(tmp.tbl$range)#2020-07-10 hinzugefügt
          tmp.out[index,]$range_score <- 1.0 - (tmp.tbl[index,]$range / maxRange) # range should be small
        }
        if(is.finite(maxSD)) {
          index <- is.finite(tmp.tbl$sd)#2020-07-10 hinzugefügt
          tmp.out[index,]$sd_score <- 1.0 - (tmp.tbl[index,]$sd / maxSD) # standard deviation should be small
        }
        if(is.finite(maxDisp)) {
          index <- is.finite(tmp.tbl$dispers)#2020-07-10 hinzugefügt
          tmp.out[index,]$dispers_score <- (tmp.tbl[index,]$dispers / maxDisp) # dispersion should be large
        }
        index <- is.finite(tmp.tbl$m_margin)#2020-07-10 hinzugefügt
        tmp.out[index,]$m_margin_score <- 1.0 - (tmp.tbl[index,]$m_margin / tmp.tbl[index,]$n_total) # margin overlap should be small
        index <- is.finite(tmp.tbl$q_margin)#2020-07-10 hinzugefügt
        tmp.out[index,]$q_margin_score <- 1.0 - (tmp.tbl[index,]$q_margin / tmp.tbl[index,]$n_total) # margin overlap should be small

        out.tbl <- rbind(out.tbl, tmp.out)
      }
    }

    if(nrow(out.tbl)==0) {
      stop(gettextf("No space scores for file %d: %s", xFile, inputFiles[xFile,]$base))
    }

    out.tbl
  }#ENDFOREACH

  if(konf$isWindows){
    stopCluster(cl)
  }

  x <- which(!is.finite(score.tbl$range_score))
  if(length(x)>0) {
    score.tbl[x,]$range_score <- 0.0
  }
  x <- which(!is.finite(score.tbl$sd_score))
  if(length(x)>0) {
    score.tbl[x,]$sd_score <- 0.0
  }
  x <- which(!is.finite(score.tbl$m_margin_score))
  if(length(x)>0) {
    score.tbl[x,]$m_margin_score <- 0.0
  }
  x <- which(!is.finite(score.tbl$q_margin_score))
  if(length(x)>0) {
    score.tbl[x,]$q_margin_score <- 0.0
  }
  x <- which(!is.finite(score.tbl$dispers_score))
  if(length(x)>0) {
    score.tbl[x,]$dispers_score <- 0.0
  }

  return(score.tbl)
}


# -----------------------------------------------------------------------------
#                                          HELPER FUNCTION: LINGUISTIC FEATURES
# -----------------------------------------------------------------------------
#
compute_ling_features <- function(formants, inputFiles, konf, verbose=TRUE)
{
  # FOR EACH FILE AND MAXHZ SETTING

  # INITIALIZE PARALLEL PROCESSING:
  if(konf$isWindows){# using "doSNOW"
    cl<-makeCluster(konf$use.cores)
    registerDoSNOW(cl)
  }

  colF1 <- konf$praatColumns$formants[1]
  colF2 <- konf$praatColumns$formants[2]
  colF3 <- konf$praatColumns$formants[3]

  q1 <- 1/4
  q3 <- 3/4

  num_Files <- nrow(inputFiles)

  features <- foreach(xFile = 1:num_Files, .combine = rbind, .packages=c("tibble")) %dopar% {# xFile=1

    fileFlags <- formants$fileID == xFile
    tmp.tbl   <- formants[fileFlags,]
    all_maxHz <- unique(tmp.tbl$maxHz)
    rm(tmp.tbl)

    spk <- inputFiles[xFile,]$speaker

    tmp.out <- tibble()

    for(xh in all_maxHz){# xh=all_maxHz[1]

      tmp.tbl <- formants[fileFlags & formants$maxHz==xh,]

      # NOTE: WE DO NOT GROUP MULTIPLE MEASURES WITHIN ONE TOKEN TO A SINGLE VALUE
      # IN ORDER TO SPEED UP COMPUTATIONS

      flags_i <- tmp.tbl$label == konf$targetLabels[1]
      flags_y <- tmp.tbl$label == konf$targetLabels[2]
      flags_a <- tmp.tbl$label == konf$targetLabels[3]
      flags_u <- tmp.tbl$label == konf$targetLabels[4]

      flags_not_i <- flags_y | flags_a | flags_u

      q1_a_F1 <- quantile(unlist(tmp.tbl[flags_a,colF1], use.names = FALSE), q1, na.rm = TRUE)

      ft <- rep(NA_real_, 7)

      # [i]_F2 SHOULD BE GREATER THAN ALL OTHER F2 VALUES
      # WE COMPUTE THE DIFFERENCE BETWEEN THE MEAN OF [i:] AND Q3 OF ALL OTHER TOKENS
      # THIS VALUE SHOULD BE LARGE (POSITIVE)
      others <- unlist(tmp.tbl[flags_not_i,colF2], use.names = FALSE)
      ft[1] <- colMeans(tmp.tbl[flags_i,colF2], na.rm = TRUE) - quantile(others, q3, na.rm = TRUE)

      # [i]_F1 SHOULD BE SMALLER THAN [a]_F1
      # WE COMPUTE THE DIFFERENCE BETWEEN Q1 OF [a:] AND THE MEAN OF [i:]
      # THIS VALUE SHOULD BE LARGE (POSITIVE)
      ft[2] <- q1_a_F1 - colMeans(tmp.tbl[flags_i,colF1], na.rm = TRUE)

      # [i]_F3 SHOULD BE LARGER THAN ALL OTHER F3 VALUES
      # THIS VALUE SHOULD BE LARGE (POSITIVE)
      others <- unlist(tmp.tbl[flags_not_i,colF3], use.names = FALSE)
      ft[3] <- colMeans(tmp.tbl[flags_i,colF3], na.rm = TRUE) - quantile(others, q3, na.rm = TRUE)

      # [y]_F2 SHOULD BE GREATER THAN [u]_F2
      # WE COMPUTE THE DIFFERENCE BETWEEN THE MEAN OF [y:] AND Q3 OF [u:]
      # THIS VALUE SHOULD BE LARGE (POSITIVE)
      ft[4] <- colMeans(tmp.tbl[flags_y,colF2], na.rm = TRUE) - quantile(unlist(tmp.tbl[flags_u,colF2]), q3, na.rm = TRUE)

      # [y]_F1 SHOULD BE SMALLER THAN [a]_F1
      # THIS VALUE SHOULD BE LARGE (POSITIVE)
      ft[5] <- q1_a_F1 - colMeans(tmp.tbl[flags_y,colF1], na.rm = TRUE)

      # [u]_F2 SHOULD BE SMALLER THAN ALL OTHER F2 VALUES
      # THIS VALUE SHOULD BE LARGE (POSITIVE)
      others <- unlist(tmp.tbl[flags_a|flags_i|flags_y,colF2], use.names = FALSE)
      ft[6] <- quantile(others, q1, na.rm = TRUE) - colMeans(tmp.tbl[flags_u,colF2], na.rm = TRUE)

      # [u]_F1 SHOULD BE SMALLER THAN [a]_F1
      # THIS VALUE SHOULD BE LARGE (POSITIVE)
      ft[7] <- q1_a_F1 - colMeans(tmp.tbl[flags_u,colF1], na.rm = TRUE)

      tmp.out <- rbind(tmp.out,
                       tibble( speaker    = spk,
                               fileID     = xFile,
                               maxHz      = xh,
                               margin_i_2 = ft[1],
                               margin_i_1 = ft[2],
                               margin_i_3 = ft[3],
                               margin_y_2 = ft[4],
                               margin_y_1 = ft[5],
                               margin_u_2 = ft[6],
                               margin_u_1 = ft[7] ) )
    }#ENDFOR xh

    tmp.out

  }#ENDFOREACH

  if(konf$isWindows){
    stopCluster(cl)
  }
  return(features)
}


# -----------------------------------------------------------------------------
#                                            HELPER FUNCTION: LINGUISTIC SCORES
# -----------------------------------------------------------------------------
#
score_ling_features <- function(features_list, inputFiles, konf, iteration, verbose=TRUE)
{
  # INITIALIZE PARALLEL PROCESSING:
  if(konf$isWindows){# using "doSNOW"
    cl<-makeCluster(konf$use.cores)
    registerDoSNOW(cl)
  }

  features <- tibble()
  for(it in 1:iteration) {
    features <- rbind(features, features_list[[it]])
  }

  num_Files <- nrow(inputFiles)

  score.tbl <- foreach(xFile = 1:num_Files, .combine = rbind, .packages=c("tibble")) %dopar% {# xFile=1

    fileFeatures.tbl <- features[features$fileID == xFile,]

    suppressWarnings({
      max_i_1 <- max(fileFeatures.tbl$margin_i_1, na.rm = TRUE)
      max_i_2 <- max(fileFeatures.tbl$margin_i_2, na.rm = TRUE)
      max_i_3 <- max(fileFeatures.tbl$margin_i_3, na.rm = TRUE)
      max_y_1 <- max(fileFeatures.tbl$margin_y_1, na.rm = TRUE)
      max_y_2 <- max(fileFeatures.tbl$margin_y_2, na.rm = TRUE)
      max_u_1 <- max(fileFeatures.tbl$margin_u_1, na.rm = TRUE)
      max_u_2 <- max(fileFeatures.tbl$margin_u_2, na.rm = TRUE)
    })

    tmp.out <- tibble(speaker=fileFeatures.tbl$speaker, fileID=fileFeatures.tbl$fileID, maxHz=fileFeatures.tbl$maxHz,
                      score_i_1=0.0, score_i_2=0.0, score_i_3=0.0, score_y_1=0.0, score_y_2=0.0, score_u_1=0.0, score_u_2=0.0 )

    tmp.out$score_i_1 <- fileFeatures.tbl$margin_i_1 / max_i_1
    tmp.out$score_i_2 <- fileFeatures.tbl$margin_i_2 / max_i_2
    tmp.out$score_i_3 <- fileFeatures.tbl$margin_i_3 / max_i_3
    tmp.out$score_y_1 <- fileFeatures.tbl$margin_y_1 / max_y_1
    tmp.out$score_y_2 <- fileFeatures.tbl$margin_y_2 / max_y_2
    tmp.out$score_u_1 <- fileFeatures.tbl$margin_u_1 / max_u_1
    tmp.out$score_u_2 <- fileFeatures.tbl$margin_u_2 / max_u_2

    tmp.out

  }#ENDFOREACH

  if(konf$isWindows){
    stopCluster(cl)
  }

  x <- which(!is.finite(score.tbl$score_i_1) | score.tbl$score_i_1 < 0)
  if(length(x)>0) {
    score.tbl[x,]$score_i_1 <- 0.0
  }
  x <- which(!is.finite(score.tbl$score_i_2) | score.tbl$score_i_2 < 0)
  if(length(x)>0) {
    score.tbl[x,]$score_i_2 <- 0.0
  }
  x <- which(!is.finite(score.tbl$score_i_3) | score.tbl$score_i_3 < 0)
  if(length(x)>0) {
    score.tbl[x,]$score_i_3 <- 0.0
  }
  x <- which(!is.finite(score.tbl$score_y_1) | score.tbl$score_y_1 < 0)
  if(length(x)>0) {
    score.tbl[x,]$score_y_1 <- 0.0
  }
  x <- which(!is.finite(score.tbl$score_y_2) | score.tbl$score_y_2 < 0)
  if(length(x)>0) {
    score.tbl[x,]$score_y_2 <- 0.0
  }
  x <- which(!is.finite(score.tbl$score_u_1) | score.tbl$score_u_1 < 0)
  if(length(x)>0) {
    score.tbl[x,]$score_u_1 <- 0.0
  }
  x <- which(!is.finite(score.tbl$score_u_2) | score.tbl$score_u_2 < 0)
  if(length(x)>0) {
    score.tbl[x,]$score_u_2 <- 0.0
  }

  return(score.tbl)
}


# -----------------------------------------------------------------------------
#                                               HELPER FUNCTION: GLOBAL OPTIMUM
# -----------------------------------------------------------------------------
#
compute_optimum <- function(track_scores_list, space_scores_list, ling_scores_list, formants_list, inputFiles, konf, iteration, verbose=TRUE)
{
  formants <- tibble()
  for(it in 1:iteration) {
    formants <- rbind(formants, formants_list[[it]])
  }

  all_speakers <- sort(unique(formants$speaker))

  track_scores <- track_scores_list[[iteration]]
  space_scores <- space_scores_list[[iteration]]
  ling_scores  <- ling_scores_list[[iteration]]

  opt.tbl <- tibble(speaker=character(), maxHz=double(),
                    track_score=double(), space_score=double(), ling_score=double(), score=double(),
                    is_optimal=logical(), is_default=logical(), is_worst=logical())

  colTrack <- c( "f_range_score", "f_delta_score", "f_turns_score", "b_range_score", "na_score") # "bandw_score"
  colSpace <- c("range_score", "sd_score", "m_margin_score", "q_margin_score", "dispers_score")
  colLing  <- c("score_i_1", "score_i_2", "score_i_3", "score_y_1", "score_y_2", "score_u_1", "score_u_2")

  for(spk in all_speakers){# spk=all_speakers[1]

    if(verbose) cat(gettextf("+ %s ", spk))

    spkSex     <- unique(inputFiles[inputFiles$speaker==spk,]$sex)
    spkFlags   <- formants$speaker == spk
    all_maxHz  <- sort(unique(formants[ spkFlags, ]$maxHz))

    for(xHz in all_maxHz){# xHz=all_maxHz[1]
      if(verbose) cat(".")

      segmentIDs <- unique(formants[ spkFlags & formants$maxHz==xHz, ]$segID)

      hz_track <- track_scores[ track_scores$segID %in% segmentIDs, ]
      hz_space <- space_scores[ space_scores$maxHz==xHz & space_scores$speaker==spk, ]
      hz_ling  <- ling_scores[ ling_scores$maxHz==xHz & ling_scores$speaker==spk, ]

      s_t <- median(colMeans(hz_track[,colTrack]))# 2020-07-10 using median instead of mean
      s_s <- median(colMeans(hz_space[,colSpace]))
      s_l <- median(colMeans(hz_ling[,colLing]))

      opt.tbl <- add_row(opt.tbl,
                         speaker     = spk,
                         maxHz       = xHz,
                         track_score = s_t,
                         space_score = s_s,
                         ling_score  = s_l,
                         score       = (s_t + s_s + s_l),
                         is_optimal  = FALSE,
                         is_default  = konf$defaultHz[[spkSex]]==xHz,
                         is_worst    = FALSE   )
    }#ENDFOR xHz
    spkFlags <- opt.tbl$speaker==spk
    opt.tbl[ spkFlags & opt.tbl$score == max(opt.tbl[spkFlags,]$score), ]$is_optimal <- TRUE
    opt.tbl[ spkFlags & opt.tbl$score == min(opt.tbl[spkFlags,]$score), ]$is_worst <- TRUE

    if(verbose) cat("\n")
  }#ENDFOR spk

  return(opt.tbl)
}



# -----------------------------------------------------------------------------
#                                               HELPER FUNCTION: GRID UPDATE
# -----------------------------------------------------------------------------
#
update_grid <- function(grid_list, optimum_list, konf, iteration)
{
  if(iteration<2) stop("update_grid can only be called for iterations >= 2")
  opt.tbl <- optimum_list[[iteration-1]]
  all_speakers <- sort(unique(opt.tbl$speaker))
  outGrid <- list()
  for(spk in all_speakers){# spk=all_speakers[1]

    previous <- c()
    for(it in 1:(iteration-1)) {
      previous <- c(previous, unlist(searchGrid.lst[[it]][spk]))
    }
    previous <- sort(unique(previous))

    if(is.finite(konf$quantizeDigits)) {
      spkOpt <- opt.tbl[opt.tbl$speaker==spk,]
      spkOpt$q <- round(spkOpt$score, digits = konf$quantizeDigits)
      maxScore <- max(spkOpt$q)
      hz <- c()
      x <- which(spkOpt$q==maxScore)
      for(xOpt in x) {
        hz <- c(hz, seq(from=spkOpt[xOpt,]$maxHz-konf$updateGrid$margin, to=spkOpt[xOpt,]$maxHz+konf$updateGrid$margin, by=konf$updateGrid$step ) )
      }
      hz <- sort(unique(hz))

    } else {
      opt.row <- opt.tbl[ opt.tbl$speaker==spk & opt.tbl$is_optimal, ]
      if(nrow(opt.row)!=1) {
        stop(gettextf("optimum row !=1 for %s", spk))
      }
      hz <- seq(from=opt.row[1,]$maxHz-konf$updateGrid$margin, to=opt.row[1,]$maxHz+konf$updateGrid$margin, by=konf$updateGrid$step )
    }
    x  <- !(hz %in% previous)
    hz <- hz[x]
    if(length(hz)>0) {
      outGrid[[spk]] <- hz
    } else {
      warning(gettextf("no more search parameters for %s", spk), immediate. = TRUE)
    }
  }
  return(outGrid)
}

# -----------------------------------------------------------------------------
#                                                        HELPER FUNCTION: CACHE
# -----------------------------------------------------------------------------
#)
load_from_cache <- function(rds, konf)
{
  x <- which(konf$rds==rds)
  if(length(x)==0) {
    stop(gettextf("requested unknown file: %s", rds))
  }
  outData <- NULL
  if(x>1){
    if(!file.exists(rds)){
      return(NULL)
    }
    zeit <- file.mtime(rds)
    dep  <- file.mtime(konf$rds[[x-1]])
    if(!is.finite(dep)){
      stop(gettextf("dependency file not found: %s", konf$rds[[x-1]]))
    }
    if(zeit > dep){
      cat(gettextf("LOADING CACHED RDS FILE: %s\n", rds))
      outData <- readRDS(rds)
    }
  }
  if(is.null(outData)) {
    cat(gettextf("! NOT LOADING RDS FILE -- NEEDS UPDATE: %s\n", rds))
  }
  return(outData)
}



# =============================================================================
# -----------------------------------------------------------------------------
#                                                             START: INPUT DATA
# -----------------------------------------------------------------------------
# =============================================================================

dependencies.tbl <- tibble(rds = unlist(KONFIG$rds, use.names = FALSE))

suppressMessages( inputFiles.tbl <- read_csv(KONFIG$input.csv) %>% mutate(base=NA_character_, mtime=NA_integer_, ok=TRUE) )

for(xr in 1:nrow(inputFiles.tbl)) {
  tmp.tg <- importTextGridToTibble(inputFiles.tbl[xr,]$textGrid, file.id = xr, fix.encoding = KONFIG$isWindows)
  if(is.null(tmp.tg)) {
    inputFiles.tbl[xr,]$ok <- FALSE
    message(gettextf("Cannot import TextGrid: %s", inputFiles.tbl[xr,]$textGrid))
    next()
  }
  if(!file.exists(inputFiles.tbl[xr,]$audio)){
    inputFiles.tbl[xr,]$ok <- FALSE
    message(gettextf("Audio file not found: %s", inputFiles.tbl[xr,]$audio))
    next()
  }
  inputFiles.tbl[xr,]$base <- str_remove( basename(inputFiles.tbl[xr,]$textGrid), KONFIG$tgSuffix)
  inputFiles.tbl[xr,]$mtime <- file.mtime(inputFiles.tbl[xr,]$textGrid)
  # CHECK DATA
  tmp.tbl <- filter(tmp.tg, tier==KONFIG$targetTier, text %in% KONFIG$targetLabels)
  if(nrow(tmp.tbl)==0){
    inputFiles.tbl[xr,]$ok <- FALSE
    warning(gettextf("No target labels in (%d) %s", xr, inputFiles.tbl[xr,]$textGrid), immediate. = TRUE)
  } else {
    for(xl in KONFIG$targetLabels) {
      if(length(which(tmp.tbl$text == xl))==0){
        warning(gettextf("No [%s] labels in (%d) %s", xl, xr, inputFiles.tbl[xr,]$textGrid), immediate. = TRUE)
      }
    }
  }
  rm(tmp.tbl, tmp.tg)
}
vorher <- nrow(inputFiles.tbl)
inputFiles.tbl <- inputFiles.tbl[ inputFiles.tbl$ok, ]
if(nrow(inputFiles.tbl)!=vorher) {
  warning(gettextf("Excluded %d input files", (vorher-nrow(inputFiles.tbl)) ), immediate. = TRUE)
}
rm(vorher)


# CHECK IF INPUT FILES HAVE CHANGED:
saveInputTable <- FALSE
if(file.exists(KONFIG$rds$inputFiles)) {
  oldInputTable <- readRDS(konfig$rds$inputFiles)
  if(nrow(oldInputTable) != nrow(inputFiles.tbl)) {
    saveInputTable <- TRUE
  } else {
    oldInputTable$checked <- FALSE
    for(xr in 1:nrow(inputFiles.tbl)) {
      xOld <- which(inputFiles.tbl[xr,]$textGrid == oldInputTable$textGrid)
      if(length(xOld)==0) {
        saveInputTable <- TRUE
        break()
      }
      if( oldInputTable[xOld,]$mtime != inputFiles.tbl[xr,]$mtime ) {
        saveInputTable <- TRUE
        break()
      } else if( oldInputTable[xOld,]$speaker != inputFiles.tbl[xr,]$speaker ) {
        saveInputTable <- TRUE
        break()
      } else if( oldInputTable[xOld,]$sex != inputFiles.tbl[xr,]$sex ) {
        saveInputTable <- TRUE
        break()
      } else if( oldInputTable[xOld,]$audio != inputFiles.tbl[xr,]$audio ) {
        saveInputTable <- TRUE
        break()
      } else if( oldInputTable[xOld,]$base != inputFiles.tbl[xr,]$base ) {
        saveInputTable <- TRUE
        break()
      }
    }#ENDFOR xr
    rm(xr, xOld)
  }
  rm(oldInputTable)
}

if(saveInputTable){
  cat("INPUT FILES HAVE CHANGED: UPDATING RDS FILE")
  exportToRds(inputFiles.tbl, konfig$rds$inputFiles)
}
rm(saveInputTable)

all_Speakers <- sort(unique(inputFiles.tbl$speaker))

cat(gettextf("Found %d input TextGrid + audio files for %d speakers\n", nrow(inputFiles.tbl), length(all_Speakers)))

searchGrid.lst    <- list()
tokens.lst        <- list()
trackFeatures.lst <- list()
spaceFeatures.lst <- list()
lingFeatures.lst  <- list()
trackScores.lst   <- list()
spaceScores.lst   <- list()
lingScores.lst    <- list()
optimum.lst    <- list()


for(iterationCount in 1:KONFIG$maxUpdates) {

  cat(gettextf("[%s] === START ITERATION %d ===\n", Sys.time(), iterationCount))

  # -----------------------------------------------------------------------------
  #                                                              INIT SEARCH GRID
  # -----------------------------------------------------------------------------
  cat(gettextf("[%s] INITIALIZING SEARCH GRID (%d)...\n", Sys.time(), iterationCount))

  rds_key       <- gettextf("searchGrid%d",iterationCount)
  it_searchGrid <- load_from_cache(KONFIG$rds[[rds_key]], KONFIG)
  if(is.null(it_searchGrid)){
    if(iterationCount==1){
      it_searchGrid <- list()
      globalParams <- seq(KONFIG$searchGrid$min, KONFIG$searchGrid$max, by = KONFIG$searchGrid$step)
      for(spk in all_Speakers){
        it_searchGrid[[spk]] <- globalParams
      }
    } else {
      it_searchGrid <- update_grid(searchGrid.lst, optimum.lst, KONFIG, iterationCount)
      if( length(it_searchGrid) == 0 ){
        cat("No more parameters in search grid!\n")
        break()
      }
    }
    exportToRds(it_searchGrid, KONFIG$rds[[rds_key]])
  }

  searchGrid.lst[[iterationCount]] <- it_searchGrid
  rm(rds_key, it_searchGrid)


  # -----------------------------------------------------------------------------
  #                                                           EXECUTE GRID SEARCH
  # -----------------------------------------------------------------------------
  cat(gettextf("[%s] RUNNING PRAAT GRID SEARCH (%d)...\n", Sys.time(), iterationCount))

  ok <- executeGridSearch(inputFiles.tbl, searchGrid.lst[[iterationCount]], KONFIG)
  if(!ok) {
    warning("There were problems with the grid search!")
    print(warnings())
  }
  rm(ok)


  # -----------------------------------------------------------------------------
  #                                                                  COLLECT DATA
  # -----------------------------------------------------------------------------
  cat(gettextf("[%s] COLLECTING FORMANT DATA (%d)...\n", Sys.time(), iterationCount))

  rds_key     <- gettextf("formants%d",iterationCount)
  it_formants <- load_from_cache(KONFIG$rds[[rds_key]], KONFIG)

  if(is.null(it_formants)){
    it_formants <- importFormants(inputFiles.tbl, searchGrid.lst[[iterationCount]], KONFIG)
    check_formant_labels(it_formants, KONFIG, iterationCount)
    exportToRds(it_formants, KONFIG$rds[[rds_key]])
  }
  rm(rds_key)


  # -----------------------------------------------------------------------------
  #                                                                        TRACKS
  # -----------------------------------------------------------------------------
  cat(gettextf("[%s] COMPUTING TRACK FEATURES (%d)...\n", Sys.time(), iterationCount))

  rds_key          <- gettextf("trackFeatures%d",iterationCount)
  it_trackFeatures <- load_from_cache(KONFIG$rds[[rds_key]], KONFIG)

  if(is.null(it_trackFeatures)){
    it_trackFeatures <- compute_track_features(it_formants, KONFIG)
    exportToRds(it_trackFeatures, KONFIG$rds[[rds_key]])
  }
  trackFeatures.lst[[iterationCount]] <- it_trackFeatures
  rm(rds_key, it_trackFeatures)


  # -----------------------------------------------------------------------------
  #                                                                        TOKENS
  # -----------------------------------------------------------------------------
  cat(gettextf("[%s] COMPUTING TOKENS FROM FORMANT TRACKS (%d)...\n", Sys.time(), iterationCount))

  rds_key   <- gettextf("tokens%d",iterationCount)
  it_tokens <- load_from_cache(KONFIG$rds[[rds_key]], KONFIG)

  if(is.null(it_tokens)){
    it_tokens <- get_tokens(it_formants, KONFIG)
    exportToRds(it_tokens, KONFIG$rds[[rds_key]])
  }
  tokens.lst[[iterationCount]] <- it_tokens
  rm(rds_key, it_tokens)


  # -----------------------------------------------------------------------------
  #                                                                         SPACE
  # -----------------------------------------------------------------------------
  cat(gettextf("[%s] COMPUTING SPACE FEATURES (%d)...\n", Sys.time(), iterationCount))

  rds_key          <- gettextf("spaceFeatures%d",iterationCount)
  it_spaceFeatures <- load_from_cache(KONFIG$rds[[rds_key]], KONFIG)

  if(is.null(it_spaceFeatures)){
    it_spaceFeatures <- compute_space_features(tokens.lst[[iterationCount]], inputFiles.tbl, KONFIG)
    exportToRds(it_spaceFeatures, KONFIG$rds[[rds_key]])
  }
  spaceFeatures.lst[[iterationCount]] <- it_spaceFeatures
  rm(rds_key, it_spaceFeatures)


  # -----------------------------------------------------------------------------
  #                                                                          LING
  # -----------------------------------------------------------------------------
  cat(gettextf("[%s] COMPUTING LINGUISTIC FEATURES (%d)...\n", Sys.time(), iterationCount))

  rds_key         <- gettextf("lingFeatures%d",iterationCount)
  it_lingFeatures <- load_from_cache(KONFIG$rds[[rds_key]], KONFIG)

  if(is.null(it_lingFeatures)){
    it_lingFeatures <- compute_ling_features(tokens.lst[[iterationCount]], inputFiles.tbl, KONFIG)
    exportToRds(it_lingFeatures, KONFIG$rds[[rds_key]])
  }
  lingFeatures.lst[[iterationCount]] <- it_lingFeatures
  rm(rds_key, it_lingFeatures)


  # -----------------------------------------------------------------------------
  #                                                                        SCORES
  # -----------------------------------------------------------------------------
  cat(gettextf("[%s] COMPUTING SCORES (%d)...\n", Sys.time(), iterationCount))

  trackScores.lst[[iterationCount]] <- score_track_features(trackFeatures.lst, inputFiles.tbl, KONFIG, iterationCount)
  spaceScores.lst[[iterationCount]] <- score_space_featurs(spaceFeatures.lst, inputFiles.tbl, KONFIG, iterationCount)
  lingScores.lst[[iterationCount]]  <- score_ling_features(lingFeatures.lst, inputFiles.tbl, KONFIG, iterationCount)

  # -----------------------------------------------------------------------------
  #                                                                       OPTIMUM
  # -----------------------------------------------------------------------------
  cat(gettextf("[%s] COMPUTING OPTIMAL maxHz PARAMETERS (%d)...\n", Sys.time(), iterationCount))

  optimum.lst[[iterationCount]] <- compute_optimum(trackScores.lst, spaceScores.lst, lingScores.lst,
                                                   tokens.lst, inputFiles.tbl, KONFIG, iterationCount)

  rm(it_formants)

  if(iterationCount==KONFIG$maxUpdates) {
    cat("ATTENTION. Reached maximum number of iterations without convergence!\n")
  }
}#ENDFOR iterationCount

# -----------------------------------------------------------------------------
#                                                                       OPTIMUM
# -----------------------------------------------------------------------------
cat(gettextf("[%s] EXPORTING FINAL OPTIMUM...\n", Sys.time()))

if(KONFIG$isWindows){
  write_excel_csv2(x = filter(optimum.lst[[ length(optimum.lst) ]], is_optimal==TRUE) %>% select(speaker,maxHz),
                   path = file.path(KONFIG$outputDir, "optimal_maxHz.csv"))
} else {
  write_csv(x = filter(optimum.lst[[ length(optimum.lst) ]], is_optimal==TRUE) %>% select(speaker,maxHz),
            path = file.path(KONFIG$outputDir, "optimal_maxHz.csv"))
}
cat(gettextf("[%s] ALL DONE. BYE!\n", Sys.time()))
