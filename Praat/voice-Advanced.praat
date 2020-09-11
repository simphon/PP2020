# voice-Advanced.praat
# --------------------------------------------------------------------
# Daniel Duran, Albert-Ludwigs-Universität Freiburg
# - daniel.duran@germanistik.uni-freiburg.de
# - http://simphon.net/
# --------------------------------------------------------------------

# ========================================================
#                                                    SETUP
# ========================================================

Text reading preferences: "UTF-8"
Text writing preferences: "UTF-8"

include ./_csvstrToArray.praat
include ./_getTimestamp.praat

# ========================================================
#                                               INPUT FORM
# ========================================================

form Voiced-unvoiced extraction (advanced)

  comment Input sound (path and file name):
  text inSound D:\Uni-Freiburg\Ethnolekt\Daten\Aufnahmen\Kerngruppe\Leseliste\M\nicht normalisiert\R2-EX2-08_M077_L1.wav

  comment Input TextGrid (path and file name):
  text inTG X:\Daten\Annotation\Kerngruppe\Leseliste\M\R2-EX2-08_M077_L1.TextGrid

  word Target_tier_name PHO
  comment Target segment labels (comma separated list)
  text labelCSV p,b,t,d,k,g

  comment Output table (path and file name):
  text outFile C:\Users\Daniel Duran\Data\Ethnolekt\Analyse\Voicing_Advanced\01\R2-EX2-08_M077_L1_voice.tsv

  boolean Extract_vuv_table 1

  comment Pitch object parameters:
  real time_step_(s,_0_=_auto) 0.0
  integer pitch_floor_(Hz) 75
  positive maximum_number_of_candidates 15
  boolean very_accurate 1
  real silence_threshold 0.03
  real voicing_threshold 0.45
  real octave_cost 0.01
  real octave_jump_cost 0.35
  real voiced_unvoiced_cost 0.14
  integer pitch_ceiling_(Hz) 600

  comment Voice report parameters:
  real maximum_period_factor 1.3
  real maximum_amplitude_factor 1.6

  comment Show processing info?
  boolean Verbose 1

  boolean Write_log_file 1

endform

# ========================================================
#                                          NORMALIZE PATHS
# ========================================================

# normalize paths:

inSound$ = replace$(inSound$, "\", "/", 0)
inTG$ = replace$(inTG$, "\", "/", 0)
outFile$ = replace$(outFile$, "\", "/", 0)

if verbose == 1
  appendInfoLine: "START: Voiced-unvoiced extraction (ICLaVE 2019): ADVANCED ..."
  appendInfoLine: "  input sound file = ", inSound$
  appendInfoLine: "  input TextGrid = ", inTG$
  appendInfoLine: "  output file = ", outFile$
endif

# ========================================================
#                                            INIT LOG FILE
# ========================================================

if write_log_file == 1

  @getTimestamp

  logFile$ = outFile$ + "_" + getTimestamp.str$ + ".log"

  appendFileLine:  logFile$, "log file (this file) = ", logFile$
  appendFileLine:  logFile$, "time stamp           = ", getTimestamp.str$
  appendFileLine:  logFile$, "input sound file     = ", inSound$
  appendFileLine:  logFile$, "input TextGrid       = ", inTG$

  appendFileLine:  logFile$, "target tier name     = ", target_tier_name$
  appendFileLine:  logFile$, "target labels        = ", labelCSV$

  appendFileLine:  logFile$, "output table file    = ", outFile$

  appendFileLine:  logFile$, "Pitch: time_step                    = ", time_step
  appendFileLine:  logFile$, "Pitch: pitch_floor                  = ", pitch_floor
  appendFileLine:  logFile$, "Pitch: maximum_number_of_candidates = ", maximum_number_of_candidates
  appendFileLine:  logFile$, "Pitch: very_accurate                = ", very_accurate
  appendFileLine:  logFile$, "Pitch: silence_threshold            = ", silence_threshold
  appendFileLine:  logFile$, "Pitch: voicing_threshold            = ", voicing_threshold
  appendFileLine:  logFile$, "Pitch: octave_cost                  = ", octave_cost
  appendFileLine:  logFile$, "Pitch: octave_jump_cost             = ", octave_jump_cost
  appendFileLine:  logFile$, "Pitch: voiced_unvoiced_cost         = ", voiced_unvoiced_cost
  appendFileLine:  logFile$, "Pitch: pitch_ceiling                = ", pitch_ceiling

  appendFileLine:  logFile$, "Voice: maximum period factor    = ", maximum_period_factor
  appendFileLine:  logFile$, "Voice: maximum amplitude factor = ", maximum_amplitude_factor
  appendFileLine:  logFile$, "Voice: silence threshold        = ", silence_threshold
  appendFileLine:  logFile$, "Voice: voicing threshold        = ", voicing_threshold

endif

# ========================================================
#                                          OPEN INPUT DATA
# ========================================================

# READ SOUND FILE:
if fileReadable (inSound$) == 0
   # TERMINATE SCRIPT WITH ERROR MESSAGE
   exitScript: "Input sound file not found: ", inSound$
endif
sound = Read from file: inSound$

# READ TEXTGRID FILE:
if fileReadable (inTG$) == 0
   # TERMINATE SCRIPT WITH ERROR MESSAGE
   select sound
   Remove
   exitScript: "Input TextGrid file not found: ", inTG$
endif
textGrid = Read from file: inTG$


# ========================================================
#                                PRE-PROCESS TARGET LABELS
# ========================================================
@csvstrToArray: labelCSV$

# ========================================================
#                                        INIT OUTPUT TABLE
# ========================================================

#CREATE AN OUTPUT TABLE
table = Create Table with column names... table 1 tier interval lab duration voiced unvoiced dovb pitch.md pitch.mn pitch.sd pitch.min pitch.max

# ========================================================
#                                               PROCESS...
# ========================================================

# GET TIER NUMBER BY TIER NAME:
selectObject: textGrid
numberOfTiers = Get number of tiers
tierNum = -1
for i to numberOfTiers
  tierName$ = Get tier name: i
  if tierName$ == target_tier_name$
    tierNum = i
  endif
endfor
if tierNum < 0
  exitScript: "Found no tier with name ", target_tier_name$
endif

# CREATE PITCH OBJECT:
selectObject: sound
pitch = To Pitch (ac): time_step, pitch_floor, maximum_number_of_candidates, very_accurate, silence_threshold, voicing_threshold, octave_cost, octave_jump_cost, voiced_unvoiced_cost, pitch_ceiling

# CREATE POINT PROCESS
selectObject: sound
plusObject: pitch
pointprocess = To PointProcess (cc)


# LOOP THROUGH ALL INTERVALS ON TIER "tierNum"
selectObject: textGrid
nIntervals = Get number of intervals... tierNum
zaehlerSeg = 0
for iInterval to nIntervals

  # GET LABEL OF CURRENT INTERVAL
  selectObject: textGrid
  label$ = Get label of interval... tierNum iInterval

  # CHECK IF INTERVAL IS A TARGET SEGMENT
  gefunden=0
  if length(label$) > 0
    for i to csvstrToArray.size
      if label$ == csvstrToArray.result$[i]
        gefunden=1
        # BREAK LOOP:
        i = csvstrToArray.size + 1
      endif
    endfor
  endif

  if gefunden == 1

    zaehlerSeg += 1

    # GET START AND END POINT + DURATION OF INTERVAL: 
    start = Get start point... tierNum iInterval
    end = Get end point... tierNum iInterval
    duration = end-start
    duration = duration *1000

    # VOICED-UNVOICED
    selectObject: sound
    plusObject: pitch
    plusObject: pointprocess
    voiceReport$ = Voice report: start, end, pitch_floor, pitch_ceiling, maximum_period_factor, maximum_amplitude_factor, silence_threshold, voicing_threshold
    unvoice = extractNumber (voiceReport$, "Fraction of locally unvoiced frames: ")
    voice = 1-unvoice
    dovb = extractNumber (voiceReport$, "Degree of voice breaks: ")
    pitch_md = extractNumber (voiceReport$, "Median pitch: ")
    pitch_mn = extractNumber (voiceReport$, "Mean pitch: ")
    pitch_sd = extractNumber (voiceReport$, "Standard deviation: ")
    pitch_min = extractNumber (voiceReport$, "Minimum pitch: ")
    pitch_max = extractNumber (voiceReport$, "Maximum pitch: ")

    selectObject: table
    Set string value... zaehlerSeg tier 'target_tier_name$'
    Set numeric value... zaehlerSeg interval 'iInterval'
    Set string value... zaehlerSeg lab 'label$'
    Set numeric value... zaehlerSeg duration 'duration'
    
    if unvoice <> undefined
      Set numeric value... zaehlerSeg voiced 'voice'
      Set numeric value... zaehlerSeg unvoiced 'unvoice'
    else
      Set string value... zaehlerSeg unvoiced "NA"
      Set string value... zaehlerSeg voiced "NA"
    endif

    if dovb <> undefined
      Set numeric value... zaehlerSeg dovb 'dovb'
    else
      Set string value... zaehlerSeg dovb "NA"
    endif

    if dovb <> undefined
      Set numeric value... zaehlerSeg dovb 'dovb'
    else
      Set string value... zaehlerSeg dovb "NA"
    endif

    if pitch_md <> undefined
      Set numeric value... zaehlerSeg pitch.md 'pitch_md'
    else
      Set string value... zaehlerSeg pitch.md "NA"
    endif

    if pitch_mn <> undefined
      Set numeric value... zaehlerSeg pitch.mn 'pitch_mn'
    else
      Set string value... zaehlerSeg pitch.mn "NA"
    endif

    if pitch_sd <> undefined
      Set numeric value... zaehlerSeg pitch.sd 'pitch_sd'
    else
      Set string value... zaehlerSeg pitch.sd "NA"
    endif

    if pitch_min <> undefined
      Set numeric value... zaehlerSeg pitch.min 'pitch_min'
    else
      Set string value... zaehlerSeg pitch.min "NA"
    endif

    if pitch_max <> undefined
      Set numeric value... zaehlerSeg pitch.max 'pitch_max'
    else
      Set string value... zaehlerSeg pitch.max "NA"
    endif

    Append row
  endif

endfor



if extract_vuv_table == 1

  output_file_vuv$ = outFile$ + "_VUV.tab"
  runScript: "voicing-VUV.praat", inSound$, output_file_vuv$, time_step, pitch_floor,  maximum_number_of_candidates, very_accurate, silence_threshold, voicing_threshold, octave_cost, octave_jump_cost, voiced_unvoiced_cost, pitch_ceiling, 1, 1, 5, 0.02, 0.01, verbose

endif

selectObject: sound
plusObject: textGrid
plusObject: pitch
plusObject: pointprocess
Remove

selectObject: table
lastRow = Get number of rows
if lastRow > 0
  Remove row... lastRow
  Save as tab-separated file: outFile$
else
  if verbose == 1
    appendInfoLine: "  No data found: skipping empty table!"
  endif
endif

selectObject: table
Remove

if verbose == 1
  appendInfoLine: "  Found ", zaehlerSeg, " target segments."
endif
