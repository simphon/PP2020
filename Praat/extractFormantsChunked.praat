# extractFormantsChunked.praat
# --------------------------------------------------------------------
# Daniel Duran, Albert-Ludwigs-Universität Freiburg
# - daniel.duran@germanistik.uni-freiburg.de
# - http://simphon.net/
# --------------------------------------------------------------------
# 2019-11-21
# Updates:
#   2019-12-30 : exporting formants 1-5 and bandwidths

# ====================================================================
#                                                                SETUP
# ====================================================================

Text reading preferences: "UTF-8"
Text writing preferences: "UTF-8"

include ./_getTimestamp.praat
include ./_csvstrToArray.praat
include ./_strToNumArray.praat

# ====================================================================
#                                                           INPUT FORM
# ====================================================================
form Extract formant values from chunks

  comment Input TextGrid (path and file name)
  text inputTG .TextGrid
  comment Input sound (path and file name)
  text inputS .wav
  boolean is_long_sound 1

  comment Segment labels (separated by commas, no spaces)
  text labelCSV a,a:,6,e:,e,E,E:,@,i:,i,I,o:,o,O,2:,9,u:,u,U,y:,y,Y,aI,aU,OY
  word phonetic_tier_name PHO
  word chunk_tier_name SEG

  real chunk_margin_(in_seconds) 0.5

  comment Formant (burg) options:
  integer max_Formants 5
  integer max_Hertz 5500
  real window_length_(s) 0.025
  integer pre_emph 50

  comment Formant extraction points (comma-separated list of duration ratios):
  word exPointStr 0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75

  comment Output directory:
  text outDir C:\Users\Daniel Duran\Data\Ethnolekt\Analyse\Formanten\Praat\

  boolean verbose 0
  boolean debug 0
  boolean create_subdirectory 1

endform


if verbose == 1
 appendInfoLine: "Running script: formant extraction (on chunks) ..."
endif

# ==============================================================================
#                                                               CHECK PARAMETERS
# ==============================================================================
if max_Formants < 3
  #terminate script with error message
  exitScript: "number of max formants must not be less than 3. Exiting script now!"
endif

if max_Formants > 5
  #terminate script with error message
  exitScript: "number of max formants must not be greater than 5. Exiting script now!"
endif


# ====================================================================
#                                                      OPEN INPUT DATA
# ====================================================================

# NORMALIZE PATHS:
inputS$ = replace$ (inputS$, "\", "/", 0)
inputTG$ = replace$ (inputTG$, "\", "/", 0)


if fileReadable (inputS$) == 0
  # TERMINATE SCRIPT WITH ERROR MESSAGE
  exitScript: "Input sound file not found: ", inputS$
endif

if fileReadable (inputTG$) == 0
  # TERMINATE SCRIPT WITH ERROR MESSAGE
  exitScript: "Input TextGrid file not found: ", inputTG$
endif

# OPEN INPUT FILES:
if is_long_sound
  if verbose == 1
    appendInfoLine: "  opening long sound: ", inputS$
  endif
  mySound = Open long sound file: inputS$
else
  if verbose == 1
    appendInfoLine: "  opening sound: ", inputS$
  endif
  mySound = Read from file: inputS$
endif

mySoundEndTime = Get end time


if verbose == 1
  appendInfoLine: "  opening TextGrid: ", inputTG$
endif
myTextGrid = Read from file: inputTG$



# ========================================================
#                                PRE_PROCESS TARGET LABELS
# ========================================================

@csvstrToArray: labelCSV$


# ========================================================
#                                        FIND TIER NUMBERS
# ========================================================

# EXTRACT TOTAL NUMBER OF TIERS
selectObject: myTextGrid
numberOfTiers = Get number of tiers

# FIND TIERS WITH PHONE AND CHUNK SEGMENTS
phoTierNum = -1
chkTierNum = -1

for i to numberOfTiers
  tierName$ = Get tier name: i
  tierNum = i
  if tierName$ == phonetic_tier_name$
    phoTierNum = tierNum
  endif
  if tierName$ == chunk_tier_name$
    chkTierNum = tierNum
  endif  
endfor

tierNum = -1

# CHECK IF TIERS HAVE BEEN FOUND

if phoTierNum < 0
 exitScript: "Found no tier with name ", phonetic_tier_name$
endif
if chkTierNum < 0
 exitScript: "Found no tier with name ", chunk_tier_name$
endif


# ========================================================
#                                        INITIALIZE OUTPUT
# ========================================================

# NORMALIZE PATH:
outDir$ = replace$ (outDir$, "\", "/", 0)
if endsWith(outDir$, "/")==0
 outDir$ = outDir$ + "/"
endif

@getTimestamp

selectObject: mySound
if is_long_sound
  output_file_prefix$ = selected$ ("LongSound")
else
  output_file_prefix$ = selected$ ("Sound")
endif


if create_subdirectory
  outDirSub$ = outDir$ + getTimestamp.str$ +"/"
  createDirectory: outDirSub$
else
  outDirSub$ = outDir$
endif

outFileFmt$ = outDirSub$ + output_file_prefix$ + "_Formants.csv"

logFile$ = outDirSub$ + output_file_prefix$ + "_Formants.log"

# INITIALIZE OUTPUT FILES:

# WRITE HEADER ONLY IF DATA IS FOUND
outputHeaderWritten = 0

# WRITE CURRENT CONFIGURATION TO LOG FILE:

writeFileLine: logFile$, "SCRIPT: extractFormantsChunked.praat -- PARAMETERS:"
appendFileLine: logFile$, " inputTG               = ", inputTG$
appendFileLine: logFile$, " inputS                = ", inputS$
appendFileLine: logFile$, " is_long_sound         = ", is_long_sound
appendFileLine: logFile$, " labelCSV              = ", labelCSV$
appendFileLine: logFile$, " phonetic_tier_name    = ", phonetic_tier_name$
appendFileLine: logFile$, " chunk_tier_name       = ", chunk_tier_name$
appendFileLine: logFile$, " chunk_margin          = ", chunk_margin

appendFileLine: logFile$, " max_Formants          = ", max_Formants
appendFileLine: logFile$, " max_Hertz             = ", max_Hertz
appendFileLine: logFile$, " window_length         = ", window_length
appendFileLine: logFile$, " pre_emph              = ", pre_emph
appendFileLine: logFile$, " exPointStr            = ", exPointStr$

appendFileLine: logFile$, " outDir                = ", outDir$
appendFileLine: logFile$, " outDirSub             = ", outDirSub$

appendFileLine: logFile$, " verbose               = ", verbose
appendFileLine: logFile$, " debug                 = ", debug
appendFileLine: logFile$, " create_subdirectory   = ", create_subdirectory
appendFileLine: logFile$, ""
appendFileLine: logFile$, " output_file_prefix    = ", output_file_prefix$
appendFileLine: logFile$, " this log file         = ", logFile$
appendFileLine: logFile$, " output file           = ", outFileFmt$
appendFileLine: logFile$, "Start time: ", date$()



# ========================================================
#                        CREATE FORMANT OBJECT (ON DEMAND)
# ========================================================

formant_object_created = 0
formant_object_chunk = -1
formant_object_start = -1

# ========================================================
#                                                     LOOP
# ========================================================


if verbose == 1
  appendInfoLine: "  extracting formants..."
endif

selectObject: myTextGrid
numIntervals = Get number of intervals: phoTierNum
numChunks = Get number of intervals: chkTierNum

@strToNumArray: exPointStr$

zaehlerF = 0
zaehlerS = 0

# LOOP THROUGH ALL INTERVALS IN PHONETIC TIER:
for currentInterval from 1 to numIntervals

  selectObject: myTextGrid

  # EXTRACT THE LABEL OF THE CURRENT INTERVAL:
  currentLabel$ = Get label of interval: phoTierNum, currentInterval

  # CHECK IF CURRENT LABEL IS A TARGET LABEL.
  gefunden=0
  for i to csvstrToArray.size
    if currentLabel$ == csvstrToArray.result$[i]
      gefunden=1
      # BREAK LOOP NOW:
      i = csvstrToArray.size + 1
    endif
  endfor


  if gefunden == 1

    if debug
      appendInfoLine: "  Interval ", currentInterval, " is labeled: ", currentLabel$
    endif

    zaehlerS = zaehlerS + 1

    currentStartTime = Get start point: phoTierNum, currentInterval
    currentEndTime = Get end point: phoTierNum, currentInterval
    duration = currentEndTime - currentStartTime

    currentChunkInterval = Get interval at time... chkTierNum currentStartTime

    if currentChunkInterval <> formant_object_chunk

      if formant_object_chunk > 0
        # REMOVE PREVIOUS OBJECTS
        selectObject: myFormant
        plusObject: meinChunk
        Remove
      endif

      selectObject: myTextGrid
      cutStart = Get start point: chkTierNum, currentChunkInterval
      cutEnd = Get end point: chkTierNum, currentChunkInterval

      if chunk_margin > 0
        cutStart = cutStart - chunk_margin
        if cutStart < 0
          cutStart = 0
        endif
        cutEnd = cutEnd + chunk_margin
        if cutEnd > mySoundEndTime
          cutEnd = mySoundEndTime
        endif
      endif

      selectObject: mySound
      if is_long_sound
        meinChunk = Extract part: cutStart, cutEnd, "no"
      else
        meinChunk = Extract part: cutStart, cutEnd, "rectangular", 1, "no"
      endif

      selectObject: meinChunk
      if debug==1
        myFormant = To Formant (burg)... 0 max_Formants max_Hertz window_length pre_emph
      else
        myFormant = noprogress To Formant (burg)... 0 max_Formants max_Hertz window_length pre_emph
      endif

      formant_object_chunk = currentChunkInterval
      formant_object_start = cutStart
    endif

    # EXTRACT FORMANT VALUES AT SPECIFIED RELATIVE TIME POINTS:
    for xpx from 1 to strToNumArray.size

      qm = (currentStartTime - formant_object_start) + (strToNumArray.result[xpx] * duration)
      qmGlobal = currentStartTime + (strToNumArray.result[xpx] * duration)

      selectObject: myFormant
      f1 = Get value at time... 1 qm Hertz Linear
      f2 = Get value at time... 2 qm Hertz Linear
      f3 = Get value at time... 3 qm Hertz Linear

      b1 = Get bandwidth at time: 1, qm, "hertz", "Linear"
      b2 = Get bandwidth at time: 2, qm, "hertz", "Linear"
      b3 = Get bandwidth at time: 3, qm, "hertz", "Linear"

      if max_Formants > 3
        f4 = Get value at time... 4 qm Hertz Linear
        b4 = Get bandwidth at time: 4, qm, "hertz", "Linear"
      else
        f4 = undefined
        b4 = undefined
      endif

      if max_Formants > 4
        f5 = Get value at time... 5 qm Hertz Linear
        b5 = Get bandwidth at time: 5, qm, "hertz", "Linear"
      else
        f5 = undefined
        b5 = undefined
      endif

      if outputHeaderWritten == 0
        writeFileLine: outFileFmt$, "phoSeg,time,relTime,label,F1,F2,F3,F4,F5,b1,b2,b3,b4,b5"
        outputHeaderWritten = 1
      endif

      appendFileLine: outFileFmt$,
      ... currentInterval, ",",
      ... qmGlobal, ",",
      ... strToNumArray.result[xpx], ",",
      ... currentLabel$, ",",
      ... f1, ",",
      ... f2, ",",
      ... f3, ",",
      ... f4, ",",
      ... f5, ",",
      ... b1, ",",
      ... b2, ",",
      ... b3, ",",
      ... b4, ",",
      ... b5

      zaehlerF = zaehlerF +1
    endfor
  endif
  # END gefunden==1
endfor


appendFileLine: logFile$, ""
appendFileLine: logFile$, "Matched segments:       ", zaehlerS
appendFileLine: logFile$, "Extracted measurements: ", zaehlerF


if formant_object_chunk > 0
 selectObject: myFormant
 plusObject: meinChunk
 Remove
endif


selectObject: mySound
plusObject: myTextGrid
Remove

appendFileLine: logFile$, ""
appendFileLine: logFile$, "All processing finished! ", date$()

if verbose
  appendInfoLine: "  extracted measurements from ", zaehlerS, " segments at ", zaehlerF, " time points"
  if outputHeaderWritten == 0
    appendInfoLine: "  No data exported!"
  else
    appendInfoLine: "  Data table written to: ", outFileFmt$
  endif
  appendInfoLine: "Formant extraction: done. Bye!"
endif
