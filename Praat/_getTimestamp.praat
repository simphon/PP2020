# _getTimestamp.praat
# --------------------------------------------------------------------
# Daniel Duran, Albert-Ludwigs-Universität Freiburg
# - daniel.duran@germanistik.uni-freiburg.de
# - http://simphon.net/
# --------------------------------------------------------------------
# Creates a time stamp according to the current date and time in a format
# which is suitable for file or directory names.
#
# call:
#   @getTimestamp
#
# result:
#   getTimestamp.str$
#
procedure getTimestamp

   .dict$["Jan"] = "01"
   .dict$["Feb"] = "02"
   .dict$["Mar"] = "03"
   .dict$["Apr"] = "04"
   .dict$["May"] = "05"
   .dict$["Jun"] = "06"
   .dict$["Jul"] = "07"
   .dict$["Aug"] = "08"
   .dict$["Sep"] = "09"
   .dict$["Oct"] = "10"
   .dict$["Nov"] = "11"
   .dict$["Dec"] = "12"

   .datum$ = date$()

   .str$ = mid$(.datum$, 21, 4) +"-"+ .dict$[mid$(.datum$, 5, 3)] +"-"+ mid$(.datum$, 9, 2) +"+"+ mid$(.datum$, 12, 2) + mid$(.datum$, 15, 2) + mid$(.datum$, 18, 2)

endproc
