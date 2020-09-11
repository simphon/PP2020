# _csvstrToArray.praat
# --------------------------------------------------------------------
# Daniel Duran, Albert-Ludwigs-Universität Freiburg
# - daniel.duran@germanistik.uni-freiburg.de
# - http://simphon.net/
# --------------------------------------------------------------------
#
# splits a string in the form "a,b,c" into an array of strings
#
# call:
#  @csvstrToArray: csvString$
#
# result:
#  csvstrToArray.result$[i] -- array with string values
#  csvstrToArray.size       -- array length (number of elements)
#
procedure csvstrToArray: .csvStr$

  .result$[1] = ""
  .size = 0
  .tmp = index (.csvStr$, ",")

  while .tmp > 0
    .size = .size + 1
    .result$[.size] =  left$(.csvStr$, .tmp-1)
    .csvStr$ = mid$(.csvStr$, .tmp+1, length (.csvStr$)-.tmp)
    .tmp = index (.csvStr$, ",")
  endwhile

  if length (.csvStr$) > 0
    .size = .size + 1
    .result$[.size] = .csvStr$
  endif
endproc
