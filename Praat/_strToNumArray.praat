# _strToNumArray.praat
# --------------------------------------------------------------------
# Daniel Duran, Albert-Ludwigs-Universität Freiburg
# - daniel.duran@germanistik.uni-freiburg.de
# - http://simphon.net/
# --------------------------------------------------------------------
#
# splits a string in the form "1.0,2.5,0.3" into an array of numerical values
#
# call:
#  @strToNumArray: numberString$
#
# result:
#  strToNumArray.result[i] -- numerical array
#  strToNumArray.size      -- array length (number of elements)
#
procedure strToNumArray: .numStr$

  .result[1] = 0
  .size = 0
  .tmp = index (.numStr$, ",")

  while .tmp > 0
    .size = .size + 1
    .result[.size] = number( left$(.numStr$, .tmp-1) )
    .numStr$ = mid$(.numStr$, .tmp+1, length (.numStr$)-.tmp)
    .tmp = index (.numStr$, ",")
  endwhile

  if length (.numStr$) > 0
    .size = .size + 1
    .result[.size] = number( .numStr$ )
  endif
endproc
