### Test gdxInfo
# We get a list of symbol names from the transport data

source ("chkSame.R")

tryCatch({

  fn <- "trnsport.gdx"
  s <- gdxInfo (fn, dump=FALSE, returnList=TRUE)

  if (!is.list(s))
    stop ("Expected gdxInfo output to be in list form")
  if (10 != length(s))
    stop ("Expected gdxInfo output to have length 10")
  if (12 != s$symCount)
    stop ("gdxInfo: expected trnsport symCount==12")
  if (5 != s$uelCount)
    stop ("gdxInfo: expected trnsport uelCount==5")
  if (! chkSameVec("sets", c("i","j"), s$sets))
    stop ("gdxInfo: s$sets for trnsport is bogus")
  if (! chkSameVec("parameters", c("a","b", "d", "f", "c"), s$parameters))
    stop ("gdxInfo: s$parameters for trnsport is bogus")
  if (! chkSameVec("variables", c("x","z"), s$variables))
    stop ("gdxInfo: s$variables for trnsport is bogus")
  if (! chkSameVec("equations", c("cost","supply", "demand"), s$equations))
    stop ("gdxInfo: s$equations for trnsport is bogus")
  if (! chkSameVec("aliases", character(0), s$aliases))
    stop ("gdxInfo: s$aliases for trnsport is bogus")

  print ("Successfully completed gdxInfo test 1")
  return (TRUE)
}

, error = function(ex) { print(ex) ; return (FALSE) }
)
