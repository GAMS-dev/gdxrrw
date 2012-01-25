### Test gdxInfo
# We get a list of symbol names from the transport data

# compare the character vectors v1 and v2, return TRUE if the same, FALSE o/w
chkVec <- function(s, v1,v2) {
  if (! is.vector(v1,mode="character"))   return (FALSE)
  if (! is.vector(v2,mode="character"))   return (FALSE)
  n <- length(v1)
  if (n != length(v2))     return (FALSE)
  if (n == 0)              return (TRUE)
  for (k in c(1:n)) {
    if (v1[k] != v2[k]) {
      print (paste("checking", s, ": item", k, "is wrong"))
      return (FALSE)
    }
  }
  return (TRUE)
}

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
  if (! chkVec("sets", c("i","j"), s$sets))
    stop ("gdxInfo: s$sets for trnsport is bogus")
  if (! chkVec("parameters", c("a","b", "d", "f", "c"), s$parameters))
    stop ("gdxInfo: s$parameters for trnsport is bogus")
  if (! chkVec("variables", c("x","z"), s$variables))
    stop ("gdxInfo: s$variables for trnsport is bogus")
  if (! chkVec("equations", c("cost","supply", "demand"), s$equations))
    stop ("gdxInfo: s$equations for trnsport is bogus")
  if (! chkVec("aliases", character(0), s$aliases))
    stop ("gdxInfo: s$aliases for trnsport is bogus")

  print ("Successfully completed gdxInfo tests")
  return (TRUE)
}

, error = function(ex) { print(ex) ; return (FALSE) }
)
