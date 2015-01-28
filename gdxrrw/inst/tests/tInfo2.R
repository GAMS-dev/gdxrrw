### Test gdxInfo
# with returnDF=TRUE we get data frames with symbol info for the transport data

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

# compare the data frames f1 and f2, return TRUE if the same, FALSE o/w
chkFrame <- function(s, f1, f2) {
  if (! is.data.frame(f1))   return (FALSE)
  if (! is.data.frame(f2))   return (FALSE)
  nc     <- ncol(f1)
  if (nc != ncol(f2))     return (FALSE)
  nr     <- nrow(f1)
  if (nr != nrow(f2))     return (FALSE)
  if (nr == 0)            return (TRUE)
  for (k in c(1:nr)) {
    if (f1[k,"name"] != f2[k,"name"]) {
      print (paste("checking", s, ": item", k, "name is wrong"))
      return (FALSE)
    }
    if (f1[k,"index"] != f2[k,"index"]) {
      print (paste("checking", s, ": item", k, "index is wrong"))
      return (FALSE)
    }
    if (f1[k,"dim"] != f2[k,"dim"]) {
      print (paste("checking", s, ": item", k, "dim is wrong"))
      return (FALSE)
    }
    if (f1[k,"card"] != f2[k,"card"]) {
      print (paste("checking", s, ": item", k, "card is wrong"))
      return (FALSE)
    }
    if (! (is.null(f1[k,"text"]) && is.null(f2[k,"text"]))) {
      if (f1[k,"text"] != f2[k,"text"]) {
        print (paste("checking", s, ": item", k, "text is wrong"))
        return (FALSE)
      }
    }
    f1d <- f1[k,"doms"][[1]]
    f2d <- f2[k,"doms"][[1]]
    if (length(f1d) != length(f2d)) {
      print (paste("checking", s, ": item", k, "doms length is wrong"))
      return (FALSE)
    }
    n <- length(f1d)
    if (n > 0) {
      for (kk in c(1:n)) {
        if (f1d[kk] != f2d[kk]) {
          print (paste("checking", s, ": item", k, "doms is wrong"))
          return (FALSE)
        }
      }
    }
  }
  return (TRUE)
} # chkFrame

# compare the data frames f1 and f2, return TRUE if the same, FALSE o/w
chkVarEqu <- function(s, f1, f2) {
  if (! is.data.frame(f1))   return (FALSE)
  if (! is.data.frame(f2))   return (FALSE)
  nc     <- ncol(f1)
  if (nc != ncol(f2))     return (FALSE)
  nr     <- nrow(f1)
  if (nr != nrow(f2))     return (FALSE)
  if (nr == 0)            return (TRUE)
  for (k in c(1:nr)) {
    if (f1[k,"name"] != f2[k,"name"]) {
      print (paste("checking", s, ": item", k, "name is wrong"))
      return (FALSE)
    }
    if (f1[k,"index"] != f2[k,"index"]) {
      print (paste("checking", s, ": item", k, "index is wrong"))
      return (FALSE)
    }
    if (f1[k,"dim"] != f2[k,"dim"]) {
      print (paste("checking", s, ": item", k, "dim is wrong"))
      return (FALSE)
    }
    if (f1[k,"card"] != f2[k,"card"]) {
      print (paste("checking", s, ": item", k, "card is wrong"))
      return (FALSE)
    }
    if (! (is.null(f1[k,"text"]) && is.null(f2[k,"text"]))) {
      print ("unchecked text")
      return (FALSE)
    }
    f1d <- f1[k,"doms"][[1]]
    f2d <- f2[k,"doms"][[1]]
    if (length(f1d) != length(f2d)) {
      print (paste("checking", s, ": item", k, "doms length is wrong"))
      return (FALSE)
    }
    n <- length(f1d)
    if (n > 0) {
      for (kk in c(1:n)) {
        if (f1d[kk] != f2d[kk]) {
          print (paste("checking", s, ": item", k, "doms is wrong"))
          return (FALSE)
        }
      }
    }
  }
  return (TRUE)
} # chkVarEqu

tryCatch({

  fn <- "trnsport.gdx"
  s <- gdxInfo (fn, dump=FALSE, returnDF=TRUE)

  if (!is.list(s))
    stop ("Expected gdxInfo output to be in list form")
  if (10 != length(s))
    stop ("Expected gdxInfo output to have length 10")
  if (12 != s$symCount)
    stop ("gdxInfo: expected trnsport symCount==12")
  if (5 != s$uelCount)
    stop ("gdxInfo: expected trnsport uelCount==5")

  s_sets <- structure(list(
          name = c("i", "j"),
          index = c(1, 2),
          dim = c(1, 1),
          card = c(2, 3),
          text = c("canning plants", "markets"),
          doms = c("0", "0")),
      .Names = c("name", "index", "dim", "card", "text", "doms"),
      row.names = 1:2, class = "data.frame")

  s_parameters <- structure(list(
    name = c("a", "b", "d", "f", "c"),
    index = 3:7,
    dim = c(1L, 1L, 2L, 0L, 2L),
    card = c(2L, 3L, 6L, 1L, 6L),
    text = c("capacity of plant i in cases", "demand at market j in cases", 
      "distance in thousands of miles", "freight in dollars per case per thousand miles", 
      "transport cost in thousands of dollars per case"),
    doms = structure(list(1L, 2L, 1:2, integer(0), 1:2), class = "AsIs")),
  .Names = c("name", "index", "dim", "card", "text", "doms"),
  row.names = c(NA, 5L), class = "data.frame")

  s_variables <- structure(list(
    name = c("x", "z"),
    index = 8:9,
    dim = c(2L, 0L),
    card = c(6L, 1L),
    doms = structure(list(1:2, integer(0)), class = "AsIs")),
  .Names = c("name", "index", "dim", "card", "doms"),
  row.names = 1:2, class = "data.frame")

  s_equations <- structure(list(
    name = c("cost", "supply", "demand"),
    index = 10:12,
    dim = c(0L, 1L, 1L),
    card = 1:3,
    doms = structure(list(integer(0), 1L, 2L), class = "AsIs")),
  .Names = c("name", "index", "dim", "card", "doms"),
  row.names = c(NA, 3L), class = "data.frame")

  if (! chkFrame("sets", s_sets, s$sets))
    stop ("gdxInfo: s$sets for trnsport is bogus")
  if (! chkFrame("parameters", s_parameters, s$parameters))
    stop ("gdxInfo: s$parameters for trnsport is bogus")
  if (! chkFrame("variables", s_variables, s$variables))
    stop ("gdxInfo: s$variables for trnsport is bogus")
  if (! chkFrame("equations", s_equations, s$equations))
    stop ("gdxInfo: s$equations for trnsport is bogus")
  if (0 != nrow(s$aliases))
    stop ("gdxInfo: s$aliases for trnsport is bogus")

  print ("Successfully completed gdxInfo test 2")
  TRUE
}

, error = function(ex) { print(ex) ; FALSE }
)
