### Test wgdx.reshape

source ("chkFrames.R")

tryCatch({
  print ("test wgdx.reshape")

  # start with a dataframe as you might read it with read.delim
  df <-
structure(list(hdrA = structure(c(2L, 2L, 2L, 1L, 1L, 1L), .Label = c("crn", 
"wht"), class = "factor"), hdrB = c(100, 101, 102, 103, 104, 
105), hdrC = c(1, 2, 3, 5, 6, 7), hdrD = c(1.11, 2.11, 3.11, 
5.11, 6.11, 7.11), hdrE = c(1.12, 2.12, 3.12, 5.12, 6.12, 7.12
)), .Names = c("hdrA", "hdrB", "hdrC", "hdrD", "hdrE"), row.names = c(NA, 
6L), class = "data.frame", symName = "defName")

  # df has one factor column => one existing index.  The next two
  # columns are integer and could be viewed as data or index columns.
  # The final columns are double and will be data columns.  So this
  # could be a 2-, 3-, or 4-dimensional parameter

  lstA <- structure(list(name = "hdrA", type = "set", uels = list(c("crn", 
"wht"))), .Names = c("name", "type", "uels"))

  lst1 <- wgdx.reshape (df, 2)
  if (! chkLists ("test1 index 1", lst1[[1]], lstA))
    stop ("test1: index set 1 'hdrA' failed check"); 

  print ("all tests for wgdx.reshape passed")
  return (TRUE)
}

, error = function(ex) { print(ex) ; return (FALSE) }
)
