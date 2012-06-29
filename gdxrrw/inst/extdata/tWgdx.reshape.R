### Test wgdx.reshape

source ("chkFrames.R")

tryCatch({
  print ("test wgdx.reshape")

  # start with a dataframe as you might read it with read.delim
  df <-
structure(list(hdrA = structure(c(2L, 2L, 2L, 1L, 1L, 1L), .Label = c("crn", 
"wht"), class = "factor"), hdrB = c(100, 200, 300, 400, 500, 
600), hdrC = c(1, 2, 3, 4, 5, 6), hdrD = c(1.4, 2.4, 3.4, 
4.4, 5.4, 6.4), hdrE = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5)),
.Names = c("hdrA", "hdrB", "hdrC", "hdrD", "hdrE"),
row.names = c(NA, -6L), class = "data.frame", symName = "defName")

  # df has one factor column => one existing index.  The next two
  # columns are integer and could be viewed as data or index columns.
  # The final columns are double and will be data columns.  So this
  # could be a 2-, 3-, or 4-dimensional parameter

  lstA <- structure(list(name = "hdrA", type = "set", uels = list(c("crn", 
"wht"))), .Names = c("name", "type", "uels"))

  lst1 <- wgdx.reshape (df, 2)
  if (! chkLists ("test1 index 1", lst1[[1]], lstA))
    stop ("test1: index set 1 'hdrA' failed check");
  lstT <- structure(list(
                         name = "time", type = "set",
                         uels = list(c("hdrB", "hdrC", "hdrD", "hdrE"))),
                    .Names = c("name", "type", "uels"))
  if (! chkLists ("test1 index 2(time agg)", lst1[[2]], lstT))
    stop ("test1: index set 2 'time' failed check");

  df1_ <- structure(list(hdrA = structure(c(2L, 2L, 2L, 1L, 1L, 1L, 2L, 
2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 
1L), class = "factor", .Label = c("crn", "wht")), time = structure(c(1L, 
1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 
3L, 4L, 4L, 4L, 4L, 4L, 4L), .Label = c("hdrB", "hdrC", "hdrD", 
"hdrE"), class = "factor"), value = c(100, 200, 300, 400, 500, 
600, 1, 2, 3, 4, 5, 6, 1.4, 2.4, 3.4, 4.4, 5.4, 6.4, 1.5, 2.5, 
3.5, 4.5, 5.5, 6.5)), .Names = c("hdrA", "time", "value"), row.names = c(NA, 
-24L), class = "data.frame", symName = "defName")
  if (! chkFrames ("test1 df", lst1[[3]], df1_))
    stop ("test1: data frame failed check");


  print ("all tests for wgdx.reshape passed")
  return (TRUE)
}

, error = function(ex) { print(ex) ; return (FALSE) }
)
