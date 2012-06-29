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
  lstB <- structure(list(name = "hdrB", type = "set", uels = list(c("100", 
"200","300","400","500","600"))), .Names = c("name", "type", "uels"))
  lstC <- structure(list(name = "hdrC", type = "set", uels = list(c("1", 
"2","3","4","5","6"))), .Names = c("name", "type", "uels"))

  ## -----------------------------------------------------------------------------
  ## test1: simple call, symDim=2
  lst1 <- wgdx.reshape (df, 2)
  if (! chkLists ("test1 index 1", lst1[[1]], lstA))
    stop ("test1: index set 1 'hdrA' failed check");
  lstT1 <- structure(list(
                          name = "time", type = "set",
                          uels = list(c("hdrB", "hdrC", "hdrD", "hdrE"))),
                     .Names = c("name", "type", "uels"))
  if (! chkLists ("test1 index 2(time agg)", lst1[[2]], lstT1))
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


  ## -----------------------------------------------------------------------------
  ## test2: simple call, symDim=3
  lst2 <- wgdx.reshape (df, 3)
  if (! chkLists ("test2 index 1", lst2[[1]], lstA))
    stop ("test2: index set 1 'hdrA' failed check");
  if (! chkLists ("test2 index 2", lst2[[2]], lstB))
    stop ("test2: index set 2 'hdrB' failed check");
  lstT2 <- structure(list(
                         name = "time", type = "set",
                         uels = list(c("hdrC", "hdrD", "hdrE"))),
                    .Names = c("name", "type", "uels"))
  if (! chkLists ("test2 index 3(time agg)", lst2[[3]], lstT2))
    stop ("test2: index set 3 'time' failed check");
df2_ <-
structure(list(hdrA = structure(c(2L, 2L, 2L, 1L, 1L, 1L, 2L, 
2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L), class = "factor", .Label = c("crn", 
"wht")), hdrB = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 
4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L), class = "factor", .Label = c("100", 
"200", "300", "400", "500", "600")), time = structure(c(1L, 1L, 
1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L
), .Label = c("hdrC", "hdrD", "hdrE"), class = "factor"), value = c(1, 
2, 3, 4, 5, 6, 1.4, 2.4, 3.4, 4.4, 5.4, 6.4, 1.5, 2.5, 3.5, 4.5, 
5.5, 6.5)), .Names = c("hdrA", "hdrB", "time", "value"), row.names = c(NA, 
-18L), class = "data.frame", symName = "defName")
  if (! chkFrames ("test2 df", lst2[[4]], df2_))
    stop ("test2: data frame failed check");


  ## -----------------------------------------------------------------------------
  ## test3: simple call, symDim=4
  lst3 <- wgdx.reshape (df, 4)
  if (! chkLists ("test3 index 1", lst3[[1]], lstA))
    stop ("test3: index set 1 'hdrA' failed check");
  if (! chkLists ("test3 index 2", lst3[[2]], lstB))
    stop ("test3: index set 2 'hdrB' failed check");
  if (! chkLists ("test3 index 3", lst3[[3]], lstC))
    stop ("test3: index set 3 'hdrC' failed check");
  lstT3 <- structure(list(
                         name = "time", type = "set",
                         uels = list(c("hdrD", "hdrE"))),
                    .Names = c("name", "type", "uels"))
  if (! chkLists ("test3 index 4(time agg)", lst3[[4]], lstT3))
    stop ("test3: index set 4 'time' failed check");
df3_ <-
structure(list(hdrA = structure(c(2L, 2L, 2L, 1L, 1L, 1L, 2L, 
2L, 2L, 1L, 1L, 1L), class = "factor", .Label = c("crn", "wht"
)), hdrB = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 
5L, 6L), class = "factor", .Label = c("100", "200", "300", "400", 
"500", "600")), hdrC = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 1L, 
2L, 3L, 4L, 5L, 6L), class = "factor", .Label = c("1", "2", "3", 
"4", "5", "6")), time = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 
2L, 2L, 2L, 2L, 2L), .Label = c("hdrD", "hdrE"), class = "factor"), 
    value = c(1.4, 2.4, 3.4, 4.4, 5.4, 6.4, 1.5, 2.5, 3.5, 4.5, 
    5.5, 6.5)), .Names = c("hdrA", "hdrB", "hdrC", "time", "value"
), row.names = c(NA, -12L), class = "data.frame", symName = "defName")
  if (! chkFrames ("test3 df", lst3[[5]], df3_))
    stop ("test3: data frame failed check");


  ## -----------------------------------------------------------------------------
  ## test4: use some optional args
  lst4 <- wgdx.reshape (df, 4, tName = 'leftovers', symName = 'mySymName')
  if (! chkLists ("test4 index 1", lst4[[1]], lstA))
    stop ("test4: index set 1 'hdrA' failed check");
  if (! chkLists ("test3 index 2", lst4[[2]], lstB))
    stop ("test4: index set 2 'hdrB' failed check");
  if (! chkLists ("test3 index 3", lst4[[3]], lstC))
    stop ("test4: index set 3 'hdrC' failed check");
  lstT4 <- lstT3
  lstT4$name <- 'leftovers'
  if (! chkLists ("test4 index 4(time agg)", lst4[[4]], lstT4))
    stop ("test4: index set 4 'time' failed check");
  df4_ <- df3_
  attr(df4_,"symName") <- 'mySymName'
  names(df4_)[[4]] <- 'leftovers'
  if (! chkFrames ("test4 df", lst4[[5]], df4_))
    stop ("test4: data frame failed check");
  


  ## -----------------------------------------------------------------------------
  print ("all tests for wgdx.reshape passed")
  return (TRUE)
}

, error = function(ex) { print(ex) ; return (FALSE) }
)
