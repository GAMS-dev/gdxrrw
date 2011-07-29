## how can I do a sort of "pivot table" operation on a data frame?
#  Specifically, I start with a data frame with multiple columns of
#  factors and a final numeric column.  I want to take a factor column
#  and move it over so that each level in the factor becomes its own
#  column.  At the same time I want to remove the numeric column since
#  all the values will be in the new columns.
#  A 3-dimensional example follows.


orig <- data.frame(f1=c("A","A","A","A","A","A","B","B","B","B","B","B"), f2=c("2000","2000","2001","2001","2002","2002"), f3=c("whe","crn"), val=1:12)


ttt <- reshape(orig, v.names="val",idvar=c("f1","f3"),timevar="f2", direction="wide")
names(ttt) <- c("f1","f3",levels(orig$f2))
