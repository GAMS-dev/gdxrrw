## how can I do a sort of "pivot table" operation on a data frame?
#  Specifically, I start with a data frame with multiple columns of
#  factors and a final numeric column.  I want to take a factor column
#  and move it over so that each level in the factor becomes its own
#  column.  At the same time I want to remove the numeric column since
#  all the values will be in the new columns.
#  An example follows.


orig <- data.frame(f1=c("A","A","B","B"), f2=c("one","two","one","two"), val=c(1,2,11,12))
want <- data.frame(f1=c("A","B"),one=c(1,11),two=c(2,12))

# 
