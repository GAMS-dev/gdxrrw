dyn.load('inspect.so')
inspect <- function(o)
{
  .External("inspect", o)
}
orig <- data.frame(f1=c("A","A","B","B"), f2=c("one","two","one","two"), val=c(1,2,11,12))
inspect (orig)
df2 <- data.frame(f1=c("A","A","B","B"), f2=c("one","two","one","two"), val=c(1,2,11,12))
attr(df2,"row.names") <- c('Aone','Atwo','Bone','Btwo')
inspect (df2)
