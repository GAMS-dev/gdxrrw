dyn.load('inspect.so')
inspect <- function(o)
{
  .External("inspect", o)
}
orig <- data.frame(f1=c("A","B","C"), val=c(20,11,12))
rr <- inspect (orig)

inspect(rr)

rr
