## Renger's query about reading a data frame with some indices "on the column"

orig <- data.frame(f1=c("i0","i1","i2","i3", "i4"), f2=c("one","one","two","two","three"), APER=c(2,2,3,3,3),CAR=c(1,1,0,0,1),WKSTAT=c(1,0,0,0,0),ZERO=c(0,0,0,0,0))

xxx <- reshape(orig,idvar=c("f1","f2"),varying=list(3:6),direction="long",times=c("APER","CAR","WKSTAT","ZERO"))
xxx[[3]] <- as.factor(xxx[[3]])
attr(xxx,"symName") <- 'dirkseParm';

wgdx.df('rrr',xxx)
