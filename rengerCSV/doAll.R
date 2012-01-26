# given an input dataframe inDF, return a reshaped dataframe prepped
# for output via wgdx.df or wgdx.lst.
# The output parameter parName will have dimension nDims
gdxReshape <- function(inDF, parName, nDims, aggName=NULL) {
  nCols <- ncol(inDF)
  inNames <- names(inDF)
  idCols <- 1:(nDims-1)
  dtCols <- nDims:nCols

  outDF <- reshape (inDF, idvar=inNames[idCols], varying=list(dtCols),
                    direction="long", times=inNames[dtCols])
  for (i in 1:nDims) {
    outDF[[i]] <- as.factor(outDF[[i]])
  }
  attr(outDF,"symName") <- parName
  if (is.character(aggName)) {
    names(outDF)[nDims] <- aggName
  }
  names(outDF)[nDims+1] <- "value"
  return (outDF)
} # gdxReshape

refsample<-read.csv("REFSAMPLE.dat",sep="\t", header=T)
str(refsample)

refsamplel <- reshape(refsample,idvar=c("PNR","HHNR"),varying=list(3:6),
                      direction="long",times=c("APER","CAR","WKSTAT","ZERO"))
str(refsamplel)
refsamplel[[1]]<-as.factor(refsamplel[[1]])
refsamplel[[2]]<-as.factor(refsamplel[[2]])
refsamplel[[3]]<-as.factor(refsamplel[[3]])
attr(refsamplel,"symName") <- 'refsample'

wgdx.df('entropy',refsamplel)

t <- gdxReshape (refsample,"refsample",3,aggName="QUESTIONS")
str(t)
wgdx.df("entropy2.gdx",t,squeeze='e')

pnr     <- list(unique(refsample$PNR))
pnrlst  <- list(name='pnr', type='set', ts='Person ID', uels=c(pnr))
hhnr    <- list(unique(refsample$HHNR))
hhnrlst <- list(name='hhnr', type='set', ts='Household ID', uels=c(hhnr))
q       <- list(colnames(refsample[,3:6]))
qlst    <- list(name='q', type='set', ts='Questions', uels=c(q))

wgdx.lst("entropyAll",list(pnrlst,hhnrlst,qlst,t),squeeze=FALSE)
