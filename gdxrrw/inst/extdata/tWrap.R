### Test all the wrappers: rgdx.set, rgdx.param, rgdx.scalar, and wgdx.lst
# We first read the trnsport data from GDX,
# and write it out again using the wgdx.lst wrapper

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

testName <- 'tWrap - testing GDXRRW wrappers'

errFunc <- function(ex) {
  print (paste(testName,": FAILED"))
  print(ex)
  FALSE
} # errFunc

tryCatch({
  fnIn <- "trnsport.gdx"
  fnOut <- "wtransport.gdx"
  if (file_test ('-f', fnOut) == TRUE) {
    file.remove (fnOut)
  }
  print (paste("starting",testName))
  # read all the data, using various combination of options
  idf <- rgdx.set(fnIn,'i',ts=TRUE)
  jdf <- rgdx.set(fnIn,'j',names="J",compress=TRUE)
  adf <- rgdx.param(fnIn,'a')
  bdf <- rgdx.param(fnIn,'b', names=c('JJ','theValues'), ts=TRUE)
  cdf <- rgdx.param(fnIn,'c', names=c('from','to'), compress=TRUE, ts=TRUE)
  ddf <- rgdx.param(fnIn,'d')
  fsc <- rgdx.scalar(fnIn,'f')

  wgdx.lst (fnOut, list(idf,jdf,adf,bdf,cdf,ddf,fsc))
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  rc <- system (paste("gdxdiff",fnIn,fnOut,"releps=1e-15 id=i,j,a,b,c,d,f"))
  if (0 != rc) {
    stop(paste("Bad return from gdxdiff: wanted 0, got",rc))
  } else {
    print ("gdxdiff call succeeded")
  }
  if (file_test ('-f', fnOut) == TRUE) {
    file.remove (fnOut)
  }

  # do the write again, but mix in some symLists
  ilst <- list(name='i',type='set',ts='canning plants', uels=list(as.character(idf$i)))
  blst <- list(name='b',type='parameter',ts=attr(bdf,"symName"),
               form="full", uels=list(as.character(bdf$JJ)),
               val=as.array(as.numeric(bdf$theValues)) )
  clst <- list(name="c", type="parameter", ts=attr(cdf,"symName"),
               form="sparse", uels=c(list(levels(cdf$from)),list(levels(cdf$to))),
               val=matrix(data=NA,nrow=nrow(cdf),ncol=ncol(cdf)) )
  clst$val[,1] <- as.numeric(cdf$from)
  clst$val[,2] <- as.numeric(cdf$to)
  clst$val[,3] <- cdf$value
  wgdx.lst (fnOut, list(ilst,jdf,adf,blst,clst,ddf,fsc))
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  rc <- system (paste("gdxdiff",fnIn,fnOut,"releps=1e-15 id=i,j,a,b,c,d,f"))
  if (0 != rc) {
    stop(paste("Bad return from gdxdiff: wanted 0, got",rc))
  } else {
    print ("gdxdiff call succeeded")
  }
  if (file_test ('-f', fnOut) == TRUE) {
    file.remove (fnOut)
  }

  print (paste(testName, ": PASSED"))
  TRUE   ## all tests passed: return TRUE
},

error = errFunc
)
