## test wgdx on writing set text
## gdxdump or gdxdiff do not really do what we want here, so just have
## to read from the generated GDX and the target GDX and compare the results
## for I and IJ

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

testName <- 'writing set text'
logFile <- 'diffLog.txt'
redirect <- paste(' >', logFile)

errFunc <- function(ex) {
  print (paste0("test of wgdx on ",testName,": FAILED"))
  print (paste("Check file", logFile, "for possible gdxdiff output"))
  print(ex)
  FALSE
} # errFunc

tryCatch({
  print (paste("testing wgdx on", testName))
  wgdx('?')
  fnOut <- "tmp.gdx"
  fnWant <- "teWriteSetText.gdx"
  fnWant2 <- "teWriteSetTextAlt.gdx"
  if (! file_test ('-f', fnWant)) {
    stop (paste("FAIL: File-to-duplicate", fnWant, "does not exist"))
  }

  iUels <- paste0("i","1":"4")
  iN <- length(iUels)
  jUels <- paste0("j","1":"3")
  jN <- length(jUels)
  ijN <- 5
  uels <- list(c(iUels,jUels))
  valI <- matrix(0,nrow=iN,ncol=1)
  for (k in 1:iN) {
    valI[k,1] <- k
  }
  teI <- matrix(NA_character_,nrow=iN,ncol=1)
  teI[1,1] <- "i1 associated text"
  teI[2,1] <- "i2's text here"
  teI[3,1] <- "  "
  teI0 <- c("i1 associated text", "i2's text here", "  ", NA_character_)

  valJ <- matrix(0,nrow=jN,ncol=1)
  for (k in 1:jN) {
    valJ[k,1] <- iN + k
  }
  teJ <- matrix(NA_character_,nrow=jN,ncol=1)
  teJ[1,1] <- "j1 text"
  teJ[2,1] <- "j2 text"
  teJ[3,1] <- "j3 text"

  valIJ <- matrix(c(1,1,
                    2,1,
                    2,2,
                    3,3,
                    4,3),
                  nrow=ijN,ncol=2,byrow=TRUE)
  teIJ <- matrix(NA_character_,nrow=ijN,ncol=1)
  teIJ[1,1] <- "one.one"
  teIJ[3,1] <- "trailing blank "
  teIJ[4,1] <- ""
  teIJ[5,1] <- " "
  teIJ0 <- c('one.one', NA_character_, 'trailing blank ', '', ' ')

  vI <- list(name='I',type='set',form='sparse',val=valI,uels=uels,
             ts='',te=teI)
  vJ <- list(name='J',type='set',form='sparse',val=valJ,uels=uels,
             ts='',te=teJ)
  vIJ <- list(name='IJ',type='set',form='sparse',val=valIJ,uels=list(iUels,jUels),
              ts='',te=teIJ)

  ## test with inventSetText at default
  options(gdx.inventSetText=NULL)
  wgdx.lst(fnOut, vI, vJ, vIJ)
  if (file_test ('-f', fnOut) == TRUE) {
    # print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  rc <- system (paste("gdxdiff", fnWant, fnOut, redirect))
  if (0 != rc) {
    stop(paste("With gdx.inventSetText=NULL, bad return from gdxdiff: wanted 0, got",rc))
  } else {
#    print ("gdxdiff call succeeded")
  }
  ## no need to test via wgdx: will get done below

  ## test with inventSetText=NA
  options(gdx.inventSetText=NA)
  wgdx.lst(fnOut, vI, vJ, vIJ)
  if (file_test ('-f', fnOut) == TRUE) {
    # print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  rc <- system (paste("gdxdiff", fnWant, fnOut, redirect))
  if (0 != rc) {
    stop(paste("With gdx.inventSetText=NA, Bad return from gdxdiff: wanted 0, got",rc))
  } else {
#    print ("gdxdiff call succeeded")
  }
  ## gdxdiff does not differentiate between no set text and
  ## empty set text, so we test that explicitly here
  I1 <- rgdx(fnOut,   list(name='I',form='sparse',te=TRUE))
  teI1 <- teI0
  if (! identical(teI1,I1$te)) {
    stop (paste('With gdx.inventSetText=NA, inconsistent set text for I in file',fnOut))
  }
  IJ1 <- rgdx(fnOut,   list(name='IJ',form='sparse',te=TRUE))
  teIJ1 <- teIJ0
  if (! identical(teIJ1,IJ1$te)) {
    stop (paste('With gdx.inventSetText=NA, inconsistent set text for IJ in file',fnOut))
  }

  ## test with inventSetText=FALSE
  options(gdx.inventSetText=F)
  wgdx.lst(fnOut, vI, vJ, vIJ)
  if (file_test ('-f', fnOut) == TRUE) {
    # print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  rc <- system (paste("gdxdiff", fnWant2, fnOut, redirect))
  if (0 != rc) {
    stop(paste("With gdx.inventSetText=F, Bad return from gdxdiff: wanted 0, got",rc))
  } else {
    # print ("gdxdiff call succeeded")
  }
  ## gdxdiff does not differentiate between no set text and
  ## empty set text, so we test that explicitly here
  options(gdx.inventSetText=NA)
  I1 <- rgdx(fnOut,   list(name='I',form='sparse',te=TRUE))
  teI1 <- teI0
  if (! identical(teI1,I1$te)) {
    stop (paste('With gdx.inventSetText=F, inconsistent set text for I in file',fnOut))
  }
  ## with inventSetText=F, fnOut should store empty text strings like no string
  ## and this should come back as NA when we read
  IJ1 <- rgdx(fnOut,   list(name='IJ',form='sparse',te=TRUE))
  teIJ1 <- teIJ0
  teIJ1[4] <- NA_character_
  if (! identical(teIJ1,IJ1$te)) {
    stop (paste('JJJ With gdx.inventSetText=F, inconsistent set text for IJ in file',fnOut))
  }

  ## test with inventSetText=TRUE
  options(gdx.inventSetText=T)
  wgdx.lst(fnOut, vI, vJ, vIJ)
  if (file_test ('-f', fnOut) == TRUE) {
    # print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  rc <- system (paste("gdxdiff", fnWant, fnOut, redirect))
  if (0 != rc) {
    stop(paste("With gdx.inventSetText=T, Bad return from gdxdiff: wanted 0, got",rc))
  } else {
    # print ("gdxdiff call succeeded")
  }
  ## gdxdiff does not differentiate between no set text and
  ## empty set text, so we test that explicitly here
  options(gdx.inventSetText=NA)
  I1 <- rgdx(fnOut,   list(name='I',form='sparse',te=TRUE))
  teI1 <- teI0
  if (! identical(teI1,I1$te)) {
    stop (paste('With gdx.inventSetText=NA, inconsistent set text for I in file',fnOut))
  }
  IJ1 <- rgdx(fnOut,   list(name='IJ',form='sparse',te=TRUE))
  teIJ1 <- teIJ0
  if (! identical(teIJ1,IJ1$te)) {
    stop (paste('With gdx.inventSetText=NA, inconsistent set text for IJ in file',fnOut))
  }

  print (paste0("test of wgdx on ", testName, ": PASSED"))
  suppressWarnings(file.remove(logFile))
  invisible(TRUE)   ## all tests passed: return TRUE
},

error = errFunc
)
