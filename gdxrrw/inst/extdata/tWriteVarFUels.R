## ## test wgdx with 1-d variable and duplicate labels for the "field" dimension

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

testName <- 'duplicate field labels in variable writes'

errFunc <- function(ex) {
  print (paste("test of wgdx on",testName,"failed"))
  print(ex)
  FALSE
} # errFunc

tryCatch({
  print (paste("testing wgdx on", testName))
  wgdx('?')
  fnOut <- "tmp.gdx"
  fnWant <- "tVarTypes.gdx"
  if (! file_test ('-f', fnWant)) {
    stop (paste("FAIL: File-to-duplicate", fnWant, "does not exist"))
  }

  ## all writes should use the same uels
  iUels <- paste0("i","1":"10")
  fUels <- c("l", "m", "lo", "up", "s", "ignored") # this is potentially OK
  uels <- list(iUels,fUels)
  fxUels <- c("l", "m", "lo", "up", "s", "m") # this is potentially OK too
  xuels <- list(iUels,fxUels)

  ## write binary variable to GDX, compare with fnWant version
  val0 <- matrix(0,nrow=5,ncol=3)
  for (i in 1:5) {
    val0[i,1] <- i
    val0[i,2] <- i
  }
  val0[1,3] <- 1                        # binary.L = 1
  val0[2,3] <- 0.5                      # binary.m = 0.5
  val0[3,3] <- 1                        # binary.lo = 1
  val0[4,3] <- 0                        # binary.up = 0
  val0[5,3] <- 10                       # binary.prior = 10
  valBinary <- val0
  vBinary <- list(name='v_binary',type='variable',val=valBinary,uels=uels,
                  typeCode=GMS_VARTYPE$BINARY,ts='text for v_binary')
  wgdx (fnOut, vBinary)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  vWant <- rgdx(fnWant,list(name='v_binary',form='sparse',field='all',ts=TRUE))
  vWrote <- rgdx(fnOut,list(name='v_binary',form='sparse',field='all',ts=TRUE))
  if (identical(vWrote$domains[1],"*"))
    vWrote$domains[1] <- vWant$domains[1]
  if (identical(vWant,vWrote)) {
  }
  else {
    print(all.equal(vWant,vWrote))
    stop ("FAIL: for v_binary, vWant and vWrote do not agree")
  }

  ## this is OK since we don't use the duplicate
  vBinary$uels <- xuels
  wgdx (fnOut, vBinary)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  vWant <- rgdx(fnWant,list(name='v_binary',form='sparse',field='all',ts=TRUE))
  vWrote <- rgdx(fnOut,list(name='v_binary',form='sparse',field='all',ts=TRUE))
  if (identical(vWrote$domains[1],"*"))
    vWrote$domains[1] <- vWant$domains[1]
  if (identical(vWant,vWrote)) {
  }
  else {
    print(all.equal(vWant,vWrote))
    stop ("FAIL: for v_binary, vWant and vWrote do not agree")
  }

  ## this should break since we have duplicate "m" labels used
  vBinary$val[5,1] <- 5                 # 'i5'
  vBinary$val[5,2] <- 6                 # the duplicate 'm'
  vBinary$val[5,3] <- 0
  msg <- "wgdx test for writing variables, duplicate field labels used"
  tcr <- tryCatch({
    wgdx (fnOut, vBinary) ; FALSE
  },
    error = function(e) { print(paste(' Caught error: msg =',e)) ; TRUE }
  )
  if (tcr) {
    print(paste(msg,": passed",sep=""))
  }
  else {
    stop (paste(msg, ": failed",sep=""))
  }

  print (paste("test of wgdx on", testName, "passed"))
  TRUE   ## all tests passed: return TRUE
},

error = errFunc
)
