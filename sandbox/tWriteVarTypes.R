## ## test wgdx with form='sparse', 1-d variable, write different types
## gdxdump or gdxdiff do not really do what we want here, so just have
## to read from the generated GDX and the target GDX and compare the results

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

testName <- 'all types of variable writes'

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
  fUels <- c("l", "m", "lo", "up", "s")
  uels <- list(iUels,fUels)

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
  val0[5,3] <- 1                        # binary.scale = 1, no choice
  valBinary <- val0
  vBinary <- list(name='v_binary',type='variable',val=valBinary,uels=uels,typeCode=1,ts='text for v_binary')
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

  ## write integer variable to GDX, compare with fnWant version
  val0 <- matrix(0,nrow=5,ncol=3)
  for (i in 1:5) {
    val0[i,1] <- i
    val0[i,2] <- i
  }
  val0[1,3] <- 1.5                      # integer.L = 1.5
  val0[2,3] <- 0.5                      # integer.m = 0.5
  val0[3,3] <- 3                        # integer.lo = 1
  val0[4,3] <- 16                       # integer.up = 0
  val0[5,3] <- 1                        # integer.scale = 1, no choice, so try .up = 0
  valInteger <- val0
  vInteger <- list(name='v_integer',type='variable',val=valInteger,uels=uels,typeCode=2,ts='CHECK CASING')
  wgdx (fnOut, vInteger)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }

  vWant <- rgdx(fnWant,list(name='v_integer',form='sparse',field='all',ts=TRUE))
  vWrote <- rgdx(fnOut,list(name='v_integer',form='sparse',field='all',ts=TRUE))
  if (identical(vWrote$domains[1],"*"))
    vWrote$domains[1] <- vWant$domains[1]
  if (identical(vWant,vWrote)) {
  }
  else {
    print(all.equal(vWant,vWrote))
    stop ("FAIL: for v_integer, vWant and vWrote do not agree")
  }

  ## write positive variable to GDX, compare with fnWant version
  val0 <- matrix(0,nrow=5,ncol=3)
  for (i in 1:5) {
    val0[i,1] <- i
    val0[i,2] <- 6-i
  }
  val0[5,3] <- 322                      # positive.L
  val0[4,3] <- -1024                    # positive.m
  val0[3,3] <- -Inf                     # positive.lo
  val0[2,3] <- 0                        # positive.up
  val0[1,3] <- 10                       # positive.scale
  valPositive <- val0
  vPositive <- list(name='v_positive',type='variable',val=valPositive,uels=uels,typeCode=3,ts='try some special values')
  wgdx (fnOut, vPositive)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }

  vWant <- rgdx(fnWant,list(name='v_positive',form='sparse',field='all',ts=TRUE))
  vWrote <- rgdx(fnOut,list(name='v_positive',form='sparse',field='all',ts=TRUE))
  if (identical(vWrote$domains[1],"*"))
    vWrote$domains[1] <- vWant$domains[1]
  if (identical(vWant,vWrote)) {
  }
  else {
    print(all.equal(vWant,vWrote))
    stop ("FAIL: for v_positive, vWant and vWrote do not agree")
  }

  ## write negative variable to GDX, compare with fnWant version
  val0 <- matrix(0,nrow=5,ncol=3)
  ## this is a confusing order, tested so intentionally
  for (i in 1:5) {
    val0[i,1] <- 6-i
    val0[i,2] <- i
  }
  val0[1,3] <- 525                      # negative('i5').L
  val0[2,3] <- 0                        # negative('i4').up
  val0[3,3] <- 1                        # negative('i3').lo
  val0[4,3] <- -1                       # negative('i2').up
  val0[5,3] <- 0.5                      # negative('i1').scale
  valNegative <- val0
  vNegative <- list(name='v_negative',type='variable',val=valNegative,uels=uels,typeCode=4,ts='')
  wgdx (fnOut, vNegative)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }

  vWant <- rgdx(fnWant,list(name='v_negative',form='sparse',field='all',ts=TRUE))
  vWrote <- rgdx(fnOut,list(name='v_negative',form='sparse',field='all',ts=TRUE))
  if (identical(vWrote$domains[1],"*"))
    vWrote$domains[1] <- vWant$domains[1]
  if (identical(vWant,vWrote)) {
  }
  else {
    print(all.equal(vWant,vWrote))
    stop ("FAIL: for v_negative, vWant and vWrote do not agree")
  }


  print (paste("test of wgdx on", testName, "passed"))
  TRUE   ## all tests passed: return TRUE
},

error = errFunc
)
