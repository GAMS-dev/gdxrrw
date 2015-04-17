## ## test wgdx with form='sparse', 1-d variable, write different types
## gdxdump or gdxdiff do not really do what we want here, so just have
## to read from the generated GDX and the target GDX and compare the results

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

testName <- 'all types of variable writes with form=sparse'

errFunc <- function(ex) {
  print (paste0("test of wgdx on ",testName,": FAILED"))
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
  val0[5,3] <- 10                       # binary.prior = 10 stored in scale
  valBinary <- val0
  vBinary <- list(name='v_binary',type='variable',
                  val=valBinary,uels=uels,domains='i',
                  typeCode=GMS_VARTYPE$BINARY,ts='text for v_binary')
  wgdx (fnOut, vBinary)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }

  vWant <- rgdx(fnWant,list(name='v_binary',form='sparse',field='all',ts=TRUE))
  vWrote <- rgdx(fnOut,list(name='v_binary',form='sparse',field='all',ts=TRUE))
  if (identical(vWrote$domInfo,"relaxed"))
    vWrote$domInfo <- "full"
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
  val0[5,3] <- 1000                     # integer.prior = 1000 stored in scale
  valInteger <- val0
  vInteger <- list(name='v_integer',type='variable',
                   val=valInteger,uels=uels,domains='i',
                   typeCode=GMS_VARTYPE$INTEGER,ts='CHECK CASING')
  wgdx (fnOut, vInteger)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }

  vWant <- rgdx(fnWant,list(name='v_integer',form='sparse',field='all',ts=TRUE))
  vWrote <- rgdx(fnOut,list(name='v_integer',form='sparse',field='all',ts=TRUE))
  if (identical(vWrote$domInfo,"relaxed"))
    vWrote$domInfo <- "full"
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
  vPositive <- list(name='v_positive',type='variable',
                    val=valPositive,uels=uels,domains='i',
                    typeCode=GMS_VARTYPE$POSITIVE,ts='try some special values')
  wgdx (fnOut, vPositive)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }

  vWant <- rgdx(fnWant,list(name='v_positive',form='sparse',field='all',ts=TRUE))
  vWrote <- rgdx(fnOut,list(name='v_positive',form='sparse',field='all',ts=TRUE))
  if (identical(vWrote$domInfo,"relaxed"))
    vWrote$domInfo <- "full"
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
  val0[2,3] <- -4                       # negative('i4').m
  val0[3,3] <- 1                        # negative('i3').lo
  val0[4,3] <- -1                       # negative('i2').up
  val0[5,3] <- 0.5                      # negative('i1').scale
  valNegative <- val0
  vNegative <- list(name='v_negative',type='variable',
                    val=valNegative,uels=uels,domains='i',
                    typeCode=GMS_VARTYPE$NEGATIVE,ts='')
  wgdx (fnOut, vNegative)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }

  vWant <- rgdx(fnWant,list(name='v_negative',form='sparse',field='all',ts=TRUE))
  vWrote <- rgdx(fnOut,list(name='v_negative',form='sparse',field='all',ts=TRUE))
  if (identical(vWrote$domInfo,"relaxed"))
    vWrote$domInfo <- "full"
  if (identical(vWant,vWrote)) {
  }
  else {
    print(all.equal(vWant,vWrote))
    stop ("FAIL: for v_negative, vWant and vWrote do not agree")
  }

  ## write free variable to GDX, compare with fnWant version
  val0 <- matrix(0,nrow=5,ncol=3)
  for (i in 1:5) {
    val0[i,1] <- i
    val0[i,2] <- i
  }
  val0[1,3] <- -12.5                    # free.L
  val0[2,3] <- 0.5                      # free.m
  val0[3,3] <- 3                        # free.lo
  val0[4,3] <- -8                       # free.up
  val0[5,3] <- 512                      # free.scale
  valFree <- val0
  vFree <- list(name='v_free',type='variable',
                val=valFree,uels=uels,domains='i',
                typeCode=GMS_VARTYPE$FREE,
                ts='WHAT if we use a long text with special chars __ %% -- @@  ?? !! jj')
  wgdx (fnOut, vFree)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }

  vWant <- rgdx(fnWant,list(name='v_free',form='sparse',field='all',ts=TRUE))
  vWrote <- rgdx(fnOut,list(name='v_free',form='sparse',field='all',ts=TRUE))
  if (identical(vWrote$domInfo,"relaxed"))
    vWrote$domInfo <- "full"
  if (identical(vWant,vWrote)) {
  }
  else {
    print(all.equal(vWant,vWrote))
    stop ("FAIL: for v_free, vWant and vWrote do not agree")
  }

  ## write sos1 variable to GDX, compare with fnWant version
  val0 <- matrix(0,nrow=5,ncol=3)
  for (i in 1:5) {
    val0[i,1] <- i
    val0[i,2] <- i
  }
  val0[1,3] <- 123                      # sos1.L
  val0[2,3] <- -10.5                    # sos1.m
  val0[3,3] <- 2                        # sos1.lo
  val0[4,3] <- 10                       # sos1.up
  val0[5,3] <- 100                      # sos1.prior
  valSos1 <- val0
  vSos1 <- list(name='v_sos1',type='variable',
                val=valSos1,uels=uels,domains='i',
                typeCode=GMS_VARTYPE$SOS1,ts='v_sos1')
  wgdx (fnOut, vSos1)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }

  vWant <- rgdx(fnWant,list(name='v_sos1',form='sparse',field='all',ts=TRUE))
  vWrote <- rgdx(fnOut,list(name='v_sos1',form='sparse',field='all',ts=TRUE))
  if (identical(vWrote$domInfo,"relaxed"))
    vWrote$domInfo <- "full"
  if (identical(vWant,vWrote)) {
  }
  else {
    print(all.equal(vWant,vWrote))
    stop ("FAIL: for v_sos1, vWant and vWrote do not agree")
  }

  ## write sos2 variable to GDX, compare with fnWant version
  val0 <- matrix(0,nrow=5,ncol=3)
  for (i in 1:5) {
    val0[i,1] <- i
    val0[i,2] <- i
  }
  val0[1,3] <- 123                      # sos2.L
  val0[2,3] <- -10.5                    # sos2.m
  val0[3,3] <- 2                        # sos2.lo
  val0[4,3] <- 10                       # sos2.up
  val0[5,3] <- 100                      # sos2.prior
  valSos2 <- val0
  vSos2 <- list(name='v_sos2',type='variable',
                val=valSos2,uels=uels,domains='i',
                typeCode=GMS_VARTYPE$SOS2,ts='v_sos2')
  wgdx (fnOut, vSos2)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }

  vWant <- rgdx(fnWant,list(name='v_sos2',form='sparse',field='all',ts=TRUE))
  vWrote <- rgdx(fnOut,list(name='v_sos2',form='sparse',field='all',ts=TRUE))
  if (identical(vWrote$domInfo,"relaxed"))
    vWrote$domInfo <- "full"
  if (identical(vWant,vWrote)) {
  }
  else {
    print(all.equal(vWant,vWrote))
    stop ("FAIL: for v_sos2, vWant and vWrote do not agree")
  }

  ## write semicont variable to GDX, compare with fnWant version
  val0 <- matrix(0,nrow=5,ncol=3)
  for (i in 1:5) {
    val0[i,1] <- i
    val0[i,2] <- i
  }
  val0[1,3] <- 10.5                     # semicont.L
  val0[2,3] <- .875                     # semicont.m
  val0[3,3] <- 13                       # semicont.lo
  val0[4,3] <- 1000                     # semicont.up
  val0[5,3] <- 999                      # semicont.prior
  valSemicont <- val0
  vSemicont <- list(name='v_semicont',type='variable',
                    val=valSemicont,uels=uels,domains='i',
                    typeCode=GMS_VARTYPE$SEMICONT,ts='v_semicont')
  wgdx (fnOut, vSemicont)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }

  vWant <- rgdx(fnWant,list(name='v_semicont',form='sparse',field='all',ts=TRUE))
  vWrote <- rgdx(fnOut,list(name='v_semicont',form='sparse',field='all',ts=TRUE))
  if (identical(vWrote$domInfo,"relaxed"))
    vWrote$domInfo <- "full"
  if (identical(vWant,vWrote)) {
  }
  else {
    print(all.equal(vWant,vWrote))
    stop ("FAIL: for v_semicont, vWant and vWrote do not agree")
  }

  ## write semiint variable to GDX, compare with fnWant version
  val0 <- matrix(0,nrow=5,ncol=3)
  for (i in 1:5) {
    val0[i,1] <- i
    val0[i,2] <- i
  }
  val0[1,3] <- 10.5                     # semiint.L
  val0[2,3] <- .875                     # semiint.m
  val0[3,3] <- 13                       # semiint.lo
  val0[4,3] <- 1000                     # semiint.up
  val0[5,3] <- 100000                   # semiint.prior
  valSemiint <- val0
  vSemiint <- list(name='v_semiint',type='variable',
                   val=valSemiint,uels=uels,domains='i',
                   typeCode=GMS_VARTYPE$SEMIINT,ts='v_semiint text')
  wgdx (fnOut, vSemiint)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }

  vWant <- rgdx(fnWant,list(name='v_semiint',form='sparse',field='all',ts=TRUE))
  vWrote <- rgdx(fnOut,list(name='v_semiint',form='sparse',field='all',ts=TRUE))
  if (identical(vWrote$domInfo,"relaxed"))
    vWrote$domInfo <- "full"
  if (identical(vWant,vWrote)) {
  }
  else {
    print(all.equal(vWant,vWrote))
    stop ("FAIL: for v_semiint, vWant and vWrote do not agree")
  }


  print (paste0("test of wgdx on ", testName, ": PASSED"))
  invisible(TRUE)   ## all tests passed: return TRUE
},

error = errFunc
)
