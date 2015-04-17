## ## test wgdx with form='full', 1-d variable, write different types
## gdxdump or gdxdiff do not really do what we want here, so just have
## to read from the generated GDX and the target GDX and compare the results

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

testName <- 'all types of variable writes with form=full'

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
  nrow <- length(iUels)
  ncol <- length(fUels)

  val0 <- matrix(0,nrow=nrow,ncol=ncol)
  dimnames(val0) <- uels

  ## write binary variable to GDX, compare with fnWant version
  ## defaults first
  val0[,'up'] <- 1
  val0[,'s'] <- 1
  val0['i1','l'] <- 1
  val0['i2','m'] <- 0.5
  val0['i3','lo'] <- 1
  val0['i4','up'] <- 0
  val0['i5','s'] <- 10
  valBinary <- val0
  vBinary <- list(name='v_binary',type='variable',form='full',val=valBinary,
                  uels=uels,domains='i',
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
  val0[,] <- 0
  ## nonzero defaults first
  val0[,'up'] <- Inf
  val0[,'s'] <- 1
  val0['i1','l'] <- 1.5
  val0['i2','m'] <- 0.5
  val0['i3','lo'] <- 3
  val0['i4','up'] <- 16
  val0['i5','s'] <- 1000
  valInteger <- val0
  vInteger <- list(name='v_integer',type='variable',form='full',val=valInteger,
                   uels=uels,domains='i',
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
  val0[,] <- 0
  ## nonzero defaults first
  val0[,'up'] <- Inf
  val0[,'s'] <- 1
  val0['i5','l'] <- 322
  val0['i4','m'] <- -1024
  val0['i3','lo'] <- -Inf
  val0['i2','up'] <- 0
  val0['i1','s'] <- 10
  valPositive <- val0
  vPositive <- list(name='v_positive',type='variable',form='full',val=valPositive,
                    uels=uels,domains='i',
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
  val0[,] <- 0
  ## nonzero defaults first
  val0[,'lo'] <- -Inf
  val0[,'s'] <- 1
  val0['i5','l'] <- 525
  val0['i4','m'] <- -4
  val0['i3','lo'] <- 1
  val0['i2','up'] <- -1
  val0['i1','s'] <- 0.5
  valNegative <- val0
  vNegative <- list(name='v_negative',type='variable',form='full',val=valNegative,
                    uels=uels,domains='i',
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
  val0[,] <- 0
  ## nonzero defaults first
  val0[,'lo'] <- -Inf
  val0[,'up'] <-  Inf
  val0[,'s'] <- 1
  val0['i1','l'] <- -12.5
  val0['i2','m'] <- 0.5
  val0['i3','lo'] <- 3
  val0['i4','up'] <- -8
  val0['i5','s'] <- 512
  valFree <- val0
  vFree <- list(name='v_free',type='variable',form='full',val=valFree,
                uels=uels,domains='i',
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
  val0[,] <- 0
  ## nonzero defaults first
  val0[,'up'] <-  Inf
  val0[,'s'] <- 1
  val0['i1','l'] <- 123
  val0['i2','m'] <- -10.5
  val0['i3','lo'] <- 2
  val0['i4','up'] <- 10
  val0['i5','s'] <- 100
  valSos1 <- val0
  vSos1 <- list(name='v_sos1',type='variable',form='full',val=valSos1,
                uels=uels,domains='i',
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
  val0[,] <- 0
  ## nonzero defaults first
  val0[,'up'] <-  Inf
  val0[,'s'] <- 1
  val0['i1','l'] <- 123
  val0['i2','m'] <- -10.5
  val0['i3','lo'] <- 2
  val0['i4','up'] <- 10
  val0['i5','s'] <- 100
  valSos2 <- val0
  vSos2 <- list(name='v_sos2',type='variable',form='full',val=valSos2,
                uels=uels,domains='i',
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
  val0[,] <- 0
  ## nonzero defaults first
  val0[,'lo'] <-  1
  val0[,'up'] <-  Inf
  val0[,'s'] <- 1
  val0['i1','l'] <- 10.5
  val0['i2','m'] <- .875
  val0['i3','lo'] <- 13
  val0['i4','up'] <- 1000
  val0['i5','s'] <- 999
  valSemicont <- val0
  vSemicont <- list(name='v_semicont',type='variable',form='full',val=valSemicont,
                    uels=uels,domains='i',
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
  val0[,] <- 0
  ## nonzero defaults first
  val0[,'lo'] <- 1
  val0[,'up'] <- Inf
  val0[,'s'] <- 1
  val0['i1','l'] <- 10.5
  val0['i2','m'] <- .875
  val0['i3','lo'] <- 13
  val0['i4','up'] <- 1000
  val0['i5','s'] <- 100000
  valSemiint <- val0
  vSemiint <- list(name='v_semiint',type='variable',form='full',val=valSemiint,
                   uels=uels,domains='i',
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
