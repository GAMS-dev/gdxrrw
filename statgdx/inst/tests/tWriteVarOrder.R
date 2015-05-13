## ## test wgdx with form='sparse', 2-d variable, when some typical orders are not present
## ##   0. Start with input to wgdx that is identical to what rgdx will give
## ##   1. Usually, index positions that are aliases will use the same order.  But what if not?
## ##   2. the variable fields come out of rgdx() like (1,2,3,4,5) -> ("l","m","lo","up","s")
## ##      We can make this anything though on input, like
## ##        (500,600,700,800,801) -> ('lo','up','s','M','L')
## ##   3. Usually, the 'val' matrix will be ordered.  But it doesn't have to be.
## ##      We randomize it completely and test.

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

testName <- 'unordered variable writes'
logFile <- 'diffLog.txt'
iCard <- 2
fCard <- 5
n <- 10

errFunc <- function(ex) {
  print (paste0("test of wgdx on ",testName,": FAILED"))
  print (paste("Check file", logFile, "for possible gdxdiff output"))
  print (ex)
  FALSE
} # errFunc

tryCatch({
  print (paste("testing wgdx on", testName))
  wgdx('?')
  fnWant <- "tVar2d.gdx"
  fnOut <- "tmp.gdx"
  if (! file_test ('-f', fnWant)) {
    stop (paste("FAIL: File-to-duplicate", fnWant, "does not exist"))
  }

  ## 0. first generate things nice and ordered and simple
  print ("case 0: easy, ordered, simple")
  nrow <- iCard * iCard * fCard
  val0 <- matrix(0,nrow=nrow,ncol=4)
  irow <- 0
  for (i in 1:iCard) {
    for (j in 1:iCard) {

      ## level: v.l(i,j) = n * ord(i) + ord(j);
      lev <- n * i + j
      irow <- irow + 1
      val0[irow,1] <- i
      val0[irow,2] <- j
      val0[irow,3] <- 1
      val0[irow,4] <- lev

      ## marginal: v.m(i,j) = ord(j);
      irow <- irow + 1
      val0[irow,1] <- i
      val0[irow,2] <- j
      val0[irow,3] <- 2
      val0[irow,4] <- j

      ## lo: v.lo(i,j) = v.l(i,j) - 1;
      irow <- irow + 1
      val0[irow,1] <- i
      val0[irow,2] <- j
      val0[irow,3] <- 3
      val0[irow,4] <- lev-1

      ## up: v.up(i,j) = v.l(i,j) * 2;
      irow <- irow + 1
      val0[irow,1] <- i
      val0[irow,2] <- j
      val0[irow,3] <- 4
      val0[irow,4] <- lev*2

      ## scale: v.scale(i,j) = ord(i);
      irow <- irow + 1
      val0[irow,1] <- i
      val0[irow,2] <- j
      val0[irow,3] <- 5
      val0[irow,4] <- i

    }                                   # j loop
  }                                     # i loop
  if (irow != nrow)
    error ("bad test implementation");
  iUels <- c("i1","i2")
  fUels <- c("l", "m", "lo", "up", "s")
  uels0 <- list(iUels,iUels,fUels)

  vOut <- list(name='v',type='variable',val=val0,uels=uels0,varTypeText='free',typeCode=5)
  wgdx (fnOut, vOut)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  rc <- system2 ("gdxdiff",args=c(fnWant, fnOut, "id=v"), stdout=logFile)
  if (0 != rc) {
    stop(paste("Bad return from gdxdiff: wanted 0, got",rc))
  } else {
    print ("gdxdiff call succeeded")
  }

  ## 1. swap order in 2nd dim: 1 -> "i2", 2 -> "i1
  print ("case 1: swap order of UELs in 2nd dim of variable")
  iiUels <- c('i2','i1')
  uels1 <- list(iUels, iiUels, fUels)
  val1 <- val0
  for (irow in 1:nrow) {
    if (1 == val1[irow,2]) {            # was 1 -> 'i1' in iUels
      val1[irow,2] <- 2                 # now 2 -> 'i1' in iiUels
    }
    else if (2 == val1[irow,2]) {       # was 2 -> 'i2' in iUels
      val1[irow,2] <- 1                 # now 1 -> 'i2' in iiUels
    }
    else {
      error ('bad test implementation');
    }
  }
  vOut <- list(name='v',type='variable',val=val1,uels=uels1,varTypeText='free',typeCode=5)
  wgdx (fnOut, vOut)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  rc <- system2 ("gdxdiff",args=c(fnWant, fnOut, "id=v"), stdout=logFile)
  if (0 != rc) {
    stop(paste("Bad return from gdxdiff: wanted 0, got",rc))
  } else {
    print ("gdxdiff call succeeded")
  }
  ## will not work if we adjust vals and not uels
  vOut <- list(name='v',type='variable',val=val1,uels=uels0,varTypeText='free',typeCode=5)
  wgdx (fnOut, vOut)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  rc <- system2 ("gdxdiff",args=c(fnWant, fnOut, "id=v"), stdout=logFile)
  if (0 == rc) {
    stop(paste("Bad return from gdxdiff: wanted nonzero indicating different content"))
  } else {
    print ("gdxdiff call indicated different content as expected")
  }


  ## 2. change field mapping, from
  ##       (1,2,3,4,5) -> ("l","m","lo","up","s")
  ##    to
  ##       (500,600,700,800,801) -> ('lo','up','s','M','L')
  print ("case 2: goofy indexing for 'field' column of $val")
  ffUels <- vector(mode="character",length=1000)
  ffMap <- vector(mode="integer",length=fCard)
  ffUels[500] <- 'lo' ; ffMap[3] <- 500
  ffUels[600] <- 'up' ; ffMap[4] <- 600
  ffUels[700] <- 's'  ; ffMap[5] <- 700
  ffUels[800] <- 'M'  ; ffMap[2] <- 800
  ffUels[801] <- 'L'  ; ffMap[1] <- 801
  uels2 <- list(iUels, iiUels, ffUels)
  val2 <- val1
  for (irow in 1:nrow) {
    val2[irow,3] <- ffMap[val1[irow,3]]
  }

  vOut <- list(name='v',type='variable',val=val2,uels=uels1,varTypeText='free',typeCode=5)
  ## will not work if we adjust vals and not uels, should raise exception
  msg <- "wgdx test with bogus field specifiers should raise exception"
  tcr <- tryCatch({
    wgdx (fnOut, vOut) ; FALSE          # should not complete this branch
  },
    error = function(e) { print(paste(' Caught expected error: msg =',e)) ; TRUE }
  )
  if (tcr) {
    print(paste0(msg,": as expected, passed"))
  }
  else {
    stop (paste0(msg, ": not expected, failed"))
  }
  vOut <- list(name='v',type='variable',val=val2,uels=uels2,varTypeText='free',typeCode=5)
  wgdx (fnOut, vOut)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  rc <- system2 ("gdxdiff",args=c(fnWant, fnOut, "id=v"), stdout=logFile)
  if (0 != rc) {
    stop(paste("Bad return from gdxdiff: wanted 0, got",rc))
  } else {
    print ("gdxdiff call succeeded")
  }

  ##   3. Usually, the 'val' matrix will be ordered.  But it doesn't have to be.
  ##      We randomize it completely and test.
  print ("case 3: permute the rows in the $val matrix from case 2")

  ## first compute a reproducible permutation of 1..nrow
  set.seed (525)
  perm <- sample.int(nrow)
  val3 <- matrix(0,nrow=nrow,ncol=4)

  for (irow in 1:nrow) {
    val3[perm[irow],] <- val2[irow,]
  }  # irow loop
  ## just to try it, we'll make val3 an integer matrix, not a double one
  storage.mode(val3) <- "integer"
  ## if we only change ordering of rows in val, no other change is necessary in vOut
  vOut <- list(name='v',type='variable',val=val3,uels=uels2,varTypeText='free',typeCode=5)
  wgdx (fnOut, vOut)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  rc <- system2 ("gdxdiff",args=c(fnWant, fnOut, "id=v"), stdout=logFile)
  if (0 != rc) {
    stop(paste("Bad return from gdxdiff: wanted 0, got",rc))
  } else {
    print ("gdxdiff call succeeded")
  }

  val3 <- val3 * 0L
  for (irow in 1:nrow) {
    val3[irow,] <- val2[perm[irow],]
  }  # irow loop
  ## just to try it, we'll make val3 an integer matrix, not a double one
  storage.mode(val3) <- "integer"
  ## if we only change ordering of rows in val, no other change is necessary in vOut
  vOut <- list(name='v',type='variable',val=val3,uels=uels2,varTypeText='free',typeCode=5)
  wgdx (fnOut, vOut)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  rc <- system2 ("gdxdiff",args=c(fnWant, fnOut, "id=v"), stdout=logFile)
  if (0 != rc) {
    stop(paste("Bad return from gdxdiff: wanted 0, got",rc))
  } else {
    print ("gdxdiff call succeeded")
  }

  print (paste0("test of wgdx on ", testName, ": PASSED"))
  suppressWarnings(file.remove(logFile))
  invisible(TRUE)   ## all tests passed: return TRUE
},

error = errFunc
)
