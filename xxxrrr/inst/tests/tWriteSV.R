### Test wgdx with special values and zero

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

testName <- "writing special values with form='sparse' and no filter"
logFile <- 'diffLog.txt'

errFunc <- function(ex) {
  print (paste0("test of wgdx on ",testName,": FAILED"))
  print (paste("Check file", logFile, "for possible gdxdiff output"))
  print (ex)
  FALSE
} # errFunc

tryCatch({
  print (paste("testing wgdx on", testName))
  wgdx ('?')

  uels <- list(c('dummy',"R-PInf","R-MInf","R-NaN","R-NA","R-Zero","R-denorm"));
  sv <- matrix(c(1,2, 1,
                 2,2, Inf,
                 1,3, 1,
                 3,3, -Inf,
                 1,4, 1,
                 4,4, NaN,
                 1,5, 1,
                 5,5, NA,
                 1,6, 1,
                 6,6, 0,
                 1,7, 1,
                 7,7, 1e-320),
                 nrow=12,ncol=3,byrow=TRUE);

  svList <- list (name='sv', type='parameter', form='sparse', dim=2,
                  ts='special values with default squeeze="yes"',
                  val=sv, uels=c(uels,uels))

  fn1 <- "wSV1.gdx"
  want1 <- "wSV1_want.gdx"
  print (paste("first write with defaults: output =",fn1));
  wgdx (fn1, svList)

  fn2 <- "wSV2.gdx"
  want2 <- "wSV2_want.gdx"
  print (paste("second write with option squeeze=FALSE: output =",fn2));
  wgdx (fn2, svList, squeeze=FALSE)

  fn3 <- "wSV3.gdx"
  want3 <- "wSV3_want.gdx"
  print (paste("third write with option squeeze='eps': output =",fn3));
  wgdx (fn3, svList, squeeze='Eps')


  if (file_test ('-f', fn1) == TRUE) {
    print (paste("File", fn1, "was created"))
  } else {
    stop (paste("FAIL: File", fn1, "is not readable"))
  }
  rc <- system2 ("gdxdiff",args=c(fn1, want1), stdout=logFile)
  if (0 != rc) {
    stop(paste("Bad return from gdxdiff: wanted 0, got",rc))
  } else {
    print (paste("gdxdiff call succeeded, file",fn1,"is OK"))
  }

  if (file_test ('-f', fn2) == TRUE) {
    print (paste("File", fn2, "was created"))
  } else {
    stop (paste("FAIL: File", fn2, "is not readable"))
  }
  rc <- system2 ("gdxdiff",args=c(fn2, want2), stdout=logFile)
  if (0 != rc) {
    stop(paste("Bad return from gdxdiff: wanted 0, got",rc))
  } else {
    print (paste("gdxdiff call succeeded, file",fn2,"is OK"))
  }

  if (file_test ('-f', fn3) == TRUE) {
    print (paste("File", fn3, "was created"))
  } else {
    stop (paste("FAIL: File", fn3, "is not readable"))
  }
  rc <- system2 ("gdxdiff",args=c(fn3, want3), stdout=logFile)
  if (0 != rc) {
    stop(paste("Bad return from gdxdiff: wanted 0, got",rc))
  } else {
    print (paste("gdxdiff call succeeded, file",fn3,"is OK"))
  }

  print (paste0("test of wgdx on ", testName, ": PASSED"))
  suppressWarnings(file.remove(logFile))
  invisible(TRUE)   ## all tests passed: return TRUE
},

error = errFunc
)
