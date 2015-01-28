## test wgdx on writing a NaN to GDX: should show up as Undf in GDX
## with gdxrrw 0.4.0, this was working with R 3.0.2, failing with R 3.1.2

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

testName <- 'writing R-NaN to GDX'
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
  fnWant <- "tWriteNan.gdx"
  if (! file_test ('-f', fnWant)) {
    stop (paste("FAIL: File-to-duplicate", fnWant, "does not exist"))
  }
  v <- list(name='w',type='parameter',form='full', val=NaN,
            dim=0,ts='R-NaN sent to GDX')
  wgdx.lst(fnOut, v)
  if (file_test ('-f', fnOut) == TRUE) {
    # print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  rc <- system2 ("gdxdiff",args=c(fnWant, fnOut), stdout=logFile)
  if (0 != rc) {
    stop(paste("With val=NaN, bad return from gdxdiff: wanted 0, got",rc))
  } else {
#    print ("gdxdiff call succeeded")
  }

  print (paste0("test of wgdx on ", testName, ": PASSED"))
  suppressWarnings(file.remove(logFile))
  invisible(TRUE)   ## all tests passed: return TRUE
},

error = errFunc
)
