## ## test wgdx.lst wrapper on dplyr table input

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

testName <- 'writing from dplyr tables'
logFile <- 'diffLog.txt'
 
errFunc <- function(ex) {
  print (paste0("test of wgdx on ",testName,": FAILED"))
  print (paste("Check file", logFile, "for possible gdxdiff output"))
  print(ex)
  FALSE
} # errFunc

suppressWarnings(ok <- library(dplyr,logical.return=T))
if (!ok) {
  print (paste0("test of wgdx on ", testName, ": SKIPPED"))
  invisible(TRUE)
} else {
tryCatch({
  print (paste("testing wgdx on", testName))
  wgdx('?')
  fnDF <- 'junkDF.gdx'
  fnTBL <- 'junkTBL.gdx'

  tdf <- data.frame(
    name=factor(c('Wietse','Foppe','Gea','Wietse')),
    year=factor(c(2016,2015,2014,2020)),
    value=c(100,99,98,112)
  )

  attr(tdf,'symName') <- 't';
  attr(tdf,'ts') <- 'test of tbl input';
  attr(tdf,'domains') <- names(tdf)[1:(ncol(tdf)-1)]
  str(tdf)

  sdf <- tdf[c(1,2)]
  attr(sdf,'symName') <- 's';
  str(sdf)

  ssdf <- sdf
  ssdf[[3]] <- c('text one', 'text 2', 'text III', 'text vier')
  attr(ssdf,'symName') <- 'ss';
  str(ssdf)

  wgdx.lst(fnDF, list(tdf,sdf,ssdf))
  if (file_test ('-f', fnDF) == TRUE) {
    print (paste("File", fnDF, "was created"))
  } else {
    stop (paste("FAIL: File", fnDF, "is not readable"))
  }

  ttbl <- as.tbl(tdf)
  str(ttbl)
  stbl <- as.tbl(sdf)
  str(stbl)
  sstbl <- as.tbl(ssdf)
  str(sstbl)

  wgdx.lst(fnTBL, list(ttbl,stbl,sstbl))
  if (file_test ('-f', fnTBL) == TRUE) {
    print (paste("File", fnTBL, "was created"))
  } else {
    stop (paste("FAIL: File", fnTBL, "is not readable"))
  }
  rc <- system2 ("gdxdiff",args=c(fnDF, fnTBL), stdout=logFile)
  if (0 != rc) {
    stop(paste("Bad return from gdxdiff: wanted 0, got",rc))
  } else {
    print ("gdxdiff call succeeded")
  }

  print (paste0("test of wgdx on ", testName, ": PASSED"))
  invisible(TRUE)   ## all tests passed: return TRUE
},

error = errFunc
)
}
