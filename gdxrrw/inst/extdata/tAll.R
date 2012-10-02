### run through all the tests

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

tests <- c("tReadSparse1", "tReadFull1",
           "tRead.set", "tRead.param",
           "tReadSV", "tReadText",
           "tReadErr",
           "tReadWithDomainInfo", "tReadPreDomainInfo",
           "tReadCompressWithoutDomInfo", "tReadCompressWithDomInfo",
           "tWriteSparse1", "tWriteSparse2", "tWriteFull1", "tWriteFull2",
           "tWriteSV",
           "tWrap",
           "tInfo1", "tInfo2",
           "tLS")
f <- find('interact',mode='logical')
doPrompt <- FALSE
if (length(f) > 0) {
  doPrompt <- interact
} else {
  doPrompt <- interactive()
}

nRuns <- 0
nFails  <- 0
for (t in tests) {
  print (paste("Starting test", t))
  rc <- source (paste(t,".R",sep=""))
  if (rc$value) {
    print (paste("Test", t, "result: PASS"))
  } else {
    print (paste("Test", t, "result: FAIL"))
    nFails  <- nFails + 1
  }
  nRuns  <- nRuns + 1
  if (doPrompt) {
    ans <- readline("Hit enter to continue ")
    print ("")
  }
}
if (nFails) {
  print (paste("Tests complete. ", nRuns,"tests run,",nFails,"tests FAILED"))
} else {
  print (paste("Testing complete.  All", nRuns,"tests PASSED"))
}
