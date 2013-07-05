### run through all the tests

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

gdir <- igdx(NULL,silent=TRUE,returnStr=TRUE)
pth <- Sys.getenv('PATH')
sepChar <- ':'
if (Sys.info()[[1]] == "Windows") sepChar <- ';'
Sys.setenv(PATH=paste(gdir,pth,sep=sepChar))

rc <- source ("tgdxdiff.R")
if (! rc$value)  stop ("The gdxdiff utility is unavailable or broken: Test run aborted.")

tests <- c("tReadSparse1", "tReadFull1",
           "tRead.set", "tRead.param",
           "tReadSV", "tReadText", "tReadCase",
           "tReadErr",
           "tReadWithDomainInfo", "tReadPreDomainInfo",
           "tReadCompressWithoutDomInfo", "tReadCompressWithDomInfo",
           "tReadVar0", "tReadVar1", "tReadVar3",
           "tReadEqu",
           "tWriteSparse1", "tWriteSparse2", "tWriteFull1", "tWriteFull2",
           "tWriteSV",
           "tWriteEquTypes",
           "tWriteVarField", "tWriteVarSort", "tWriteVarOrder",
           "tWriteVarTypesSparse", "tWriteVar20", "tWriteVarFUels",
           "tWriteVarTypesFull",
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
fTests <- list()
for (ttt in tests) {
  print (paste("Starting test", ttt))
  rc <- source (paste(ttt,".R",sep=""))
  if (rc$value) {
    print (paste("Test", ttt, "result: PASS"))
  } else {
    print ("WTF: what is ttt??")
    print (ttt)
    print (paste("Test", ttt, "result: FAIL"))
    nFails  <- nFails + 1
    fTests[[ttt]] <- T
  }
  nRuns  <- nRuns + 1
  if (doPrompt) {
    ans <- readline("Hit enter to continue ")
    print ("")
  }
}
Sys.setenv(PATH=pth)
if (nFails) {
  print (paste("Tests complete. ", nRuns,"tests run,",nFails,"tests FAILED"))
  for (nm in names(fTests)) print(paste0('  ',nm))
} else {
  print (paste("Testing complete.  All", nRuns,"tests PASSED"))
}
