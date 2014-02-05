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

if (! interactive()) {
  a <- commandArgs(trailingOnly=TRUE)
  quickMode <- 0
  if ((length(a) > 0) && ("quick" == a[1]))  quickMode <- 1
  print(paste("quick set via commandArgs =",quickMode))
} else {
  f <- find('quickMode',mode='numeric')
  if (0 == length(f)) quickMode <- 0
  print(paste("quick set interactively =",quickMode))
}

# first the majority of tests: these are quicker
tests <- c("tReadSparse1", "tReadFull1",
           "tRead.set", "tRead.param",
           "tReadSV", "tReadText", "tReadCase",
           "tReadErr",
           "tReadWithDomainInfo", "tReadPreDomainInfo",
           "tReadCompressWithoutDomInfo", "tReadCompressWithDomInfo",
           "tReadVar0", "tReadVar1", "tReadVar3",
           "tReadEqu",
           "tReadDFNames",
           "tWriteSparse1", "tWriteSparse2", "tWriteFull1", "tWriteFull2",
           "tWriteLst",
           "tWriteSV",
           "tWriteEquTypes",
           "tWriteVarField", "tWriteVarSort", "tWriteVarOrder",
           "tWriteVarTypesSparse", "tWriteVarFUels",
           "tWriteVarTypesFull",
           "tWrap",
           "tInfo1", "tInfo2",
           "tLS")
if (0 == quickMode) {
  tests <- c(tests, "tWriteVar20")
}
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
  print ('---------------------------------------------------------------')
  print (paste("Starting test", ttt))
  rc <- source (paste(ttt,".R",sep=""))
  if (rc$value) {
    print (paste("Test", ttt, "result: PASSED"))
  } else {
    print (paste("Test", ttt, "result: FAILED"))
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
if (quickMode) print ("N.B.: quickMode is on, some tests were skipped")
if (nFails) {
  print (paste("Tests complete. ", nRuns,"tests run,",nFails,"tests FAILED"))
  for (nm in names(fTests)) print(paste0('  ',nm))
} else {
  print (paste("Testing complete.  All", nRuns,"tests PASSED"))
}
