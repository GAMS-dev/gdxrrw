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

tests <- c(
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
if (nFails) {
  print (paste("Tests complete. ", nRuns,"tests run,",nFails,"tests FAILED"))
  for (nm in names(fTests)) print(paste0('  ',nm))
} else {
  print (paste("Testing complete.  All", nRuns,"tests PASSED"))
}
