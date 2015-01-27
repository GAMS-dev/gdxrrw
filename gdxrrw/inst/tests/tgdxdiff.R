### Test that the gdxdiff utility can be run successfully via system()

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

fnData <- "trnsport.gdx"

tryCatch({
  if (! file_test ('-f', fnData)) {
    stop (paste("FAIL: File", fnData, "does not exist"))
  }
  rc <- system (paste("gdxdiff",fnData,fnData))
  if (0 != rc) {
    stop(paste("Bad return from trivial gdxdiff: wanted 0, got",rc))
  } else {
    print ("gdxdiff call succeeded: it must be available and functional")
  }

  TRUE
}

, error = function(ex) { print(ex) ; FALSE }
)
