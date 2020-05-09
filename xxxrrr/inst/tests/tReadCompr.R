#### test rgdx reading the universe from a compressed GDX file

#### wanted lists can be produced with    dump("listName",file="")

if (! require(gdxrrw))      stop ("gdxrrw package is not available")

# this test is good to run prior to any GDX library being loaded,
# since it tests a bug that is turfed up in that case
# if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

source ("chkSame.R")
reqIdent <- TRUE

kUels <- c('k1', 'k2', 'k3', 'k4')
jUels <- c('jA', 'jB', 'jC', 'jD', 'jE', 'jF')

tryCatch({
  print ("testing rgdx on read of compressed GDX file")
  rgdx('?')
  fnIn <- "tReadCompr.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }

  ### ---------- reading universe
  uwantA <- list(name='*', type='set', dim=1L,
                 val=NULL, form=NULL, uels=c(kUels,jUels))
  u <- rgdx(fnIn)
  chk <- chkRgdxRes (u, uwantA, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx() on universe failed",chk$msg))
  }


  print ("test of rgdx on variable reads passed")
  TRUE   ## all tests passed: return TRUE
},

error = function(ex) { print ("test of rgdx on variable reads failed"); print(ex) ; FALSE }
)
