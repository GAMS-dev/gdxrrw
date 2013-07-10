## ## test wgdx.lst wrapper
## read from target GDX file, reproduce it in different ways using the
## wgdx.lst wrapper

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

testName <- 'wgdx.lst wrapper'

errFunc <- function(ex) {
  print (paste("test of wgdx on",testName,"failed"))
  print(ex)
  FALSE
} # errFunc

tryCatch({
  print (paste("testing wgdx on", testName))
  wgdx('?')
  fnTarg <- "lstTarget.gdx"
  fnOut <- "tmp.gdx"
  if (! file_test ('-f', fnTarg)) {
    stop (paste("FAIL: File-to-duplicate", fnTarg, "does not exist"))
  }

  ## read symbols from target, using multiple formats
  ii <- rgdx (fnTarg, list(name='i'))
  aa <- rgdx (fnTarg, list(name='a'))
  bb <- rgdx (fnTarg, list(name='b'))
  uu <- rgdx (fnTarg, list(name='u'))
  idf <- rgdx.set (fnTarg, 'i')
  adf <- rgdx.param (fnTarg, 'a')
  bdf <- rgdx.param (fnTarg, 'b')
  cdf <- rgdx.param (fnTarg, 'c')
  ddf <- rgdx.param (fnTarg, 'd')
  edf <- rgdx.param (fnTarg, 'e')
  fdf <- rgdx.param (fnTarg, 'f')
  gdf <- rgdx.param (fnTarg, 'g')
  usc <- rgdx.scalar (fnTarg, 'u')
  vsc <- rgdx.scalar (fnTarg, 'v')

  # ---------- works with older wgdx.lst: 0.2.0 and previous ---------------
  wgdx.lst(fnOut, list(idf, adf))
  cmd <- paste('gdxdiff', fnTarg, fnOut, 'id="i a"')
  rc <- system(cmd)
  if (0 != rc) {
    stop (paste("GDXDIFF FAILURE:", cmd));
  } else {
    print (paste("gdxdiff ok:", cmd));
  }
  wgdx.lst(fnOut, list(ii, adf, bb, cdf, vsc))
  cmd <- paste('gdxdiff', fnTarg, fnOut, 'id="i a b c v"')
  rc <- system(cmd)
  if (0 != rc) {
    stop (paste("GDXDIFF FAILURE:", cmd));
  } else {
    print (paste("gdxdiff ok:", cmd));
  }
  wgdx.lst(fnOut, list(ii, adf, bb, cdf, ddf, edf, fdf, gdf, uu, vsc))
  cmd <- paste('gdxdiff', fnTarg, fnOut)
  rc <- system(cmd)
  if (0 != rc) {
    stop (paste("GDXDIFF FAILURE:", cmd));
  } else {
    print (paste("gdxdiff ok:", cmd));
  }

  # ------------ requires newer wgdx.lst: 0.4.0 or later -------------
  wgdx.lst(fnOut, idf, adf)
  cmd <- paste('gdxdiff', fnTarg, fnOut, 'id="i a"')
  rc <- system(cmd)
  if (0 != rc) {
    stop (paste("GDXDIFF FAILURE:", cmd));
  } else {
    print (paste("gdxdiff ok:", cmd));
  }
  wgdx.lst(fnOut, list(idf), list(adf))
  cmd <- paste('gdxdiff', fnTarg, fnOut, 'id="i a"')
  rc <- system(cmd)
  if (0 != rc) {
    stop (paste("GDXDIFF FAILURE:", cmd));
  } else {
    print (paste("gdxdiff ok:", cmd));
  }
  wgdx.lst(fnOut, list(idf,aa,uu), list(bb,vsc,edf,gdf),cdf)
  cmd <- paste('gdxdiff', fnTarg, fnOut, 'id="i a u b v e g c"')
  rc <- system(cmd)
  if (0 != rc) {
    stop (paste("GDXDIFF FAILURE:", cmd));
  } else {
    print (paste("gdxdiff ok:", cmd));
  }
  wgdx.lst(fnOut, fdf, list(idf,aa,uu), ddf, list(bb,vsc,edf,gdf), cdf)
  cmd <- paste('gdxdiff', fnTarg, fnOut)
  rc <- system(cmd)
  if (0 != rc) {
    stop (paste("GDXDIFF FAILURE:", cmd));
  } else {
    print (paste("gdxdiff ok:", cmd));
  }


  print (paste("test of wgdx on", testName, "passed"))
  TRUE   ## all tests passed: return TRUE
},

error = errFunc
)
