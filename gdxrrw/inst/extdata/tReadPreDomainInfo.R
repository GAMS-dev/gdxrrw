#### test rgdx with a file generated prior to domain info in GDX
#### test form=['sparse','full'] X [filtered,unfiltered] X compress=[T,F]

#### wanted lists produced with    dump("listName",file="")

source ("chkSame.R")

tryCatch({
  print ("testing rgdx handling of GDX file with no domain info")
  rgdx('?')
  fnIn <- "teTestPreDomainInfo.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }
  useDomInfo <- TRUE

  source ("tReadDomInfoBody.R")


  print ("test of rgdx on pre-domain-info GDX file passed")
  TRUE   ## all tests passed: return TRUE
},

error = function(ex) { print ("test of rgdx on pre-domain-info GDX file failed"); print(ex) ; FALSE }
)
