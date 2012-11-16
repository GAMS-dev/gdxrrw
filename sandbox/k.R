#### test rgdx reading equations
#### test form=['sparse','full'] X [filtered,unfiltered] X squeeze=[T(default),F]
#### ['l','m','lo','up','s']

#### wanted lists can be produced with    dump("listName",file="")

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

source ("chkSame.R")
reqIdent <- TRUE

lev <- 1
mar <- 2
low <- 3
upp <- 4
sca <- 5
fields <- c('l','m','lo','up','s')
nFields <- length(fields)

tryCatch({
  print ("test of rgdx on equation reads")
  rgdx('?')
  fnIn <- "tReadEqu.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }

  ### ---------- reading form=sparse, no filter
  # level
  e0wantL <- list(name='e0', type='equation', dim=0L,
                 val=matrix(0, nrow=1, ncol=1),
                 form='sparse', uels=list(), domains=character(0),
                 field='l')
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',field='L'))
  chk <- chkRgdxRes (e0, e0wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'L',unfiltered) failed",chk$msg))
  }

  ### ---------- reading form=sparse, filtered

  ### ---------- reading form=full, no filter

  ### ---------- reading form=full, filtered

  print ("test of rgdx on equation reads passed")
  TRUE   ## all tests passed: return TRUE
},

error = function(ex) { print ("test of rgdx on equation reads failed"); print(ex) ; FALSE }
)
