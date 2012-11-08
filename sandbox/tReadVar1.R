#### test rgdx reading 1-dim variables
#### test form=['sparse','full'] X [filtered,unfiltered] X compress=[T,F]
#### ['l','m','lo','up','s']

#### wanted lists can be produced with    dump("listName",file="")

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

source ("chkSame.R")

kUels <- c('k1', 'k2', 'k3', 'k4')
kCard <- length(kUels)
dom <- c('k')
comprDom <- c('_compressed')
userDom <- c('_user')
cart <- list(kUels)

tryCatch({
  print ("testing rgdx on 1-dim variable reads")
  rgdx('?')
  fnIn <- "tReadVar1.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }

  ### ---------- reading form=sparse, no filter, no compress
  # level
  uwantL <- list(name='u', type='variable', dim=1L,
                 val=matrix(c( 1,   5), nrow=1, ncol=2, byrow=T),
                 form='sparse',
                 uels=cart, domains=dom,
                 field='l')
  u <- rgdx(fnIn,list(name='u',form='sparse',field='L'))
  chk <- chkRgdxRes (u, uwantL)
  if (!chk$same) {
    stop (paste("test rgdx(u,'L',unfiltered,uncompressed) failed",chk$msg))
  }
  # marginal
  # lower
  # upper
  # scale

  ### ---------- reading form=sparse, no filter, compress=T
  # level
  uwantL$domains <- comprDom
  uwantL$uels <- list(c('k1'))
  u <- rgdx(fnIn,list(name='u',form='sparse',field='L',compress=T))
  chk <- chkRgdxRes (u, uwantL)
  if (!chk$same) {
    stop (paste("test rgdx(u,'L',unfiltered,compress=T) failed",chk$msg))
  }
  # marginal
  # lower
  # upper
  # scale

  ### ---------- reading form=sparse, filtered, compress=F
  # level
  f <- list(c('k1','k3','k4'))
  uwantL <- list(name='u', type='variable', dim=1L,
                 val=matrix(c( 1,   5,
                               2,   0), nrow=2, ncol=2, byrow=T),
                 form='sparse',
                 uels=f, domains=userDom,
                 field='l')
  u <- rgdx(fnIn,list(name='u',form='sparse',uels=f))
  chk <- chkRgdxRes (u, uwantL)
  if (!chk$same) {
    stop (paste("test rgdx(u,'L',filtered,compress=F) failed",chk$msg))
  }
  # marginal
  # lower
  # upper
  # scale

  ### ---------- reading form=full, no filter, compress=F
  # level
  t <- array(0,c(kCard,1),dimnames=cart)
  t['k1',1] <- 5
  uwantL <- list(name='u', type='variable', dim=1L,
                 val=t,
                 form='full',
                 uels=list(kUels), domains=dom,
                 field='l')
  u <- rgdx(fnIn,list(name='u',form='full'))
  chk <- chkRgdxRes (u, uwantL, T)
  if (!chk$same) {
    stop (paste("test rgdx(u,'L',full,unfiltered,compress=F) failed",chk$msg))
  }
  # marginal
  # lower
  # upper
  # scale

  ### ---------- reading form=full, no filter, compress=T
  # level
  f <- list(c('k1'))
  t <- array(0,c(1,1),dimnames=f)
  t['k1',1] <- 5
  uwantL <- list(name='u', type='variable', dim=1L,
                 val=t,
                 form='full',
                 uels=f, domains=comprDom,
                 field='l')
  u <- rgdx(fnIn,list(name='u',form='full',compress=T))
  chk <- chkRgdxRes (u, uwantL, T)
  if (!chk$same) {
    stop (paste("test rgdx(u,'L',full,unfiltered,compress=T) failed",chk$msg))
  }
  # marginal
  # lower
  # upper
  # scale

  ### ---------- reading form=full, filtered, compress=F
  f <- list(c('k1','k3','k4'))
  t <- array(0,c(3,1),dimnames=f)
  t['k1',1] <- 5
  uwantL <- list(name='u', type='variable', dim=1L,
                 val=t,
                 form="full",
                 uels=f, domains=userDom,
                 field='l')
  u <- rgdx(fnIn,list(name='u',form='full',uels=f))
  chk <- chkRgdxRes (u, uwantL, T)
  if (!chk$same) {
    stop (paste("test rgdx(u,'L',full,filtered) failed",chk$msg))
  }
  # level
  # marginal
  # lower
  # upper
  # scale


  print ("test of rgdx on variable reads passed")
  TRUE   ## all tests passed: return TRUE
},

error = function(ex) { print ("test of rgdx on variable reads failed"); print(ex) ; FALSE }
)
