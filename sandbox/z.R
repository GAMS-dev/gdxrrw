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
                 form='sparse', uels=cart, domains=dom,
                 field='l')
  u <- rgdx(fnIn,list(name='u',form='sparse',field='L'))
  chk <- chkRgdxRes (u, uwantL)
  if (!chk$same) {
    stop (paste("test rgdx(u,'L',unfiltered,uncompressed) failed",chk$msg))
  }
  vwantL <- list(name='v', type='variable', dim=1L,
                 val=matrix(c( 2,   -2), nrow=1, ncol=2, byrow=T),
                 form='sparse', uels=cart, domains=dom,
                 field='l')
  v <- rgdx(fnIn,list(name='v',form='sparse',field='L'))
  chk <- chkRgdxRes (v, vwantL)
  if (!chk$same) {
    stop (paste("test rgdx(v,'L',unfiltered,uncompressed) failed",chk$msg))
  }
  # marginal
  uwantM <- list(name='u', type='variable', dim=1L,
                 val=matrix(c( 2, 1.5), nrow=1, ncol=2, byrow=T),
                 form='sparse', uels=cart, domains=dom,
                 field='m')
  u <- rgdx(fnIn,list(name='u',form='sparse',field='M'))
  chk <- chkRgdxRes (u, uwantM)
  if (!chk$same) {
    stop (paste("test rgdx(u,'M',unfiltered,uncompressed) failed",chk$msg))
  }
  vwantM <- list(name='v', type='variable', dim=1L,
                 val=matrix(c( 2,  -20), nrow=1, ncol=2, byrow=T),
                 form='sparse', uels=cart, domains=dom,
                 field='m')
  v <- rgdx(fnIn,list(name='v',form='sparse',field='M'))
  chk <- chkRgdxRes (v, vwantM)
  if (!chk$same) {
    stop (paste("test rgdx(v,'M',unfiltered,uncompressed) failed",chk$msg))
  }
  # lower
  # upper
  # scale

  ### ---------- reading form=sparse, no filter, compress=T
  # level
  uwantL$domains <- comprDom
  uwantL$uels[[1]] <- c('k1')
  u <- rgdx(fnIn,list(name='u',form='sparse',field='L',compress=T))
  chk <- chkRgdxRes (u, uwantL)
  if (!chk$same) {
    stop (paste("test rgdx(u,'L',unfiltered,compress=T) failed",chk$msg))
  }
  vwantL$domains <- comprDom
  vwantL$uels[[1]] <- c('k2')
  vwantL$val=matrix(c( 1,   -2), nrow=1, ncol=2, byrow=T)
  v <- rgdx(fnIn,list(name='v',form='sparse',field='L',compress=T))
  chk <- chkRgdxRes (v, vwantL)
  if (!chk$same) {
    stop (paste("test rgdx(v,'L',unfiltered,compress=T) failed",chk$msg))
  }
  # marginal
  uwantM$domains <- comprDom
  uwantM$uels[[1]] <- c('k2')
  uwantM$val=matrix(c( 1, 1.5), nrow=1, ncol=2, byrow=T)
  u <- rgdx(fnIn,list(name='u',form='sparse',field='M',compress=T))
  chk <- chkRgdxRes (u, uwantM)
  if (!chk$same) {
    stop (paste("test rgdx(u,'M',unfiltered,compress=T) failed",chk$msg))
  }
  vwantM$domains <- comprDom
  vwantM$uels[[1]] <- c('k2')
  vwantM$val=matrix(c( 1, -20), nrow=1, ncol=2, byrow=T)
  v <- rgdx(fnIn,list(name='v',form='sparse',field='M',compress=T))
  chk <- chkRgdxRes (v, vwantM)
  if (!chk$same) {
    stop (paste("test rgdx(v,'M',unfiltered,compress=T) failed",chk$msg))
  }
  # lower
  # upper
  # scale

  ### ---------- reading form=sparse, filtered, compress=F
  # level
  f <- list(c('k1','k3','k4'))
  uwantL <- list(name='u', type='variable', dim=1L,
                 val=matrix(c( 1,   5,
                               2,   0), nrow=2, ncol=2, byrow=T),
                 form='sparse', uels=f, domains=userDom,
                 field='l')
  u <- rgdx(fnIn,list(name='u',form='sparse',uels=f))
  chk <- chkRgdxRes (u, uwantL)
  if (!chk$same) {
    stop (paste("test rgdx(u,'L',filtered,compress=F) failed",chk$msg))
  }
  f <- list(c('k2','k3'))
  vwantL <- list(name='v', type='variable', dim=1L,
                 val=matrix(c( 1,  -2), nrow=1, ncol=2, byrow=T),
                 form='sparse', uels=f, domains=userDom,
                 field='l')
  v <- rgdx(fnIn,list(name='v',form='sparse',uels=f))
  chk <- chkRgdxRes (v, vwantL)
  if (!chk$same) {
    stop (paste("test rgdx(v,'L',filtered,compress=F) failed",chk$msg))
  }
  # marginal
  f <- list(c('k1','k3','k4'))
  uwantM <- list(name='u', type='variable', dim=1L,
                 val=matrix(c( 1,   0,
                               2,   0), nrow=2, ncol=2, byrow=T),
                 form='sparse', uels=f, domains=userDom,
                 field='m')
  u <- rgdx(fnIn,list(name='u',form='sparse',uels=f,field='M'))
  chk <- chkRgdxRes (u, uwantM)
  if (!chk$same) {
    stop (paste("test rgdx(u,'M',filtered,compress=F) failed",chk$msg))
  }
  f <- list(c('k2','k3'))
  vwantM <- list(name='v', type='variable', dim=1L,
                 val=matrix(c( 1, -20), nrow=1, ncol=2, byrow=T),
                 form='sparse', uels=f, domains=userDom,
                 field='m')
  v <- rgdx(fnIn,list(name='v',form='sparse',uels=f,field='M'))
  chk <- chkRgdxRes (v, vwantM)
  if (!chk$same) {
    stop (paste("test rgdx(v,'M',filtered,compress=F) failed",chk$msg))
  }
  # lower
  # upper
  # scale

  ### ---------- reading form=full, no filter, compress=F
  # level
  t <- array(0,c(kCard,1),dimnames=cart)
  t['k1',1] <- 5
  uwantL <- list(name='u', type='variable', dim=1L,
                 val=t,
                 form='full', uels=cart, domains=dom,
                 field='l')
  u <- rgdx(fnIn,list(name='u',form='full'))
  chk <- chkRgdxRes (u, uwantL, T)
  if (!chk$same) {
    stop (paste("test rgdx(u,'L',full,unfiltered,compress=F) failed",chk$msg))
  }
  t <- array(0,c(kCard,1),dimnames=cart)
  t['k2',1] <- -2
  vwantL <- list(name='v', type='variable', dim=1L,
                 val=t,
                 form='full', uels=cart, domains=dom,
                 field='l')
  v <- rgdx(fnIn,list(name='v',form='full'))
  chk <- chkRgdxRes (v, vwantL, T)
  if (!chk$same) {
    stop (paste("test rgdx(v,'L',full,unfiltered,compress=F) failed",chk$msg))
  }
  # marginal
  t <- array(0,c(kCard,1),dimnames=cart)
  t['k2',1] <- 1.5
  uwantM <- list(name='u', type='variable', dim=1L,
                 val=t,
                 form='full', uels=cart, domains=dom,
                 field='m')
  u <- rgdx(fnIn,list(name='u',form='full',field='M'))
  chk <- chkRgdxRes (u, uwantM, T)
  if (!chk$same) {
    stop (paste("test rgdx(u,'M',full,unfiltered,compress=F) failed",chk$msg))
  }
  t <- array(0,c(kCard,1),dimnames=cart)
  t['k2',1] <- -20
  vwantM <- list(name='v', type='variable', dim=1L,
                 val=t,
                 form='full', uels=cart, domains=dom,
                 field='m')
  v <- rgdx(fnIn,list(name='v',form='full',field='m'))
  chk <- chkRgdxRes (v, vwantM, T)
  if (!chk$same) {
    stop (paste("test rgdx(v,'M',full,unfiltered,compress=F) failed",chk$msg))
  }
  # lower
  # upper
  # scale

  ### ---------- reading form=full, no filter, compress=T
  # level
  uc <- list(c('k1'))
  t <- array(0,c(1,1),dimnames=uc)
  t['k1',1] <- 5
  uwantL <- list(name='u', type='variable', dim=1L,
                 val=t,
                 form='full', uels=uc, domains=comprDom,
                 field='l')
  u <- rgdx(fnIn,list(name='u',form='full',compress=T))
  chk <- chkRgdxRes (u, uwantL, T)
  if (!chk$same) {
    stop (paste("test rgdx(u,'L',full,unfiltered,compress=T) failed",chk$msg))
  }
  uc <- list(c('k2'))
  t <- array(0,c(1,1),dimnames=uc)
  t['k2',1] <- -2
  vwantL <- list(name='v', type='variable', dim=1L,
                 val=t,
                 form='full', uels=uc, domains=comprDom,
                 field='l')
  v <- rgdx(fnIn,list(name='v',form='full',compress=T))
  chk <- chkRgdxRes (v, vwantL, T)
  if (!chk$same) {
    stop (paste("test rgdx(v,'L',full,unfiltered,compress=T) failed",chk$msg))
  }
  # marginal
  uc <- list(c('k2'))
  t <- array(0,c(1,1),dimnames=uc)
  t['k2',1] <- 1.5
  uwantM <- list(name='u', type='variable', dim=1L,
                 val=t,
                 form='full', uels=uc, domains=comprDom,
                 field='m')
  u <- rgdx(fnIn,list(name='u',form='full',compress=T,field='M'))
  chk <- chkRgdxRes (u, uwantM, T)
  if (!chk$same) {
    stop (paste("test rgdx(u,'M',full,unfiltered,compress=T) failed",chk$msg))
  }
  uc <- list(c('k2'))
  t <- array(0,c(1,1),dimnames=uc)
  t['k2',1] <- -20
  vwantM <- list(name='v', type='variable', dim=1L,
                 val=t,
                 form='full', uels=uc, domains=comprDom,
                 field='m')
  v <- rgdx(fnIn,list(name='v',form='full',compress=T,field='m'))
  chk <- chkRgdxRes (v, vwantM, T)
  if (!chk$same) {
    stop (paste("test rgdx(v,'M',full,unfiltered,compress=T) failed",chk$msg))
  }
  # lower
  # upper
  # scale

  ### ---------- reading form=full, filtered, compress=F
  # level
  f <- list(c('k1','k3','k4'))
  t <- array(0,c(3,1),dimnames=f)
  t['k1',1] <- 5
  uwantL <- list(name='u', type='variable', dim=1L,
                 val=t,
                 form='full', uels=f, domains=userDom,
                 field='l')
  u <- rgdx(fnIn,list(name='u',form='full',uels=f))
  chk <- chkRgdxRes (u, uwantL, T)
  if (!chk$same) {
    stop (paste("test rgdx(u,'L',full,filtered) failed",chk$msg))
  }
  f <- list(c('k1','k2','k3','k4'))
  t <- array(0,c(4,1),dimnames=f)
  t['k2',1] <- -2
  vwantL <- list(name='v', type='variable', dim=1L,
                 val=t,
                 form='full', uels=f, domains=userDom,
                 field='l')
  v <- rgdx(fnIn,list(name='v',form='full',uels=f))
  chk <- chkRgdxRes (v, vwantL, T)
  if (!chk$same) {
    stop (paste("test rgdx(v,'L',full,filtered) failed",chk$msg))
  }
  # marginal
  f <- list(c('k1','k3','k4'))
  t <- array(0,c(3,1),dimnames=f)
  uwantM <- list(name='u', type='variable', dim=1L,
                 val=t,
                 form='full', uels=f, domains=userDom,
                 field='m')
  u <- rgdx(fnIn,list(name='u',form='full',uels=f,field='M'))
  chk <- chkRgdxRes (u, uwantM, T)
  if (!chk$same) {
    stop (paste("test rgdx(u,'M',full,filtered) failed",chk$msg))
  }
  f <- list(c('k1','k2','k3','k4'))
  t <- array(0,c(4,1),dimnames=f)
  t['k2',1] <- -20
  vwantM <- list(name='v', type='variable', dim=1L,
                 val=t,
                 form='full', uels=f, domains=userDom,
                 field='m')
  v <- rgdx(fnIn,list(name='v',form='full',uels=f,field='m'))
  chk <- chkRgdxRes (v, vwantM, T)
  if (!chk$same) {
    stop (paste("test rgdx(v,'M',full,filtered) failed",chk$msg))
  }
  # lower
  # upper
  # scale


  print ("test of rgdx on variable reads passed")
  TRUE   ## all tests passed: return TRUE
},

error = function(ex) { print ("test of rgdx on variable reads failed"); print(ex) ; FALSE }
)
