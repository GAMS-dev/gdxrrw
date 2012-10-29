#### test rgdx reading a 3-dim variable
#### test form=['sparse','full'] X [filtered,unfiltered] X compress=[T,F]
#### ['l','m','lo','up','s']

#### wanted lists produced with    dump("listName",file="")

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

source ("chkSame.R")

iUels <- c("i1", "i2")
iCard <- length(iUels)
jUels <- c("j1", "j2")
jCard <- length(jUels)
kUels <- c("k1", "k2")
kCard <- length(kUels)
domains <- c("i","j","k")
comprDomains <- c("_compressed","_compressed","_compressed")
userDomains <- c("_user","_user","_user")

tryCatch({
  print ("testing rgdx on variable reads")
  rgdx('?')
  fnIn <- "tReadVar3.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }

  ### ---------- reading form=sparse, no filter, no compress
  # level
  xwantL <- list(name="x", type="variable", dim=3,
                 val=matrix(c( 1,1,2,   1
                              ,1,2,1,  10
                              ,1,2,2,  11
                              ,2,1,1, 100
                              ,2,1,2, 101
                              ,2,2,1, 110
                              ,2,2,2,   6), nrow=7, ncol=4,byrow=T),
                 form="sparse",
                 uels=list(iUels,jUels,kUels), domains=domains,
                 field='l')
  x <- rgdx(fnIn,list(name='x',form='sparse',field='L'))
  chk <- chkRgdxRes (x, xwantL)
  if (!chk$same) {
    stop (paste("test rgdx(x,'L',unfiltered,uncompressed) failed",chk$msg))
  }
  # marginal
  xwantM <- list(name="x", type="variable", dim=3,
                 val=matrix(c( 1,1,2,  .25
                              ,1,2,2,  .25), nrow=2, ncol=4,byrow=T),
                 form="sparse",
                 uels=list(iUels,jUels,kUels), domains=domains,
                 field='m')
  x <- rgdx(fnIn,list(name='x',form='sparse',field='M'))
  chk <- chkRgdxRes (x, xwantM)
  if (!chk$same) {
    stop (paste("test rgdx(x,'M',unfiltered,uncompressed) failed",chk$msg))
  }
  # lower
  xwantLo <- list(name="x", type="variable", dim=3,
                  val=matrix(c( 1,2,1, -Inf
                               ,1,2,2,  100
                               ,2,2,2,    6), nrow=3, ncol=4,byrow=T),
                  form="sparse",
                  uels=list(iUels,jUels,kUels), domains=domains,
                  field='lo')
  x <- rgdx(fnIn,list(name='x',form='sparse',field='lo'))
  chk <- chkRgdxRes (x, xwantLo)
  if (!chk$same) {
    stop (paste("test rgdx(x,'lo',unfiltered,uncompressed) failed",chk$msg))
  }
  # upper
  xwantUp <- list(name="x", type="variable", dim=3,
                  val=matrix(c( 1,1,1,  525
                               ,1,1,2, +Inf
                               ,1,2,1, +Inf
                               ,1,2,2, +Inf
                               ,2,1,2, +Inf
                               ,2,2,1, +Inf
                               ,2,2,2,    6), nrow=7, ncol=4,byrow=T),
                  form="sparse",
                  uels=list(iUels,jUels,kUels), domains=domains,
                  field='up')
  x <- rgdx(fnIn,list(name='x',form='sparse',field='up'))
  chk <- chkRgdxRes (x, xwantUp)
  if (!chk$same) {
    stop (paste("test rgdx(x,'lo',unfiltered,uncompressed) failed",chk$msg))
  }
  # scale
  xwantS <- list(name="x", type="variable", dim=3,
                 val=matrix(c( 1,1,1,    1
                              ,1,1,2,    1
                              ,1,2,1,    1
                              ,1,2,2,    1
                              ,2,1,1,    1
                              ,2,1,2,    1
                              ,2,2,1,   10
                              ,2,2,2,    1), nrow=8, ncol=4,byrow=T),
                 form="sparse",
                 uels=list(iUels,jUels,kUels), domains=domains,
                 field='s')
  x <- rgdx(fnIn,list(name='x',form='sparse',field='s'))
  chk <- chkRgdxRes (x, xwantS)
  if (!chk$same) {
    stop (paste("test rgdx(x,'s',unfiltered,uncompressed) failed",chk$msg))
  }

  ### ---------- reading form=sparse, no filter, compress=T
  # level
  xwantL$domains <- comprDomains
  x <- rgdx(fnIn,list(name='x',form='sparse',field='L',compress=T))
  chk <- chkRgdxRes (x, xwantL)
  if (!chk$same) {
    stop (paste("test rgdx(x,'L',unfiltered,compress=T) failed",chk$msg))
  }
  # marginal
  xwantM$domains <- comprDomains
  xwantM$val[,3] <- 1
  xwantM$uels[[1]] <- c('i1')
  xwantM$uels[[3]] <- c('k2')
  x <- rgdx(fnIn,list(name='x',form='sparse',field='M',compress=T))
  chk <- chkRgdxRes (x, xwantM)
  if (!chk$same) {
    stop (paste("test rgdx(x,'M',unfiltered,compress=T) failed",chk$msg))
  }
  # lower
  xwantLo$domains <- comprDomains
  xwantLo$val[,2] <- 1
  xwantLo$uels[[2]] <- c('j2')
  x <- rgdx(fnIn,list(name='x',form='sparse',field='lo',compress=T))
  chk <- chkRgdxRes (x, xwantLo)
  if (!chk$same) {
    stop (paste("test rgdx(x,'lo',unfiltered,compress=T) failed",chk$msg))
  }
  # upper
  xwantUp$domains <- comprDomains
  x <- rgdx(fnIn,list(name='x',form='sparse',field='up',compress=T))
  chk <- chkRgdxRes (x, xwantUp)
  if (!chk$same) {
    stop (paste("test rgdx(x,'up',unfiltered,compress=T) failed",chk$msg))
  }
  # scale
  xwantS$domains <- comprDomains
  x <- rgdx(fnIn,list(name='x',form='sparse',field='s',compress=T))
  chk <- chkRgdxRes (x, xwantS)
  if (!chk$same) {
    stop (paste("test rgdx(x,'s',unfiltered,compress=T) failed",chk$msg))
  }

  ### ---------- reading form=sparse, filtered, compress=F
  # level
  f <- list(c('i2'),jUels,kUels)
  xwantL <- list(name="x", type="variable", dim=3,
                 val=matrix(c( 1,1,1, 100
                              ,1,1,2, 101
                              ,1,2,1, 110
                              ,1,2,2,   6), nrow=4, ncol=4,byrow=T),
                 form="sparse",
                 uels=f, domains=userDomains,
                 field='l')
  x <- rgdx(fnIn,list(name='x',form='sparse',uels=f))
  chk <- chkRgdxRes (x, xwantL)
  if (!chk$same) {
    stop (paste("test rgdx(x,'L',filtered,compress=F) failed",chk$msg))
  }
  # marginal
  f <- list(iUels,jUels,kUels)
  xwantM <- list(name="x", type="variable", dim=3,
                 val=matrix(c( 1,1,1,   0
                              ,1,1,2,  .25
                              ,1,2,1,   0
                              ,1,2,2,  .25
                              ,2,1,1,   0
                              ,2,1,2,   0
                              ,2,2,1,   0
                              ,2,2,2,   0 ), nrow=8, ncol=4,byrow=T),
                 form="sparse",
                 uels=f, domains=userDomains,
                 field='m')
  x <- rgdx(fnIn,list(name='x',form='sparse',uels=f,field='M'))
  chk <- chkRgdxRes (x, xwantM)
  if (!chk$same) {
    stop (paste("test rgdx(x,'M',filtered,compress=F) failed",chk$msg))
  }
  # lower
  f <- list(iUels,c('j2'),kUels)
  xwantLo <- list(name="x", type="variable", dim=3,
                  val=matrix(c( 1,1,1, -Inf
                               ,1,1,2,  100
                               ,2,1,1,    0
                               ,2,1,2,    6), nrow=4, ncol=4,byrow=T),
                  form="sparse",
                  uels=f, domains=userDomains,
                  field='lo')
  x <- rgdx(fnIn,list(name='x',form='sparse',uels=f,field='lo'))
  chk <- chkRgdxRes (x, xwantLo)
  if (!chk$same) {
    stop (paste("test rgdx(x,'lo',filtered,compress=F) failed",chk$msg))
  }
  # upper
  f <- list(iUels,jUels,kUels)
  xwantUp <- list(name="x", type="variable", dim=3,
                  val=matrix(c( 1,1,1,  525
                               ,1,1,2, +Inf
                               ,1,2,1, +Inf
                               ,1,2,2, +Inf
                               ,2,1,1,    0
                               ,2,1,2, +Inf
                               ,2,2,1, +Inf
                               ,2,2,2,    6), nrow=8, ncol=4,byrow=T),
                  form="sparse",
                  uels=f, domains=userDomains,
                  field='up')
  x <- rgdx(fnIn,list(name='x',form='sparse',uels=f,field='UP'))
  chk <- chkRgdxRes (x, xwantUp)
  if (!chk$same) {
    stop (paste("test rgdx(x,'up',filtered,compress=F) failed",chk$msg))
  }
  # scale
  f <- list(iUels,jUels,kUels)
  xwantS <- list(name="x", type="variable", dim=3,
                 val=matrix(c( 1,1,1,    1
                              ,1,1,2,    1
                              ,1,2,1,    1
                              ,1,2,2,    1
                              ,2,1,1,    1
                              ,2,1,2,    1
                              ,2,2,1,   10
                              ,2,2,2,    1), nrow=8, ncol=4,byrow=T),
                 form="sparse",
                 uels=f, domains=userDomains,
                 field='s')
  x <- rgdx(fnIn,list(name='x',form='sparse',field='S',uels=f))
  chk <- chkRgdxRes (x, xwantS)
  if (!chk$same) {
    stop (paste("test rgdx(x,'S',filtered,compress=F) failed",chk$msg))
  }

  ### ---------- reading form=full, no filter, compress=F
  # level
  v <- array(0,c(iCard,jCard,kCard))
  for (i in 1:iCard) {
    for (j in 1:jCard) {
      for (k in 1:kCard) {
        v[i,j,k] <- 100 * (i-1) + 10 * (j-1) + (k-1)
      }
    }
  }
  v[iCard,jCard,kCard] <- 6
  xwantL <- list(name="x", type="variable", dim=3,
                 val=v,
                 form="full",
                 uels=list(iUels,jUels,kUels), domains=domains,
                 field='l')
  x <- rgdx(fnIn,list(name='x',form='full'))
  chk <- chkRgdxRes (x, xwantL)
  if (!chk$same) {
    stop (paste("test rgdx(x,'L',full,unfiltered,compress=F) failed",chk$msg))
  }
  # marginal
  v <- array(0,c(iCard,jCard,kCard))
  v[1,1,2] <- .25
  v[1,2,2] <- .25
  xwantM <- list(name="x", type="variable", dim=3,
                 val=v,
                 form="full",
                 uels=list(iUels,jUels,kUels), domains=domains,
                 field='m')
  x <- rgdx(fnIn,list(name='x',form='full',field='m'))
  chk <- chkRgdxRes (x, xwantM)
  if (!chk$same) {
    stop (paste("test rgdx(x,'M',full,unfiltered,compress=F) failed",chk$msg))
  }
  # lower
  v <- array(0,c(iCard,jCard,kCard))
  v[1,2,1] <- -Inf
  v[1,2,2] <- 100
  v[2,2,2] <- 6
  xwantLo <- list(name="x", type="variable", dim=3,
                  val=v,
                  form="full",
                  uels=list(iUels,jUels,kUels), domains=domains,
                  field='lo')
  x <- rgdx(fnIn,list(name='x',form='full',field='lo'))
  chk <- chkRgdxRes (x, xwantLo)
  if (!chk$same) {
    stop (paste("test rgdx(x,'lo',full,unfiltered,compress=F) failed",chk$msg))
  }
  # upper
  v <- array(+Inf,c(iCard,jCard,kCard))
  v[1,1,1] <- 525
  v[2,1,1] <- 0
  v[2,2,2] <- 6
  xwantUp <- list(name="x", type="variable", dim=3,
                  val=v,
                  form="full",
                  uels=list(iUels,jUels,kUels), domains=domains,
                  field='up')
  x <- rgdx(fnIn,list(name='x',form='full',field='up'))
  chk <- chkRgdxRes (x, xwantUp)
  if (!chk$same) {
    stop (paste("test rgdx(x,'up',full,unfiltered,compress=F) failed",chk$msg))
  }
  # scale
  v <- array(1,c(iCard,jCard,kCard))
  v[2,2,1] <- 10
  xwantS <- list(name="x", type="variable", dim=3,
                 val=v,
                 form="full",
                 uels=list(iUels,jUels,kUels), domains=domains,
                 field='s')
  x <- rgdx(fnIn,list(name='x',form='full',field='s'))
  chk <- chkRgdxRes (x, xwantS)
  if (!chk$same) {
    stop (paste("test rgdx(x,'s',full,unfiltered,compress=F) failed",chk$msg))
  }


  print ("test of rgdx on variable reads passed")
  TRUE   ## all tests passed: return TRUE
},

error = function(ex) { print ("test of rgdx on variable reads failed"); print(ex) ; FALSE }
)
