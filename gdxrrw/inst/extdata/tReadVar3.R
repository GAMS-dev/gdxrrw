#### test rgdx reading a 3-dim variable
#### test form=['sparse','full'] X [filtered,unfiltered]
#### ['l','m','lo','up','s']

#### wanted lists produced with    dump("listName",file="")

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

source ("chkSame.R")
reqIdent <- TRUE

iUels <- c("i1", "i2")
iCard <- length(iUels)
jUels <- c("j1", "j2")
jCard <- length(jUels)
kUels <- c("k1", "k2")
kCard <- length(kUels)
domains <- c("i","j","k")
userDomains <- c("_user","_user","_user")
cart <- list(iUels,jUels,kUels)

tryCatch({
  print ("testing rgdx on variable reads")
  rgdx('?')
  fnIn <- "tReadVar3.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }

  ### ---------- reading form=sparse, no filter
  # level
  xwantL <- list(name="x", type="variable", dim=3L,
                 val=matrix(c( 1,1,2,   1
                              ,1,2,1,  10
                              ,1,2,2,  11
                              ,2,1,1, 100
                              ,2,1,2, 101
                              ,2,2,1, 110
                              ,2,2,2,   6), nrow=7, ncol=4, byrow=T),
                 form="sparse",
                 uels=list(iUels,jUels,kUels), domains=domains,
                 field='l',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='x',form='sparse',field='L'))
  chk <- chkRgdxRes (x, xwantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'L',unfiltered) failed",chk$msg))
  }
  xwantL$val <- matrix(c( 1,1,1,   0
                         ,1,1,2,   1
                         ,1,2,1,  10
                         ,1,2,2,  11
                         ,2,1,1, 100
                         ,2,1,2, 101
                         ,2,2,1, 110
                         ,2,2,2,   6), nrow=8, ncol=4, byrow=T)
  x <- rgdx(fnIn,list(name='x',form='sparse',field='L'),squeeze=F)
  chk <- chkRgdxRes (x, xwantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'L',unfiltered,squeeze=F) failed",chk$msg))
  }
  # marginal
  xwantM <- list(name="x", type="variable", dim=3L,
                 val=matrix(c( 1,1,2,  .25
                              ,1,2,2,  .25), nrow=2, ncol=4, byrow=T),
                 form="sparse",
                 uels=list(iUels,jUels,kUels), domains=domains,
                 field='m',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='x',form='sparse',field='M'))
  chk <- chkRgdxRes (x, xwantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'M',unfiltered) failed",chk$msg))
  }
  xwantM$val <- matrix(c( 1,1,1,   0
                         ,1,1,2,  .25
                         ,1,2,1,  0
                         ,1,2,2,  .25
                         ,2,1,1,  0
                         ,2,1,2,  0
                         ,2,2,1,  0
                         ,2,2,2,  0
                         ), nrow=8, ncol=4, byrow=T)
  x <- rgdx(fnIn,list(name='x',form='sparse',field='M'),squeeze=F)
  chk <- chkRgdxRes (x, xwantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'M',unfiltered,squeeze=F) failed",chk$msg))
  }
  # lower
  xwantLo <- list(name="x", type="variable", dim=3L,
                  val=matrix(c( 1,2,1, -Inf
                               ,1,2,2,  100
                               ,2,2,2,    6), nrow=3, ncol=4, byrow=T),
                  form="sparse",
                  uels=list(iUels,jUels,kUels), domains=domains,
                  field='lo',
                  varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='x',form='sparse',field='lo'))
  chk <- chkRgdxRes (x, xwantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'lo',unfiltered) failed",chk$msg))
  }
  xwantLo$val <- matrix(c( 1,1,1,  0
                          ,1,1,2,  0
                          ,1,2,1, -Inf
                          ,1,2,2,  100
                          ,2,1,1,  0
                          ,2,1,2,  0
                          ,2,2,1,  0
                          ,2,2,2,  6), nrow=8, ncol=4, byrow=T)
  x <- rgdx(fnIn,list(name='x',form='sparse',field='lo'),squeeze=F)
  chk <- chkRgdxRes (x, xwantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'lo',unfiltered,squeeze=F) failed",chk$msg))
  }
  # upper
  xwantUp <- list(name="x", type="variable", dim=3L,
                  val=matrix(c( 1,1,1,  525
                               ,2,1,1,    0
                               ,2,2,2,    6), nrow=3, ncol=4, byrow=T),
                  form="sparse",
                  uels=list(iUels,jUels,kUels), domains=domains,
                  field='up',
                  varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='x',form='sparse',field='up'))
  chk <- chkRgdxRes (x, xwantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'up',unfiltered) failed",chk$msg))
  }
  xwantUp$val <- matrix(c( 1,1,1,  525
                          ,1,1,2,  Inf
                          ,1,2,1,  Inf
                          ,1,2,2,  Inf
                          ,2,1,1,    0
                          ,2,1,2,  Inf
                          ,2,2,1,  Inf
                          ,2,2,2,    6), nrow=8, ncol=4, byrow=T)
  x <- rgdx(fnIn,list(name='x',form='sparse',field='up'),squeeze=F)
  chk <- chkRgdxRes (x, xwantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'up',unfiltered,squeeze=F) failed",chk$msg))
  }
  # scale
  xwantS <- list(name="x", type="variable", dim=3L,
                 val=matrix(c( 2,2,1,   10), nrow=1, ncol=4, byrow=T),
                 form="sparse",
                 uels=list(iUels,jUels,kUels), domains=domains,
                 field='s',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='x',form='sparse',field='s'))
  chk <- chkRgdxRes (x, xwantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'s',unfiltered) failed",chk$msg))
  }
  xwantS$val <- matrix(c( 1,1,1,    1
                         ,1,1,2,    1
                         ,1,2,1,    1
                         ,1,2,2,    1
                         ,2,1,1,    1
                         ,2,1,2,    1
                         ,2,2,1,   10
                         ,2,2,2,    1), nrow=8, ncol=4, byrow=T)
  x <- rgdx(fnIn,list(name='x',form='sparse',field='s'),squeeze=F)
  chk <- chkRgdxRes (x, xwantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'s',unfiltered,squeeze=F) failed",chk$msg))
  }

  ### ---------- reading form=sparse, filtered
  # level
  f <- list(c('i2'),jUels,kUels)
  xwantL <- list(name="x", type="variable", dim=3L,
                 val=matrix(c( 1,1,1, 100
                              ,1,1,2, 101
                              ,1,2,1, 110
                              ,1,2,2,   6), nrow=4, ncol=4, byrow=T),
                 form="sparse",
                 uels=f, domains=userDomains,
                 field='l',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='x',form='sparse',uels=f))
  chk <- chkRgdxRes (x, xwantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'L',filtered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='x',form='sparse',uels=f),squeeze=F)
  chk <- chkRgdxRes (x, xwantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'L',filtered,squeeze=F) failed",chk$msg))
  }
  # marginal
  f <- list(iUels,jUels,kUels)
  xwantM <- list(name="x", type="variable", dim=3L,
                 val=matrix(c( 1,1,2,  .25
                              ,1,2,2,  .25), nrow=2, ncol=4, byrow=T),
                 form="sparse",
                 uels=f, domains=userDomains,
                 field='m',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='x',form='sparse',uels=f,field='M'))
  chk <- chkRgdxRes (x, xwantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'M',filtered) failed",chk$msg))
  }
  xwantM$val <- matrix(c( 1,1,1,  0
                         ,1,1,2,  .25
                         ,1,2,1,  0
                         ,1,2,2,  .25
                         ,2,1,1,  0
                         ,2,1,2,  0
                         ,2,2,1,  0
                         ,2,2,2,  0
                        ), nrow=8, ncol=4, byrow=T)
  x <- rgdx(fnIn,list(name='x',form='sparse',uels=f,field='M'),squeeze=F)
  chk <- chkRgdxRes (x, xwantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'M',filtered,squeeze=F) failed",chk$msg))
  }
  # lower
  f <- list(iUels,c('j2'),kUels)
  xwantLo <- list(name="x", type="variable", dim=3L,
                  val=matrix(c( 1,1,1, -Inf
                               ,1,1,2,  100
                               ,2,1,2,    6), nrow=3, ncol=4, byrow=T),
                  form="sparse",
                  uels=f, domains=userDomains,
                  field='lo',
                  varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='x',form='sparse',uels=f,field='lo'))
  chk <- chkRgdxRes (x, xwantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'lo',filtered) failed",chk$msg))
  }
  xwantLo$val <- matrix(c( 1,1,1, -Inf
                          ,1,1,2,  100
                          ,2,1,1,    0
                          ,2,1,2,    6), nrow=4, ncol=4, byrow=T)
  x <- rgdx(fnIn,list(name='x',form='sparse',uels=f,field='lo'),squeeze=F)
  chk <- chkRgdxRes (x, xwantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'lo',filtered,squeeze=F) failed",chk$msg))
  }
  # upper
  f <- list(iUels,jUels,kUels)
  xwantUp <- list(name="x", type="variable", dim=3L,
                  val=matrix(c( 1,1,1,  525
                               ,2,1,1,    0
                               ,2,2,2,    6), nrow=3, ncol=4, byrow=T),
                  form="sparse",
                  uels=f, domains=userDomains,
                  field='up',
                  varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='x',form='sparse',uels=f,field='UP'))
  chk <- chkRgdxRes (x, xwantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'up',filtered) failed",chk$msg))
  }
  xwantUp$val <- matrix(c( 1,1,1,  525
                          ,1,1,2, +Inf
                          ,1,2,1, +Inf
                          ,1,2,2, +Inf
                          ,2,1,1,    0
                          ,2,1,2, +Inf
                         ,2,2,1, +Inf
                          ,2,2,2,    6), nrow=8, ncol=4, byrow=T)
  x <- rgdx(fnIn,list(name='x',form='sparse',uels=f,field='UP'),squeeze=F)
  chk <- chkRgdxRes (x, xwantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'up',filtered,squeeze=F) failed",chk$msg))
  }
  # scale
  f <- list(iUels,jUels,kUels)
  xwantS <- list(name="x", type="variable", dim=3L,
                 val=matrix(c( 2,2,1,   10), nrow=1, ncol=4, byrow=T),
                 form="sparse",
                 uels=f, domains=userDomains,
                 field='s',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='x',form='sparse',field='S',uels=f))
  chk <- chkRgdxRes (x, xwantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'S',filtered) failed",chk$msg))
  }
  xwantS$val <- matrix(c( 1,1,1,    1
                         ,1,1,2,    1
                         ,1,2,1,    1
                         ,1,2,2,    1
                         ,2,1,1,    1
                         ,2,1,2,    1
                         ,2,2,1,   10
                         ,2,2,2,    1), nrow=8, ncol=4, byrow=T)
  x <- rgdx(fnIn,list(name='x',form='sparse',field='S',uels=f),squeeze=F)
  chk <- chkRgdxRes (x, xwantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'S',filtered,squeeze=F) failed",chk$msg))
  }

  ### ---------- reading form=full, no filter
  # level
  v <- array(0,c(iCard,jCard,kCard),dimnames=cart)
  for (i in 1:iCard) {
    for (j in 1:jCard) {
      for (k in 1:kCard) {
        v[i,j,k] <- 100 * (i-1) + 10 * (j-1) + (k-1)
      }
    }
  }
  v['i2','j2','k2'] <- 6
  xwantL <- list(name="x", type="variable", dim=3L,
                 val=v,
                 form="full",
                 uels=list(iUels,jUels,kUels), domains=domains,
                 field='l',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='x',form='full'))
  chk <- chkRgdxRes (x, xwantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'L',full,unfiltered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='x',form='full'),squeeze=F)
  chk <- chkRgdxRes (x, xwantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'L',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  # marginal
  v <- array(0,c(iCard,jCard,kCard),dimnames=cart)
  v['i1','j1',2] <- .25
  v['i1','j2',2] <- .25
  xwantM <- list(name="x", type="variable", dim=3L,
                 val=v,
                 form="full",
                 uels=list(iUels,jUels,kUels), domains=domains,
                 field='m',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='x',form='full',field='m'))
  chk <- chkRgdxRes (x, xwantM, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'M',full,unfiltered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='x',form='full',field='m'),squeeze=F)
  chk <- chkRgdxRes (x, xwantM, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'M',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  # lower
  v <- array(0,c(iCard,jCard,kCard),dimnames=cart)
  v['i1','j2','k1'] <- -Inf
  v['i1','j2','k2'] <- 100
  v['i2','j2','k2'] <- 6
  xwantLo <- list(name="x", type="variable", dim=3L,
                  val=v,
                  form="full",
                  uels=list(iUels,jUels,kUels), domains=domains,
                  field='lo',
                  varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='x',form='full',field='lo'))
  chk <- chkRgdxRes (x, xwantLo, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'lo',full,unfiltered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='x',form='full',field='lo'),squeeze=F)
  chk <- chkRgdxRes (x, xwantLo, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'lo',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  # upper
  v <- array(+Inf,c(iCard,jCard,kCard),dimnames=cart)
  v['i1','j1','k1'] <- 525
  v['i2','j1','k1'] <- 0
  v['i2','j2','k2'] <- 6
  xwantUp <- list(name="x", type="variable", dim=3L,
                  val=v,
                  form="full",
                  uels=list(iUels,jUels,kUels), domains=domains,
                  field='up',
                  varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='x',form='full',field='up'))
  chk <- chkRgdxRes (x, xwantUp, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'up',full,unfiltered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='x',form='full',field='up'),squeeze=F)
  chk <- chkRgdxRes (x, xwantUp, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'up',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  # scale
  v <- array(1,c(iCard,jCard,kCard),dimnames=cart)
  v[2,2,1] <- 10
  xwantS <- list(name="x", type="variable", dim=3L,
                 val=v,
                 form="full",
                 uels=cart, domains=domains,
                 field='s',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='x',form='full',field='s'))
  chk <- chkRgdxRes (x, xwantS, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'s',full,unfiltered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='x',form='full',field='s'),squeeze=F)
  chk <- chkRgdxRes (x, xwantS, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'s',full,unfiltered,squeeze=F) failed",chk$msg))
  }

  ### ---------- reading form=full, filtered
  # level
  f <- list(c('i2'),jUels,kUels)
  v <- array(0,c(1,jCard,kCard),dimnames=f)
  for (j in 1:jCard) {
    for (k in 1:kCard) {
      v[1,j,k] <- 100 + 10 * (j-1) + (k-1)
    }
  }
  v['i2','j2','k2'] <- 6
  xwantL <- list(name="x", type="variable", dim=3L,
                 val=v,
                 form="full",
                 uels=f, domains=userDomains,
                 field='l',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='x',form='full',uels=f))
  chk <- chkRgdxRes (x, xwantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'L',full,filtered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='x',form='full',uels=f),squeeze=F)
  chk <- chkRgdxRes (x, xwantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'L',full,filtered,squeeze=F) failed",chk$msg))
  }
  # marginal
  f <- cart
  v <- array(0,c(iCard,jCard,kCard),f)
  v['i1',jUels,'k2'] <- 0.25
  xwantM <- list(name="x", type="variable", dim=3L,
                 val=v,
                 form="full",
                 uels=f, domains=userDomains,
                 field='m',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='x',form='full',uels=f,field='M'))
  chk <- chkRgdxRes (x, xwantM, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'M',full,filtered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='x',form='full',uels=f,field='M'),squeeze=F)
  chk <- chkRgdxRes (x, xwantM, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'M',full,filtered,squeeze=F) failed",chk$msg))
  }
  # lower
  f <- list(iUels,c('j2'),kUels)
  v <- array(0,c(iCard,1,kCard),dimnames=f)
  v['i1','j2','k1'] <- -Inf
  v['i1','j2','k2'] <- 100
  v['i2','j2','k2'] <- 6
  xwantLo <- list(name="x", type="variable", dim=3L,
                  val=v,
                  form="full",
                  uels=f, domains=userDomains,
                  field='lo',
                  varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='x',form='full',uels=f,field='lo'))
  chk <- chkRgdxRes (x, xwantLo, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'lo',full,filtered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='x',form='full',uels=f,field='lo'),squeeze=F)
  chk <- chkRgdxRes (x, xwantLo, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'lo',full,filtered,squeeze=F) failed",chk$msg))
  }
  # upper
  f <- cart
  v <- array(Inf,c(iCard,jCard,kCard),dimnames=f)
  v['i1','j1','k1'] <- 525
  v['i2','j1','k1'] <- 0
  v['i2','j2','k2'] <- 6
  xwantUp <- list(name="x", type="variable", dim=3L,
                  val=v,
                  form="full",
                  uels=f, domains=userDomains,
                  field='up',
                  varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='x',form='full',uels=f,field='up'))
  chk <- chkRgdxRes (x, xwantUp, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'up',full,filtered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='x',form='full',uels=f,field='up'),squeeze=F)
  chk <- chkRgdxRes (x, xwantUp, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'up',full,filtered,squeeze=F) failed",chk$msg))
  }
  # scale
  f <- cart
  v <- array(1,c(iCard,jCard,kCard),dimnames=f)
  v['i2','j2','k1'] <- 10
  xwantS <- list(name="x", type="variable", dim=3L,
                 val=v,
                 form="full",
                 uels=f, domains=userDomains,
                 field='s',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='x',form='full',uels=f,field='s'))
  chk <- chkRgdxRes (x, xwantS, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'s',full,filtered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='x',form='full',uels=f,field='s'),squeeze=F)
  chk <- chkRgdxRes (x, xwantS, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,'s',full,filtered,squeeze=F) failed",chk$msg))
  }


  print ("test of rgdx on variable reads passed")
  TRUE   ## all tests passed: return TRUE
},

error = function(ex) { print ("test of rgdx on variable reads failed"); print(ex) ; FALSE }
)
