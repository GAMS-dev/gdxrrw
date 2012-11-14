#### test rgdx reading 1-dim variables
#### test form=['sparse','full'] X [filtered,unfiltered] X squeeze=[T,F]
#### ['l','m','lo','up','s']

#### wanted lists can be produced with    dump("listName",file="")

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

source ("chkSame.R")
reqIdent <- TRUE

kUels <- c('k1', 'k2', 'k3', 'k4')
kCard <- length(kUels)
dom <- c('k')
userDom <- c('_user')
cart <- list(kUels)

tryCatch({
  print ("testing rgdx on 1-dim variable reads")
  rgdx('?')
  fnIn <- "tReadVar1.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }

  ### ---------- reading form=sparse, no filter
  # level
  uwantL <- list(name='u', type='variable', dim=1L,
                 val=matrix(c( 1,   5), nrow=1, ncol=2, byrow=T),
                 form='sparse', uels=cart, domains=dom,
                 field='l', varTypeText="integer", typeCode=GMS_VARTYPE$INTEGER)
  u <- rgdx(fnIn,list(name='u',form='sparse',field='L'))
  chk <- chkRgdxRes (u, uwantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'L',unfiltered) failed",chk$msg))
  }
  t <- matrix(c( 1,   5,
                 2,   0,
                 3,   0), nrow=3, ncol=2, byrow=T)
  uwantL$val <- t
  u <- rgdx(fnIn,list(name='u',form='sparse',field='L'),squeeze=F)
  chk <- chkRgdxRes (u, uwantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'L',unfiltered,squeeze=F) failed",chk$msg))
  }
  vwantL <- list(name='v', type='variable', dim=1L,
                 val=matrix(c( 2,   -2), nrow=1, ncol=2, byrow=T),
                 form='sparse', uels=cart, domains=dom,
                 field='l', varTypeText="negative", typeCode=GMS_VARTYPE$NEGATIVE)
  v <- rgdx(fnIn,list(name='v',form='sparse',field='L'))
  chk <- chkRgdxRes (v, vwantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'L',unfiltered) failed",chk$msg))
  }
  t <- matrix(c( 1,   0,
                 2,  -2), nrow=2, ncol=2, byrow=T)
  vwantL$val <- t
  v <- rgdx(fnIn,list(name='v',form='sparse',field='L'),squeeze=F)
  chk <- chkRgdxRes (v, vwantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'L',unfiltered,squeeze=F) failed",chk$msg))
  }
  # marginal
  uwantM <- list(name='u', type='variable', dim=1L,
                 val=matrix(c( 2, 1.5), nrow=1, ncol=2, byrow=T),
                 form='sparse', uels=cart, domains=dom,
                 field='m', varTypeText="integer", typeCode=GMS_VARTYPE$INTEGER)
  u <- rgdx(fnIn,list(name='u',form='sparse',field='M'))
  chk <- chkRgdxRes (u, uwantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'M',unfiltered) failed",chk$msg))
  }
  t <- matrix(c( 1,    0,
                 2,  1.5,
                 3,    0), nrow=3, ncol=2, byrow=T)
  uwantM$val <- t
  u <- rgdx(fnIn,list(name='u',form='sparse',field='M'),squeeze=F)
  chk <- chkRgdxRes (u, uwantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'M',unfiltered,squeeze=F) failed",chk$msg))
  }
  vwantM <- list(name='v', type='variable', dim=1L,
                 val=matrix(c( 2,  -20), nrow=1, ncol=2, byrow=T),
                 form='sparse', uels=cart, domains=dom,
                 field='m', varTypeText="negative", typeCode=GMS_VARTYPE$NEGATIVE)
  v <- rgdx(fnIn,list(name='v',form='sparse',field='M'))
  chk <- chkRgdxRes (v, vwantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'M',unfiltered) failed",chk$msg))
  }
  # lower
  uwantLo <- list(name='u', type='variable', dim=1L,
                  val=matrix(c( 1, 5), nrow=1, ncol=2, byrow=T),
                  form='sparse', uels=cart, domains=dom,
                  field='lo', varTypeText="integer", typeCode=GMS_VARTYPE$INTEGER)
  u <- rgdx(fnIn,list(name='u',form='sparse',field='Lo'))
  chk <- chkRgdxRes (u, uwantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'lo',unfiltered) failed",chk$msg))
  }
  vwantLo <- list(name='v', type='variable', dim=1L,
                  val=matrix(c(2,  -2), nrow=1, ncol=2, byrow=T),
                  form='sparse', uels=cart, domains=dom,
                  field='lo', varTypeText="negative", typeCode=GMS_VARTYPE$NEGATIVE)
  v <- rgdx(fnIn,list(name='v',form='sparse',field='lo'))
  chk <- chkRgdxRes (v, vwantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'lo',unfiltered) failed",chk$msg))
  }
  vwantLo$val <- matrix(c( 1,  -Inf,
                           2,  -2  ), nrow=2, ncol=2, byrow=T)
  v <- rgdx(fnIn,list(name='v',form='sparse',field='lo'),squeeze=F)
  chk <- chkRgdxRes (v, vwantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'lo',unfiltered,squeeze=F) failed",chk$msg))
  }
  # upper
  uwantUp <- list(name='u', type='variable', dim=1L,
                  val=matrix(c( 1,  5,
                                2, 15,
                                3, 15), nrow=3, ncol=2, byrow=T),
                  form='sparse', uels=cart, domains=dom,
                  field='up', varTypeText="integer", typeCode=GMS_VARTYPE$INTEGER)
  u <- rgdx(fnIn,list(name='u',form='sparse',field='Up'))
  chk <- chkRgdxRes (u, uwantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'up',unfiltered) failed",chk$msg))
  }
  u <- rgdx(fnIn,list(name='u',form='sparse',field='Up'),squeeze=F)
  chk <- chkRgdxRes (u, uwantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'up',unfiltered,squeeze=F) failed",chk$msg))
  }
  vwantUp <- list(name='v', type='variable', dim=1L,
                  val=matrix(c( 1,  +Inf,
                                2,  -2  ), nrow=2, ncol=2, byrow=T),
                  form='sparse', uels=cart, domains=dom,
                  field='up', varTypeText="negative", typeCode=GMS_VARTYPE$NEGATIVE)
  v <- rgdx(fnIn,list(name='v',form='sparse',field='up'))
  chk <- chkRgdxRes (v, vwantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'up',unfiltered) failed",chk$msg))
  }
  v <- rgdx(fnIn,list(name='v',form='sparse',field='up'),squeeze=F)
  chk <- chkRgdxRes (v, vwantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'up',unfiltered,squeeze=F) failed",chk$msg))
  }
  # scale
  
  uwantS <- list(name='u', type='variable', dim=1L,
                 val=matrix(0,nrow=0, ncol=2, byrow=T),
                 form='sparse', uels=cart, domains=dom,
                 field='s', varTypeText="integer", typeCode=GMS_VARTYPE$INTEGER)
  u <- rgdx(fnIn,list(name='u',form='sparse',field='S'))
  chk <- chkRgdxRes (u, uwantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'S',unfiltered) failed",chk$msg))
  }
  u <- rgdx(fnIn,list(name='u',form='sparse',field='S'))
  chk <- chkRgdxRes (u, uwantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'S',unfiltered) failed",chk$msg))
  }
  vwantS <- list(name='v', type='variable', dim=1L,
                 val=matrix(0,nrow=0, ncol=2, byrow=T),
                 form='sparse', uels=cart, domains=dom,
                 field='s', varTypeText="negative", typeCode=GMS_VARTYPE$NEGATIVE)
  v <- rgdx(fnIn,list(name='v',form='sparse',field='s'))
  chk <- chkRgdxRes (v, vwantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'S',unfiltered) failed",chk$msg))
  }
  vwantS$val <- matrix(c( 1,  1,
                          2,  1), nrow=2, ncol=2, byrow=T)
  v <- rgdx(fnIn,list(name='v',form='sparse',field='s'),squeeze=F)
  chk <- chkRgdxRes (v, vwantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'S',unfiltered,squeeze=F) failed",chk$msg))
  }

  ### ---------- reading form=sparse, filtered
  # level
  f <- list(c('k1','k3','k4'))
  uwantL <- list(name='u', type='variable', dim=1L,
                 val=matrix(c( 1,   5), nrow=1, ncol=2, byrow=T),
                 form='sparse', uels=f, domains=userDom,
                 field='l', varTypeText="integer", typeCode=GMS_VARTYPE$INTEGER)
  u <- rgdx(fnIn,list(name='u',form='sparse',uels=f))
  chk <- chkRgdxRes (u, uwantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'L',filtered) failed",chk$msg))
  }
  uwantL$val <- matrix(c( 1,   5,
                          2,   0), nrow=2, ncol=2, byrow=T)
  u <- rgdx(fnIn,list(name='u',form='sparse',uels=f),squeeze=F)
  chk <- chkRgdxRes (u, uwantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'L',filtered,squeeze=F) failed",chk$msg))
  }
  f <- list(c('k2','k3'))
  vwantL <- list(name='v', type='variable', dim=1L,
                 val=matrix(c( 1,  -2), nrow=1, ncol=2, byrow=T),
                 form='sparse', uels=f, domains=userDom,
                 field='l', varTypeText="negative", typeCode=GMS_VARTYPE$NEGATIVE)
  v <- rgdx(fnIn,list(name='v',form='sparse',uels=f))
  chk <- chkRgdxRes (v, vwantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'L',filtered) failed",chk$msg))
  }
  v <- rgdx(fnIn,list(name='v',form='sparse',uels=f),squeeze=F)
  chk <- chkRgdxRes (v, vwantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'L',filtered,squeeze=F) failed",chk$msg))
  }
  # marginal
  f <- list(c('k1','k3','k4'))
  uwantM <- list(name='u', type='variable', dim=1L,
                 val=matrix(0, nrow=0, ncol=2, byrow=T),
                 form='sparse', uels=f, domains=userDom,
                 field='m', varTypeText="integer", typeCode=GMS_VARTYPE$INTEGER)
  u <- rgdx(fnIn,list(name='u',form='sparse',uels=f,field='M'))
  chk <- chkRgdxRes (u, uwantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'M',filtered) failed",chk$msg))
  }
  uwantM$val <- matrix(c( 1,   0,
                          2,   0), nrow=2, ncol=2, byrow=T)
  u <- rgdx(fnIn,list(name='u',form='sparse',uels=f,field='M'),squeeze=F)
  chk <- chkRgdxRes (u, uwantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'M',filtered,squeeze=F) failed",chk$msg))
  }
  f <- list(c('k2','k3'))
  vwantM <- list(name='v', type='variable', dim=1L,
                 val=matrix(c( 1, -20), nrow=1, ncol=2, byrow=T),
                 form='sparse', uels=f, domains=userDom,
                 field='m', varTypeText="negative", typeCode=GMS_VARTYPE$NEGATIVE)
  v <- rgdx(fnIn,list(name='v',form='sparse',uels=f,field='M'))
  chk <- chkRgdxRes (v, vwantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'M',filtered) failed",chk$msg))
  }
  v <- rgdx(fnIn,list(name='v',form='sparse',uels=f,field='M'),squeeze=F)
  chk <- chkRgdxRes (v, vwantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'M',filtered,squeeze=F) failed",chk$msg))
  }
  # lower
  f <- list(c('k1','k3','k4'))
  uwantLo <- list(name='u', type='variable', dim=1L,
                  val=matrix(c(1,   5), nrow=1, ncol=2, byrow=T),
                  form='sparse', uels=f, domains=userDom,
                  field='lo', varTypeText="integer", typeCode=GMS_VARTYPE$INTEGER)
  u <- rgdx(fnIn,list(name='u',form='sparse',uels=f,field='Lo'))
  chk <- chkRgdxRes (u, uwantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'lo',filtered) failed",chk$msg))
  }
  uwantLo$val <- matrix(c(1,   5,
                          2,   0), nrow=2, ncol=2, byrow=T)
  u <- rgdx(fnIn,list(name='u',form='sparse',uels=f,field='Lo'),squeeze=F)
  chk <- chkRgdxRes (u, uwantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'lo',filtered,squeeze=F) failed",chk$msg))
  }
  f <- list(c('k2','k3'))
  vwantLo <- list(name='v', type='variable', dim=1L,
                  val=matrix(c( 1, -2), nrow=1, ncol=2, byrow=T),
                  form='sparse', uels=f, domains=userDom,
                  field='lo', varTypeText="negative", typeCode=GMS_VARTYPE$NEGATIVE)
  v <- rgdx(fnIn,list(name='v',form='sparse',uels=f,field='lo'))
  chk <- chkRgdxRes (v, vwantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'lo',filtered) failed",chk$msg))
  }
  v <- rgdx(fnIn,list(name='v',form='sparse',uels=f,field='lo'),squeeze=F)
  chk <- chkRgdxRes (v, vwantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'lo',filtered,squeeze=F) failed",chk$msg))
  }
  # upper
  f <- list(c('k1','k3','k4'))
  uwantUp <- list(name='u', type='variable', dim=1L,
                  val=matrix(c( 1,   5,
                                2,  15), nrow=2, ncol=2, byrow=T),
                  form='sparse', uels=f, domains=userDom,
                  field='up', varTypeText="integer", typeCode=GMS_VARTYPE$INTEGER)
  u <- rgdx(fnIn,list(name='u',form='sparse',uels=f,field='uP'))
  chk <- chkRgdxRes (u, uwantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'up',filtered) failed",chk$msg))
  }
  u <- rgdx(fnIn,list(name='u',form='sparse',uels=f,field='uP'),squeeze=F)
  chk <- chkRgdxRes (u, uwantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'up',filtered,squeeze=F) failed",chk$msg))
  }
  f <- list(c('k2','k3'))
  vwantUp <- list(name='v', type='variable', dim=1L,
                  val=matrix(c( 1, -2), nrow=1, ncol=2, byrow=T),
                  form='sparse', uels=f, domains=userDom,
                  field='up', varTypeText="negative", typeCode=GMS_VARTYPE$NEGATIVE)
  v <- rgdx(fnIn,list(name='v',form='sparse',uels=f,field='up'))
  chk <- chkRgdxRes (v, vwantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'up',filtered) failed",chk$msg))
  }
  v <- rgdx(fnIn,list(name='v',form='sparse',uels=f,field='up'),squeeze=F)
  chk <- chkRgdxRes (v, vwantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'up',filtered,squeeze=F) failed",chk$msg))
  }
  # scale
  f <- list(c('k1','k3','k4'))
  uwantS <- list(name='u', type='variable', dim=1L,
                 val=matrix(0, nrow=0, ncol=2, byrow=T),
                 form='sparse', uels=f, domains=userDom,
                 field='s', varTypeText="integer", typeCode=GMS_VARTYPE$INTEGER)
  u <- rgdx(fnIn,list(name='u',form='sparse',uels=f,field='S'))
  chk <- chkRgdxRes (u, uwantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'s',filtered) failed",chk$msg))
  }
  uwantS$val <- matrix(c(1,   1,
                         2,   1), nrow=2, ncol=2, byrow=T)
  u <- rgdx(fnIn,list(name='u',form='sparse',uels=f,field='S'),squeeze=F)
  chk <- chkRgdxRes (u, uwantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'s',filtered,squeeze=F) failed",chk$msg))
  }
  f <- list(c('k2','k3'))
  vwantS <- list(name='v', type='variable', dim=1L,
                 val=matrix(0, nrow=0, ncol=2, byrow=T),
                 form='sparse', uels=f, domains=userDom,
                 field='s', varTypeText="negative", typeCode=GMS_VARTYPE$NEGATIVE)
  v <- rgdx(fnIn,list(name='v',form='sparse',uels=f,field='s'))
  chk <- chkRgdxRes (v, vwantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'s',filtered) failed",chk$msg))
  }
  vwantS$val <- matrix(c( 1,  1), nrow=1, ncol=2, byrow=T)
  v <- rgdx(fnIn,list(name='v',form='sparse',uels=f,field='s'),squeeze=F)
  chk <- chkRgdxRes (v, vwantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'s',filtered,squeeze=F) failed",chk$msg))
  }

  ### ---------- reading form=full, no filter
  # level
  t <- array(0,c(kCard,1),dimnames=cart)
  t['k1',1] <- 5
  uwantL <- list(name='u', type='variable', dim=1L,
                 val=t,
                 form='full', uels=cart, domains=dom,
                 field='l', varTypeText="integer", typeCode=GMS_VARTYPE$INTEGER)
  u <- rgdx(fnIn,list(name='u',form='full'))
  chk <- chkRgdxRes (u, uwantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'L',full,unfiltered) failed",chk$msg))
  }
  u <- rgdx(fnIn,list(name='u',form='full'),squeeze=F)
  chk <- chkRgdxRes (u, uwantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'L',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  t <- array(0,c(kCard,1),dimnames=cart)
  t['k2',1] <- -2
  vwantL <- list(name='v', type='variable', dim=1L,
                 val=t,
                 form='full', uels=cart, domains=dom,
                 field='l', varTypeText="negative", typeCode=GMS_VARTYPE$NEGATIVE)
  v <- rgdx(fnIn,list(name='v',form='full'))
  chk <- chkRgdxRes (v, vwantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'L',full,unfiltered) failed",chk$msg))
  }
  v <- rgdx(fnIn,list(name='v',form='full'),squeeze=F)
  chk <- chkRgdxRes (v, vwantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'L',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  # marginal
  t <- array(0,c(kCard,1),dimnames=cart)
  t['k2',1] <- 1.5
  uwantM <- list(name='u', type='variable', dim=1L,
                 val=t,
                 form='full', uels=cart, domains=dom,
                 field='m', varTypeText="integer", typeCode=GMS_VARTYPE$INTEGER)
  u <- rgdx(fnIn,list(name='u',form='full',field='M'))
  chk <- chkRgdxRes (u, uwantM, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'M',full,unfiltered) failed",chk$msg))
  }
  u <- rgdx(fnIn,list(name='u',form='full',field='M'),squeeze=F)
  chk <- chkRgdxRes (u, uwantM, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'M',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  t <- array(0,c(kCard,1),dimnames=cart)
  t['k2',1] <- -20
  vwantM <- list(name='v', type='variable', dim=1L,
                 val=t,
                 form='full', uels=cart, domains=dom,
                 field='m', varTypeText="negative", typeCode=GMS_VARTYPE$NEGATIVE)
  v <- rgdx(fnIn,list(name='v',form='full',field='m'))
  chk <- chkRgdxRes (v, vwantM, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'M',full,unfiltered) failed",chk$msg))
  }
  v <- rgdx(fnIn,list(name='v',form='full',field='m'),squeeze=F)
  chk <- chkRgdxRes (v, vwantM, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'M',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  # lower
  t <- array(0,c(kCard,1),dimnames=cart)
  t['k1',1] <- 5
  uwantLo <- list(name='u', type='variable', dim=1L,
                  val=t,
                  form='full', uels=cart, domains=dom,
                  field='lo', varTypeText="integer", typeCode=GMS_VARTYPE$INTEGER)
  u <- rgdx(fnIn,list(name='u',form='full',field='LO'))
  chk <- chkRgdxRes (u, uwantLo, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'lo',full,unfiltered) failed",chk$msg))
  }
  u <- rgdx(fnIn,list(name='u',form='full',field='LO'),squeeze=F)
  chk <- chkRgdxRes (u, uwantLo, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'lo',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  t <- array(-Inf,c(kCard,1),dimnames=cart)
  t['k2',1] <- -2
  vwantLo <- list(name='v', type='variable', dim=1L,
                  val=t,
                  form='full', uels=cart, domains=dom,
                  field='lo', varTypeText="negative", typeCode=GMS_VARTYPE$NEGATIVE)
  v <- rgdx(fnIn,list(name='v',form='full',field='lo'))
  chk <- chkRgdxRes (v, vwantLo, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'lo',full,unfiltered) failed",chk$msg))
  }
  v <- rgdx(fnIn,list(name='v',form='full',field='lo'),squeeze=F)
  chk <- chkRgdxRes (v, vwantLo, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'lo',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  # upper
  t <- array(100,c(kCard,1),dimnames=cart)
  t['k1',1] <- 5
  t['k2',1] <- 15
  t['k3',1] <- 15
  uwantUp <- list(name='u', type='variable', dim=1L,
                  val=t,
                  form='full', uels=cart, domains=dom,
                  field='up', varTypeText="integer", typeCode=GMS_VARTYPE$INTEGER)
  u <- rgdx(fnIn,list(name='u',form='full',field='UP'))
  chk <- chkRgdxRes (u, uwantUp, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'up',full,unfiltered) failed",chk$msg))
  }
  u <- rgdx(fnIn,list(name='u',form='full',field='UP'),squeeze=F)
  chk <- chkRgdxRes (u, uwantUp, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'up',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  t <- array(0,c(kCard,1),dimnames=cart)
  t['k1',1] <- +Inf
  t['k2',1] <- -2
  vwantUp <- list(name='v', type='variable', dim=1L,
                  val=t,
                  form='full', uels=cart, domains=dom,
                  field='up', varTypeText="negative", typeCode=GMS_VARTYPE$NEGATIVE)
  v <- rgdx(fnIn,list(name='v',form='full',field='up'))
  chk <- chkRgdxRes (v, vwantUp, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'up',full,unfiltered) failed",chk$msg))
  }
  v <- rgdx(fnIn,list(name='v',form='full',field='up'),squeeze=F)
  chk <- chkRgdxRes (v, vwantUp, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'up',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  # scale
  t <- array(1,c(kCard,1),dimnames=cart)
  t['k3',1] <- 1
  uwantS <- list(name='u', type='variable', dim=1L,
                 val=t,
                 form='full', uels=cart, domains=dom,
                 field='s', varTypeText="integer", typeCode=GMS_VARTYPE$INTEGER)
  u <- rgdx(fnIn,list(name='u',form='full',field='S'))
  chk <- chkRgdxRes (u, uwantS, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'s',full,unfiltered) failed",chk$msg))
  }
  u <- rgdx(fnIn,list(name='u',form='full',field='S'),squeeze=F)
  chk <- chkRgdxRes (u, uwantS, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'s',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  t <- array(1,c(kCard,1),dimnames=cart)
  t['k1',1] <- 1
  vwantS <- list(name='v', type='variable', dim=1L,
                 val=t,
                 form='full', uels=cart, domains=dom,
                 field='s', varTypeText="negative", typeCode=GMS_VARTYPE$NEGATIVE)
  v <- rgdx(fnIn,list(name='v',form='full',field='s'))
  chk <- chkRgdxRes (v, vwantS, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'S',full,unfiltered) failed",chk$msg))
  }
  v <- rgdx(fnIn,list(name='v',form='full',field='s'),squeeze=F)
  chk <- chkRgdxRes (v, vwantS, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'S',full,unfiltered,squeeze=F) failed",chk$msg))
  }

  ### ---------- reading form=full, filtered
  # level
  f <- list(c('k1','k3','k4'))
  t <- array(0,c(3,1),dimnames=f)
  t['k1',1] <- 5
  uwantL <- list(name='u', type='variable', dim=1L,
                 val=t,
                 form='full', uels=f, domains=userDom,
                 field='l', varTypeText="integer", typeCode=GMS_VARTYPE$INTEGER)
  u <- rgdx(fnIn,list(name='u',form='full',uels=f))
  chk <- chkRgdxRes (u, uwantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'L',full,filtered) failed",chk$msg))
  }
  u <- rgdx(fnIn,list(name='u',form='full',uels=f),squeeze=F)
  chk <- chkRgdxRes (u, uwantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'L',full,filtered,squeeze=F) failed",chk$msg))
  }
  f <- list(c('k1','k2','k3','k4'))
  t <- array(0,c(4,1),dimnames=f)
  t['k2',1] <- -2
  vwantL <- list(name='v', type='variable', dim=1L,
                 val=t,
                 form='full', uels=f, domains=userDom,
                 field='l', varTypeText="negative", typeCode=GMS_VARTYPE$NEGATIVE)
  v <- rgdx(fnIn,list(name='v',form='full',uels=f))
  chk <- chkRgdxRes (v, vwantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'L',full,filtered) failed",chk$msg))
  }
  v <- rgdx(fnIn,list(name='v',form='full',uels=f),squeeze=F)
  chk <- chkRgdxRes (v, vwantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'L',full,filtered,squeeze=F) failed",chk$msg))
  }
  # marginal
  f <- list(c('k1','k3','k4'))
  t <- array(0,c(3,1),dimnames=f)
  uwantM <- list(name='u', type='variable', dim=1L,
                 val=t,
                 form='full', uels=f, domains=userDom,
                 field='m', varTypeText="integer", typeCode=GMS_VARTYPE$INTEGER)
  u <- rgdx(fnIn,list(name='u',form='full',uels=f,field='M'))
  chk <- chkRgdxRes (u, uwantM, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'M',full,filtered) failed",chk$msg))
  }
  u <- rgdx(fnIn,list(name='u',form='full',uels=f,field='M'),squeeze=F)
  chk <- chkRgdxRes (u, uwantM, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'M',full,filtered,squeeze=F) failed",chk$msg))
  }
  f <- list(c('k1','k2','k3','k4'))
  t <- array(0,c(4,1),dimnames=f)
  t['k2',1] <- -20
  vwantM <- list(name='v', type='variable', dim=1L,
                 val=t,
                 form='full', uels=f, domains=userDom,
                 field='m', varTypeText="negative", typeCode=GMS_VARTYPE$NEGATIVE)
  v <- rgdx(fnIn,list(name='v',form='full',uels=f,field='m'))
  chk <- chkRgdxRes (v, vwantM, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'M',full,filtered) failed",chk$msg))
  }
  v <- rgdx(fnIn,list(name='v',form='full',uels=f,field='m'),squeeze=F)
  chk <- chkRgdxRes (v, vwantM, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'M',full,filtered,squeeze=F) failed",chk$msg))
  }
  # lower
  f <- list(c('k1','k3','k4'))
  t <- array(0,c(3,1),dimnames=f)
  t['k1',1] <- 5
  uwantLo <- list(name='u', type='variable', dim=1L,
                  val=t,
                  form='full', uels=f, domains=userDom,
                  field='lo', varTypeText="integer", typeCode=GMS_VARTYPE$INTEGER)
  u <- rgdx(fnIn,list(name='u',form='full',uels=f,field='LO'))
  chk <- chkRgdxRes (u, uwantLo, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'lo',full,filtered) failed",chk$msg))
  }
  u <- rgdx(fnIn,list(name='u',form='full',uels=f,field='LO'),squeeze=F)
  chk <- chkRgdxRes (u, uwantLo, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'lo',full,filtered,squeeze=F) failed",chk$msg))
  }
  f <- list(c('k1','k2','k3','k4'))
  t <- array(-Inf,c(4,1),dimnames=f)
  t['k1',1] <- -Inf
  t['k2',1] <- -2
  vwantLo <- list(name='v', type='variable', dim=1L,
                  val=t,
                  form='full', uels=f, domains=userDom,
                  field='lo', varTypeText="negative", typeCode=GMS_VARTYPE$NEGATIVE)
  v <- rgdx(fnIn,list(name='v',form='full',uels=f,field='lo'))
  chk <- chkRgdxRes (v, vwantLo, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'lo',full,filtered) failed",chk$msg))
  }
  v <- rgdx(fnIn,list(name='v',form='full',uels=f,field='lo'),squeeze=F)
  chk <- chkRgdxRes (v, vwantLo, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'lo',full,filtered,squeeze=F) failed",chk$msg))
  }
  # upper
  f <- list(c('k1','k3','k4'))
  t <- array(100,c(3,1),dimnames=f)
  t['k1',1] <-  5
  t['k3',1] <- 15
  uwantUp <- list(name='u', type='variable', dim=1L,
                  val=t,
                  form='full', uels=f, domains=userDom,
                  field='up', varTypeText="integer", typeCode=GMS_VARTYPE$INTEGER)
  u <- rgdx(fnIn,list(name='u',form='full',uels=f,field='UP'))
  chk <- chkRgdxRes (u, uwantUp, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'up',full,filtered) failed",chk$msg))
  }
  u <- rgdx(fnIn,list(name='u',form='full',uels=f,field='UP'),squeeze=F)
  chk <- chkRgdxRes (u, uwantUp, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'up',full,filtered,squeeze=F) failed",chk$msg))
  }
  f <- list(c('k1','k2','k3','k4'))
  t <- array(0,c(4,1),dimnames=f)
  t['k1',1] <- Inf
  t['k2',1] <- -2
  vwantUp <- list(name='v', type='variable', dim=1L,
                  val=t,
                  form='full', uels=f, domains=userDom,
                  field='up', varTypeText="negative", typeCode=GMS_VARTYPE$NEGATIVE)
  v <- rgdx(fnIn,list(name='v',form='full',uels=f,field='up'))
  chk <- chkRgdxRes (v, vwantUp, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'up',full,filtered) failed",chk$msg))
  }
  v <- rgdx(fnIn,list(name='v',form='full',uels=f,field='up'),squeeze=F)
  chk <- chkRgdxRes (v, vwantUp, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'up',full,filtered,squeeze=F) failed",chk$msg))
  }
  # scale
  f <- list(c('k1','k3','k4'))
  t <- array(1,c(3,1),dimnames=f)
  uwantS <- list(name='u', type='variable', dim=1L,
                 val=t,
                 form='full', uels=f, domains=userDom,
                 field='s', varTypeText="integer", typeCode=GMS_VARTYPE$INTEGER)
  u <- rgdx(fnIn,list(name='u',form='full',uels=f,field='S'))
  chk <- chkRgdxRes (u, uwantS, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'S',full,filtered) failed",chk$msg))
  }
  u <- rgdx(fnIn,list(name='u',form='full',uels=f,field='S'),squeeze=F)
  chk <- chkRgdxRes (u, uwantS, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(u,'S',full,filtered,squeeze=F) failed",chk$msg))
  }
  f <- list(c('k1','k2','k3','k4'))
  t <- array(1,c(4,1),dimnames=f)
  vwantS <- list(name='v', type='variable', dim=1L,
                 val=t,
                 form='full', uels=f, domains=userDom,
                 field='s', varTypeText="negative", typeCode=GMS_VARTYPE$NEGATIVE)
  v <- rgdx(fnIn,list(name='v',form='full',uels=f,field='s'))
  chk <- chkRgdxRes (v, vwantS, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'s',full,filtered) failed",chk$msg))
  }
  v <- rgdx(fnIn,list(name='v',form='full',uels=f,field='s'),squeeze=F)
  chk <- chkRgdxRes (v, vwantS, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(v,'s',full,filtered,squeeze=F) failed",chk$msg))
  }


  print ("test of rgdx on variable reads passed")
  TRUE   ## all tests passed: return TRUE
},

error = function(ex) { print ("test of rgdx on variable reads failed"); print(ex) ; FALSE }
)
