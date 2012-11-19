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
userDom <- c('_user')
userDomf <- c('_user','_field')
kUels <- c('k1')
kCard <- length(kUels)
domK <- c('K')
domKF <- c('K','_field')
cartK <- list(kUels)
cartKF <- list(kUels,fields)

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
                 val=matrix(0, nrow=0, ncol=1),
                 form='sparse', uels=list(), domains=character(0),
                 field='l', typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',field='L'))
  chk <- chkRgdxRes (e0, e0wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'L',unfiltered) failed",chk$msg))
  }
  e0wantL$val <- matrix(0, nrow=1, ncol=1)
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',field='L'),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'L',unfiltered,squeeze=F) failed",chk$msg))
  }
  # marginal
  e0wantM <- list(name='e0', type='equation', dim=0L,
                 val=matrix(1, nrow=1, ncol=1),
                 form='sparse', uels=list(), domains=character(0),
                 field='m', typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',field='M'))
  chk <- chkRgdxRes (e0, e0wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'M',unfiltered) failed",chk$msg))
  }
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',field='M'),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'M',unfiltered,squeeze=F) failed",chk$msg))
  }
  # lower
  e0wantLo <- list(name='e0', type='equation', dim=0L,
                   val=matrix(0, nrow=0, ncol=1),
                   form='sparse', uels=list(), domains=character(0),
                   field='lo', typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',field='lo'))
  chk <- chkRgdxRes (e0, e0wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'lo',unfiltered) failed",chk$msg))
  }
  e0wantLo$val <- matrix(0, nrow=1, ncol=1)
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',field='lo'),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'lo',unfiltered,squeeze=F) failed",chk$msg))
  }
  # upper
  e0wantUp <- list(name='e0', type='equation', dim=0L,
                   val=matrix(0, nrow=0, ncol=1),
                   form='sparse', uels=list(), domains=character(0),
                   field='up', typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',field='UP'))
  chk <- chkRgdxRes (e0, e0wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'up',unfiltered) failed",chk$msg))
  }
  e0wantUp$val <- matrix(0, nrow=1, ncol=1)
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',field='UP'),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'up',unfiltered,squeeze=F) failed",chk$msg))
  }
  # scale
  e0wantS <- list(name='e0', type='equation', dim=0L,
                  val=matrix(1, nrow=0, ncol=1),
                  form='sparse', uels=list(), domains=character(0),
                  field='s', typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',field='s'))
  chk <- chkRgdxRes (e0, e0wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'s',unfiltered) failed",chk$msg))
  }
  e0wantS$val <- matrix(1, nrow=1, ncol=1)
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',field='s'),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'s',unfiltered,squeeze=F) failed",chk$msg))
  }

  ### ---------- reading form=sparse, filtered
  f <- list(kUels)
  # level
  e1wantL <- list(name='e1', type='equation', dim=1L,
                  val=matrix(c(1, -2), nrow=1, ncol=2),
                  form='sparse', uels=f, domains=userDom,
                  field='l', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',uels=f))
  chk <- chkRgdxRes (e1, e1wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'L',filtered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',uels=f),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'L',filtered,squeeze=F) failed",chk$msg))
  }
  # marginal
  e1wantM <- list(name='e1', type='equation', dim=1L,
                  val=matrix(c(1, -3.5), nrow=1, ncol=2),
                  form='sparse', uels=f, domains=userDom,
                  field='m', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='m',uels=f))
  chk <- chkRgdxRes (e1, e1wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'m',filtered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='m',uels=f),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'m',filtered,squeeze=F) failed",chk$msg))
  }
  # lower
  e1wantLo <- list(name='e1', type='equation', dim=1L,
                   val=matrix(c(1, -2), nrow=1, ncol=2),
                   form='sparse', uels=f, domains=userDom,
                   field='lo', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='lo',uels=f))
  chk <- chkRgdxRes (e1, e1wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'lo',filtered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='lo',uels=f),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'lo',filtered,squeeze=F) failed",chk$msg))
  }
  # upper
  e1wantUp <- list(name='e1', type='equation', dim=1L,
                   val=matrix(Inf, nrow=0, ncol=2),
                   form='sparse', uels=f, domains=userDom,
                   field='up', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='up',uels=f))
  chk <- chkRgdxRes (e1, e1wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'up',filtered) failed",chk$msg))
  }
  e1wantUp$val <- matrix(c(1, +Inf), nrow=1, ncol=2)
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='up',uels=f),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'up',filtered,squeeze=F) failed",chk$msg))
  }
  # scale
  e1wantS <- list(name='e1', type='equation', dim=1L,
                  val=matrix(c(1,2), nrow=1, ncol=2),
                  form='sparse', uels=f, domains=userDom,
                  field='s', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='S',uels=f))
  chk <- chkRgdxRes (e1, e1wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'s',filtered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='S',uels=f),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'s',filtered,squeeze=F) failed",chk$msg))
  }

  ### ---------- reading form=full, no filter
  # level
  t <- array(0,c(kCard,1),dimnames=cartK)
  t['k1',1] <- -2
  e1wantL <- list(name='e1', type='equation', dim=1L,
                  val=t,
                  form='full', uels=cartK, domains=domK,
                  field='l', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='full',field='L'))
  chk <- chkRgdxRes (e1, e1wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'L',full,unfiltered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',form='full',field='L'),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'L',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  # marginal
  t <- array(0,c(kCard,1),dimnames=cartK)
  t['k1',1] <- -3.5
  e1wantM <- list(name='e1', type='equation', dim=1L,
                  val=t,
                  form='full', uels=cartK, domains=domK,
                  field='m', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='full',field='M'))
  chk <- chkRgdxRes (e1, e1wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'M',full,unfiltered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',form='full',field='M'),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'M',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  # lower
  t <- array(0,c(kCard,1),dimnames=cartK)
  t['k1',1] <- -2
  e1wantLo <- list(name='e1', type='equation', dim=1L,
                   val=t,
                   form='full', uels=cartK, domains=domK,
                   field='lo', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='full',field='Lo'))
  chk <- chkRgdxRes (e1, e1wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'LO',full,unfiltered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',form='full',field='Lo'),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'LO',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  # upper
  t <- array(0,c(kCard,1),dimnames=cartK)
  t['k1',1] <- Inf
  e1wantUp <- list(name='e1', type='equation', dim=1L,
                   val=t,
                   form='full', uels=cartK, domains=domK,
                   field='up', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='full',field='up'))
  chk <- chkRgdxRes (e1, e1wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'up',full,unfiltered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',form='full',field='up'),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'up',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  # scale
  t <- array(0,c(kCard,1),dimnames=cartK)
  t['k1',1] <- 2
  e1wantS <- list(name='e1', type='equation', dim=1L,
                  val=t,
                  form='full', uels=cartK, domains=domK,
                  field='s', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='full',field='s'))
  chk <- chkRgdxRes (e1, e1wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'s',full,unfiltered) failed",chk$msg))
  }


  ### ---------- reading form=full, filtered
  f <- list(kUels)
  # level
  t <- array(0,c(kCard,1),dimnames=cartK)
  t['k1',1] <- -2
  e1wantL <- list(name='e1', type='equation', dim=1L,
                  val=t,
                  form='full', uels=f, domains=userDom,
                  field='l', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',uels=f,form='full',field='L'))
  chk <- chkRgdxRes (e1, e1wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'L',full,filtered) failed",chk$msg))
  }


  print ("test of rgdx on equation reads passed")
  TRUE   ## all tests passed: return TRUE
},

error = function(ex) { print ("test of rgdx on equation reads failed"); print(ex) ; FALSE }
)
