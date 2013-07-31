#### test rgdx reading equations
#### test form=['sparse','full'] X [filtered,unfiltered] X squeeze=[T(default),F]
#### ['all','l','m','lo','up','s']

## still to do:
##  # form=full  , filtered, all

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
userDom1 <- c('_user')
userDom1f <- c('_user','_field')
userDom3 <- c('_user','_user','_user')
userDom3f <- c('_user','_user','_user','_field')
iUels <- c('i1')
iCard <- length(iUels)
jUels <- c('j1','j2')
jCard <- length(jUels)
kUels <- c('k1')
kCard <- length(kUels)
domF <- c('_field')
domK <- c('K')
domKF <- c('K','_field')
domIJK <- c('I','J','K')
domIJKF <- c('I','J','K','_field')
cartK <- list(kUels)
cartKF <- list(kUels,fields)
cartIJK <- list(iUels,jUels,kUels)
cartIJKF <- list(iUels,jUels,kUels,fields)

tryCatch({
  print ("test of rgdx on equation reads")
  rgdx('?')
  fnIn <- "tReadEqu.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }

  ### ---------- reading form=sparse, no filter
  # all
  t <- matrix(c( lev,  0
                ,mar,  1
                ,low,  0
                ,upp,  0
                ,sca,  1
               ), nrow=nFields, ncol=2, byrow=T)
  e0wantA <- list(name='e0', type='equation', dim=0L,
                 val=t,
                 form='sparse', uels=list(fields), domains=domF,
                 field='all',
                 typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',field='all'))
  chk <- chkRgdxRes (e0, e0wantA, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'all',unfiltered) failed",chk$msg))
  }
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',field='all'),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantA, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'all',unfiltered,squeeze=F) failed",chk$msg))
  }
  t <- matrix(c( 1, lev,   -2
                ,1, mar,   -3.5
                ,1, low,   -2
                ,1, upp,   Inf
                ,1, sca,   2
             ), nrow=5, ncol=3, byrow=T)
  e1wantA <- list(name='e1', type='equation', dim=1L,
                 val=t,
                 form='sparse', uels=cartKF, domains=domKF,
                 field='all', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='ALL'))
  chk <- chkRgdxRes (e1, e1wantA, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'all',unfiltered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='all'),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantA, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'all',unfiltered,squeeze=F) failed",chk$msg))
  }
  t <- matrix(c( 1,1,1,lev,   3
                ,1,1,1,mar,   0
                ,1,1,1,low,   -Inf
                ,1,1,1,upp,   4
                ,1,1,1,sca,   1
                ,1,2,1,lev,   4
                ,1,2,1,mar,   0.5
                ,1,2,1,low,   -Inf
                ,1,2,1,upp,   4
                ,1,2,1,sca,   1
               ), nrow=10, ncol=5, byrow=T)
  e3wantA <- list(name="e3", type="equation", dim=3L,
                 val=t,
                 form="sparse",
                 uels=cartIJKF, domains=domIJKF,
                 field='all',
                 typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',field='alL'))
  chk <- chkRgdxRes (e3, e3wantA, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'all',unfiltered) failed",chk$msg))
  }
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',field='alL'),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantA, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'all',unfiltered,squeeze=F) failed",chk$msg))
  }
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
  e1wantL <- list(name='e1', type='equation', dim=1L,
                 val=matrix(c(1,  -2), nrow=1, ncol=2),
                 form='sparse', uels=cartK, domains=domK,
                 field='l', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='L'))
  chk <- chkRgdxRes (e1, e1wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'L',unfiltered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='L'),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'L',unfiltered,squeeze=F) failed",chk$msg))
  }
  e3wantL <- list(name='e3', type='equation', dim=3L,
                  val=matrix(c(1, 1, 1,  3
                              ,1, 2, 1,  4
                              ), nrow=2, ncol=4, byrow=T),
                  form='sparse', uels=cartIJK, domains=domIJK,
                  field='l', typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',field='L'))
  chk <- chkRgdxRes (e3, e3wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'L',unfiltered) failed",chk$msg))
  }
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',field='L'),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'L',unfiltered,squeeze=F) failed",chk$msg))
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
  e1wantM <- list(name='e1', type='equation', dim=1L,
                 val=matrix(c(1,  -3.5), nrow=1, ncol=2),
                 form='sparse', uels=cartK, domains=domK,
                 field='m', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='M'))
  chk <- chkRgdxRes (e1, e1wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'M',unfiltered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='M'),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'M',unfiltered,squeeze=F) failed",chk$msg))
  }
  v3s <- matrix(c(1, 2, 1,  0.5
                 ), nrow=1, ncol=4, byrow=T)
  v3  <- matrix(c(1, 1, 1,  0
                 ,1, 2, 1,  0.5
                 ), nrow=2, ncol=4, byrow=T)
  e3wantM <- list(name='e3', type='equation', dim=3L,
                  val=v3s,
                 form='sparse', uels=cartIJK, domains=domIJK,
                 field='m', typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',field='m'))
  chk <- chkRgdxRes (e3, e3wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'M',unfiltered) failed",chk$msg))
  }
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',field='m'),squeeze=F)
  e3wantM$val <- v3
  chk <- chkRgdxRes (e3, e3wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'M',unfiltered,squeeze=F) failed",chk$msg))
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
  e1wantLo <- list(name='e1', type='equation', dim=1L,
                  val=matrix(c(1,  -2), nrow=1, ncol=2),
                  form='sparse', uels=cartK, domains=domK,
                  field='lo', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='lO'))
  chk <- chkRgdxRes (e1, e1wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'LO',unfiltered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='lO'),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'LO',unfiltered,squeeze=F) failed",chk$msg))
  }
  v3s <- matrix(0, nrow=0, ncol=4, byrow=T)
  v3  <- matrix(c(1, 1, 1,  -Inf
                 ,1, 2, 1,  -Inf
                 ), nrow=2, ncol=4, byrow=T)
  e3wantLo <- list(name='e3', type='equation', dim=3L,
                   val=v3s,
                   form='sparse', uels=cartIJK, domains=domIJK,
                   field='lo', typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',field='lo'))
  chk <- chkRgdxRes (e3, e3wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'LO',unfiltered) failed",chk$msg))
  }
  e3wantLo$val <- v3
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',field='lo'),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'LO',unfiltered,squeeze=F) failed",chk$msg))
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
  e1wantUp <- list(name='e1', type='equation', dim=1L,
                  val=matrix(0, nrow=0, ncol=2),
                  form='sparse', uels=cartK, domains=domK,
                  field='up', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='Up'))
  chk <- chkRgdxRes (e1, e1wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'UP',unfiltered) failed",chk$msg))
  }
  e1wantUp$val <- matrix(c(1,  Inf), nrow=1, ncol=2)
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='Up'),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'UP',unfiltered,squeeze=F) failed",chk$msg))
  }
  v3 <- matrix(c(1, 1, 1,  4
                ,1, 2, 1,  4
                 ), nrow=2, ncol=4, byrow=T)
  e3wantUp <- list(name='e3', type='equation', dim=3L,
                   val=v3,
                   form='sparse', uels=cartIJK, domains=domIJK,
                   field='up', typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',field='up'))
  chk <- chkRgdxRes (e3, e3wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'UP',unfiltered) failed",chk$msg))
  }
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',field='up'),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'UP',unfiltered,squeeze=F) failed",chk$msg))
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
  e1wantS <- list(name='e1', type='equation', dim=1L,
                 val=matrix(c(1,  2), nrow=1, ncol=2),
                 form='sparse', uels=cartK, domains=domK,
                 field='s', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='S'))
  chk <- chkRgdxRes (e1, e1wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'S',unfiltered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='S'),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'S',unfiltered,squeeze=F) failed",chk$msg))
  }
  v3s <- matrix(0, nrow=0, ncol=4, byrow=T)
  v3 <- matrix(c(1, 1, 1,  1
                ,1, 2, 1,  1
                 ), nrow=2, ncol=4, byrow=T)
  e3wantS <- list(name='e3', type='equation', dim=3L,
                  val=v3s,
                  form='sparse', uels=cartIJK, domains=domIJK,
                  field='s', typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',field='s'))
  chk <- chkRgdxRes (e3, e3wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'S',unfiltered) failed",chk$msg))
  }
  e3wantS$val <- v3
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',field='s'),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'S',unfiltered,squeeze=F) failed",chk$msg))
  }

  ### ---------- reading form=sparse, filtered
  f0 <- list()
  f1 <- list(kUels)
  f3 <- list(iUels,c('j1'),kUels)
  # all
  t <- matrix(c( lev,  0
                ,mar,  1
                ,low,  0
                ,upp,  0
                ,sca,  1
               ), nrow=nFields, ncol=2, byrow=T)
  e0wantA <- list(name='e0', type='equation', dim=0L,
                  val=t,
                  form='sparse', uels=list(fields), domains=domF,
                  field='all',
                  typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',uels=f0,field='all'))
  chk <- chkRgdxRes (e0, e0wantA, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e00,'all',filtered) failed",chk$msg))
  }
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',uels=f0,field='all'),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantA, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'all',filtered,squeeze=F) failed",chk$msg))
  }
  t <- matrix(c( 1, lev,   -2
                ,1, mar,   -3.5
                ,1, low,   -2
                ,1, upp,   Inf
                ,1, sca,   2
             ), nrow=5, ncol=3, byrow=T)
  e1wantA <- list(name='e1', type='equation', dim=1L,
                  val=t,
                  form='sparse', uels=list(f1[[1]],fields), domains=userDom1f,
                  field='all', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',uels=f1,field='all'))
  chk <- chkRgdxRes (e1, e1wantA, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'all',filtered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',field='all',form='sparse',uels=f1),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantA, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'all',filtered,squeeze=F) failed",chk$msg))
  }
  t <- matrix(c( 1,1,1,lev,   3
                ,1,1,1,mar,   0
                ,1,1,1,low,   -Inf
                ,1,1,1,upp,   4
                ,1,1,1,sca,   1
               ), nrow=5, ncol=5, byrow=T)
  e3wantA <- list(name="e3", type="equation", dim=3L,
                  val=t,
                  form="sparse",
                  uels=list(f3[[1]],f3[[2]],f3[[3]],fields), domains=userDom3f,
                  field='all',
                  typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',uels=f3,field='all'))
  chk <- chkRgdxRes (e3, e3wantA, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'all',filtered) failed",chk$msg))
  }
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',uels=f3,field='all'),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantA, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'all',filtered,squeeze=F) failed",chk$msg))
  }
  # level
  e0wantL <- list(name='e0', type='equation', dim=0L,
                  val=matrix(0, nrow=0, ncol=1),
                  form='sparse', uels=f0, domains=character(0),
                  field='l', typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',uels=f0))
  chk <- chkRgdxRes (e0, e0wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'L',filtered) failed",chk$msg))
  }
  e0wantL$val <- matrix(0, nrow=1, ncol=1)
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',uels=f0),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'L',filtered,squeeze=F) failed",chk$msg))
  }
  e1wantL <- list(name='e1', type='equation', dim=1L,
                  val=matrix(c(1, -2), nrow=1, ncol=2),
                  form='sparse', uels=f1, domains=userDom1,
                  field='l', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',uels=f1))
  chk <- chkRgdxRes (e1, e1wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'L',filtered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',uels=f1),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'L',filtered,squeeze=F) failed",chk$msg))
  }
  v3 <- matrix(c(1, 1, 1,  3), nrow=1, ncol=4, byrow=T)
  e3wantL <- list(name='e3', type='equation', dim=3L,
                  val=v3,
                  form='sparse', uels=f3, domains=userDom3,
                  field='l', typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',uels=f3))
  chk <- chkRgdxRes (e3, e3wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'L',filtered) failed",chk$msg))
  }
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',uels=f3),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'L',filtered,squeeze=F) failed",chk$msg))
  }
  # marginal
  e0wantM <- list(name='e0', type='equation', dim=0L,
                  val=matrix(1, nrow=1, ncol=1),
                  form='sparse', uels=f0, domains=character(0),
                  field='m', typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',uels=f0,field='m'))
  chk <- chkRgdxRes (e0, e0wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'M',filtered) failed",chk$msg))
  }
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',uels=f0,field='m'),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'M',filtered,squeeze=F) failed",chk$msg))
  }
  e1wantM <- list(name='e1', type='equation', dim=1L,
                  val=matrix(c(1, -3.5), nrow=1, ncol=2),
                  form='sparse', uels=f1, domains=userDom1,
                  field='m', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='m',uels=f1))
  chk <- chkRgdxRes (e1, e1wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'m',filtered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='m',uels=f1),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'m',filtered,squeeze=F) failed",chk$msg))
  }
  v3s <- matrix(0, nrow=0, ncol=4, byrow=T)
  v3 <- matrix(c(1, 1, 1,  0), nrow=1, ncol=4, byrow=T)
  e3wantM <- list(name='e3', type='equation', dim=3L,
                  val=v3s,
                  form='sparse', uels=f3, domains=userDom3,
                  field='m', typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',uels=f3,field='m'))
  chk <- chkRgdxRes (e3, e3wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'M',filtered) failed",chk$msg))
  }
  e3wantM$val <- v3
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',uels=f3,field='m'),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'M',filtered,squeeze=F) failed",chk$msg))
  }
  # lower
  e0wantLo <- list(name='e0', type='equation', dim=0L,
                   val=matrix(0, nrow=0, ncol=1),
                   form='sparse', uels=f0, domains=character(0),
                   field='lo', typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',uels=f0,field='lo'))
  chk <- chkRgdxRes (e0, e0wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'LO',filtered) failed",chk$msg))
  }
  e0wantLo$val <- matrix(0, nrow=1, ncol=1)
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',uels=f0,field='lo'),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'LO',filtered,squeeze=F) failed",chk$msg))
  }
  e1wantLo <- list(name='e1', type='equation', dim=1L,
                   val=matrix(c(1, -2), nrow=1, ncol=2),
                   form='sparse', uels=f1, domains=userDom1,
                   field='lo', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='lo',uels=f1))
  chk <- chkRgdxRes (e1, e1wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'lo',filtered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='lo',uels=f1),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'lo',filtered,squeeze=F) failed",chk$msg))
  }
  v3s <- matrix(0, nrow=0, ncol=4, byrow=T)
  v3 <- matrix(c(1, 1, 1,  -Inf), nrow=1, ncol=4, byrow=T)
  e3wantLo <- list(name='e3', type='equation', dim=3L,
                   val=v3s,
                   form='sparse', uels=f3, domains=userDom3,
                   field='lo', typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',uels=f3,field='lo'))
  chk <- chkRgdxRes (e3, e3wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'LO',filtered) failed",chk$msg))
  }
  e3wantLo$val <- v3
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',uels=f3,field='lo'),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'LO',filtered,squeeze=F) failed",chk$msg))
  }
  # upper
  e0wantUp <- list(name='e0', type='equation', dim=0L,
                   val=matrix(0, nrow=0, ncol=1),
                   form='sparse', uels=f0, domains=character(0),
                   field='up', typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',uels=f0,field='up'))
  chk <- chkRgdxRes (e0, e0wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'UP',filtered) failed",chk$msg))
  }
  e0wantUp$val <- matrix(0, nrow=1, ncol=1)
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',uels=f0,field='up'),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'UP',filtered,squeeze=F) failed",chk$msg))
  }
  e1wantUp <- list(name='e1', type='equation', dim=1L,
                   val=matrix(Inf, nrow=0, ncol=2),
                   form='sparse', uels=f1, domains=userDom1,
                   field='up', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='up',uels=f1))
  chk <- chkRgdxRes (e1, e1wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'up',filtered) failed",chk$msg))
  }
  e1wantUp$val <- matrix(c(1, +Inf), nrow=1, ncol=2)
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='up',uels=f1),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'up',filtered,squeeze=F) failed",chk$msg))
  }
  v3s <- matrix(c(1, 1, 1,  4), nrow=1, ncol=4, byrow=T)
  e3wantUp <- list(name='e3', type='equation', dim=3L,
                   val=v3s,
                   form='sparse', uels=f3, domains=userDom3,
                   field='up', typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',uels=f3,field='up'))
  chk <- chkRgdxRes (e3, e3wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'UP',filtered) failed",chk$msg))
  }
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',uels=f3,field='up'),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'UP',filtered,squeeze=F) failed",chk$msg))
  }
  # scale
  e0wantS <- list(name='e0', type='equation', dim=0L,
                  val=matrix(1, nrow=0, ncol=1),
                  form='sparse', uels=f0, domains=character(0),
                  field='s', typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',uels=f0,field='s'))
  chk <- chkRgdxRes (e0, e0wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'S',filtered) failed",chk$msg))
  }
  e0wantS$val <- matrix(1, nrow=1, ncol=1)
  e0 <- rgdx(fnIn,list(name='e0',form='sparse',uels=f0,field='s'),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'S',filtered,squeeze=F) failed",chk$msg))
  }
  e1wantS <- list(name='e1', type='equation', dim=1L,
                  val=matrix(c(1,2), nrow=1, ncol=2),
                  form='sparse', uels=f1, domains=userDom1,
                  field='s', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='S',uels=f1))
  chk <- chkRgdxRes (e1, e1wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'s',filtered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',form='sparse',field='S',uels=f1),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'s',filtered,squeeze=F) failed",chk$msg))
  }
  v3s <- matrix(0, nrow=0, ncol=4, byrow=T)
  v3 <- matrix(c(1, 1, 1,  1), nrow=1, ncol=4, byrow=T)
  e3wantS <- list(name='e3', type='equation', dim=3L,
                  val=v3s,
                  form='sparse', uels=f3, domains=userDom3,
                  field='s', typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',uels=f3,field='s'))
  chk <- chkRgdxRes (e3, e3wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'S',filtered) failed",chk$msg))
  }
  e3wantS$val <- v3
  e3 <- rgdx(fnIn,list(name='e3',form='sparse',uels=f3,field='s'),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'S',filtered,squeeze=F) failed",chk$msg))
  }

  ### ---------- reading form=full, no filter
  # all
  t <- array(0,c(nFields),dimnames=list('_field'=fields))
  t[['l' ]] <- 0
  t[['m' ]] <- 1
  t[['lo']] <- 0
  t[['up']] <- 0
  t[['s' ]] <- 1
  e0wantA <- list(name='e0', type='equation', dim=0L,
                  val=t,
                  form='full', uels=list('_field'=fields), domains=domF,
                  field='all',
                  typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='all'))
  chk <- chkRgdxRes (e0, e0wantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'all',full,unfiltered) failed",chk$msg))
  }
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='all'),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'all',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  t <- array(0,c(kCard,nFields),dimnames=cartKF)
  t[    ,'up'] <- Inf
  t[    ,'s' ] <- 2
  t['k1','l' ] <- -2
  t['k1','m' ] <- -3.5
  t['k1','lo'] <- -2
  e1wantA <- list(name='e1', type='equation', dim=1L,
                  val=t,
                  form='full', uels=cartKF, domains=domKF,
                  field='all', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='full',field='aLL'))
  chk <- chkRgdxRes (e1, e1wantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'all',full,unfiltered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',form='full',field='all'),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'all',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  v <- array(0,c(iCard,jCard,kCard,nFields),
             dimnames=list('I'=iUels,'J'=jUels,'K'=kUels,'_field'=fields))
  v[iUels,'j1' ,kUels,'l' ] <- 3
  v[iUels,'j2' ,kUels,'l' ] <- 4
  v[iUels,'j1' ,kUels,'m' ] <- 0
  v[iUels,'j2' ,kUels,'m' ] <- 0.5
  v[iUels,jUels,kUels,'lo'] <- -Inf
  v[iUels,jUels,kUels,'up'] <- 4
  v[iUels,jUels,kUels,'s' ] <- 1

  e3wantA <- list(name="e3", type="equation", dim=3L,
                  val=v,
                  form="full",
                  uels=list('I'=iUels,'J'=jUels,'K'=kUels,'_field'=fields),
                  domains=domIJKF,
                  field='all',
                  typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',form='full',field='all'))
  chk <- chkRgdxRes (e3, e3wantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'all',full,unfiltered) failed",chk$msg))
  }
  e3 <- rgdx(fnIn,list(name='e3',form='full',field='all'),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'all',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  # level
  e0wantL <- list(name='e0', type='equation', dim=0L,
                  val=0,
                  form='full', uels=list(), domains=character(0),
                  field='l', typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='L'))
  chk <- chkRgdxRes (e0, e0wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'L',full,unfiltered) failed",chk$msg))
  }
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='L'),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'L',full,unfiltered,squeeze=F) failed",chk$msg))
  }
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
  t3 <- array(0,c(iCard,jCard,kCard),
              dimnames=list('I'=iUels,'J'=jUels,'K'=kUels))
  t3['i1','j1','k1'] <- 3
  t3['i1','j2','k1'] <- 4
  e3wantL <- list(name='e3', type='equation', dim=3L,
                  val=t3,
                  form='full',
                  uels=list('I'=iUels,'J'=jUels,'K'=kUels),
                  domains=domIJK,
                  field='l', typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',form='full',field='L'))
  chk <- chkRgdxRes (e3, e3wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'L',full,unfiltered) failed",chk$msg))
  }
  e3 <- rgdx(fnIn,list(name='e3',form='full',field='L'),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'L',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  # marginal
  e0wantM <- list(name='e0', type='equation', dim=0L,
                  val=1,
                  form='full', uels=list(), domains=character(0),
                  field='m', typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='M'))
  chk <- chkRgdxRes (e0, e0wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'M',full,unfiltered) failed",chk$msg))
  }
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='M'),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'M',full,unfiltered,squeeze=F) failed",chk$msg))
  }
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
  t3['i1','j1','k1'] <- 0
  t3['i1','j2','k1'] <- 0.5
  e3wantM <- list(name='e3', type='equation', dim=3L,
                  val=t3,
                  form='full',
                  uels=list('I'=iUels,'J'=jUels,'K'=kUels),
                  domains=domIJK,
                  field='m', typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',form='full',field='M'))
  chk <- chkRgdxRes (e3, e3wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'M',full,unfiltered) failed",chk$msg))
  }
  e3 <- rgdx(fnIn,list(name='e3',form='full',field='M'),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'M',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  # lower
  e0wantLo <- list(name='e0', type='equation', dim=0L,
                   val=0,
                   form='full', uels=list(), domains=character(0),
                   field='lo', typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='lo'))
  chk <- chkRgdxRes (e0, e0wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'Lo',full,unfiltered) failed",chk$msg))
  }
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='lo'),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'Lo',full,unfiltered,squeeze=F) failed",chk$msg))
  }
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
  t3['i1','j1','k1'] <- -Inf
  t3['i1','j2','k1'] <- -Inf
  e3wantLo <- list(name='e3', type='equation', dim=3L,
                   val=t3,
                   form='full',
                   uels=list('I'=iUels,'J'=jUels,'K'=kUels),
                   domains=domIJK,
                   field='lo', typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',form='full',field='LO'))
  chk <- chkRgdxRes (e3, e3wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'LO',full,unfiltered) failed",chk$msg))
  }
  e3 <- rgdx(fnIn,list(name='e3',form='full',field='LO'),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'LO',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  # upper
  e0wantUp <- list(name='e0', type='equation', dim=0L,
                   val=0,
                   form='full', uels=list(), domains=character(0),
                   field='up', typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='up'))
  chk <- chkRgdxRes (e0, e0wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'up',full,unfiltered) failed",chk$msg))
  }
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='up'),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'up',full,unfiltered,squeeze=F) failed",chk$msg))
  }
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
  t3['i1','j1','k1'] <- 4
  t3['i1','j2','k1'] <- 4
  e3wantUp <- list(name='e3', type='equation', dim=3L,
                   val=t3,
                   form='full',
                   uels=list('I'=iUels,'J'=jUels,'K'=kUels),
                   domains=domIJK,
                   field='up', typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',form='full',field='UP'))
  chk <- chkRgdxRes (e3, e3wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'UP',full,unfiltered) failed",chk$msg))
  }
  e3 <- rgdx(fnIn,list(name='e3',form='full',field='UP'),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'UP',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  # scale
  e0wantS <- list(name='e0', type='equation', dim=0L,
                  val=1,
                  form='full', uels=list(), domains=character(0),
                  field='s', typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='s'))
  chk <- chkRgdxRes (e0, e0wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'s',full,unfiltered) failed",chk$msg))
  }
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='s'),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'s',full,unfiltered,squeeze=F) failed",chk$msg))
  }
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
  t3['i1','j1','k1'] <- 1
  t3['i1','j2','k1'] <- 1
  e3wantS <- list(name='e3', type='equation', dim=3L,
                  val=t3,
                  form='full',
                  uels=list('I'=iUels,'J'=jUels,'K'=kUels),
                  domains=domIJK,
                  field='s', typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',form='full',field='S'))
  chk <- chkRgdxRes (e3, e3wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'S',full,unfiltered) failed",chk$msg))
  }
  e3 <- rgdx(fnIn,list(name='e3',form='full',field='S'),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'S',full,unfiltered,squeeze=F) failed",chk$msg))
  }


  ### ---------- reading form=full, filtered
  f0 <- list()
  f <- list(kUels)
  f3 <- list(iUels,c('j1'),kUels)
  filtercartIJKF <- list(iUels,c('j1'),kUels,fields)
  # all
  t <- array(0,c(nFields),dimnames=list('_field'=fields))
  t[['m' ]] <- 1
  t[['s' ]] <- 1
  e0wantA <- list(name='e0', type='equation', dim=0L,
                  val=t,
                  form='full', uels=list('_field'=fields), domains=domF,
                  field='all',
                  typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='all'))
  chk <- chkRgdxRes (e0, e0wantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'all',full,filtered) failed",chk$msg))
  }
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='all'),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'all',full,filtered,squeeze=F) failed",chk$msg))
  }
  t <- array(0,c(kCard,nFields),dimnames=cartKF)
  t['k1','l' ] <- -2
  t['k1','m' ] <- -3.5
  t['k1','lo'] <- -2
  t['k1','up'] <- +Inf
  t['k1','s' ] <- 2
  e1wantA <- list(name='e1', type='equation', dim=1L,
                  val=t,
                  form='full', uels=cartKF, domains=userDom1f,
                  field='all', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',form='full',uels=cartK,field='all'))
  chk <- chkRgdxRes (e1, e1wantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'all',full,filtered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',form='full',uels=cartK,field='all'),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'all',full,filtered,squeeze=F) failed",chk$msg))
  }
  e3uels <- list('_user'=f3[[1]],'_user'=f3[[2]],'_user'=f3[[3]],'_field'=fields)
  v <- array(0,c(iCard,1,kCard,nFields),
             dimnames=e3uels)
  v['i1','j1','k1','l' ] <- 3
  v['i1','j1','k1','m' ] <- 0
  v['i1','j1','k1','lo'] <- -Inf
  v['i1','j1','k1','up'] <- 4
  v['i1','j1','k1','s' ] <- 1
  e3wantA <- list(name="e3", type="equation", dim=3L,
                  val=v,
                  form="full",
                  uels=e3uels,
                  domains=userDom3f,
                  field='all',
                  typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',form='full',field='all',uels=f3))
  chk <- chkRgdxRes (e3, e3wantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'all',full,filtered) failed",chk$msg))
  }
  e3 <- rgdx(fnIn,list(name='e3',form='full',field='all',uels=f3),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'all',full,filtered,squeeze=F) failed",chk$msg))
  }
  # level
  e0wantL <- list(name='e0', type='equation', dim=0L,
                  val=0,
                  form='full', uels=list(), domains=character(0),
                  field='l', typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='L',uels=f0))
  chk <- chkRgdxRes (e0, e0wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'L',full,filtered) failed",chk$msg))
  }
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='L',uels=f0),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'L',full,filtered,squeeze=F) failed",chk$msg))
  }
  t <- array(0,c(kCard,1),dimnames=cartK)
  t['k1',1] <- -2
  e1wantL <- list(name='e1', type='equation', dim=1L,
                  val=t,
                  form='full', uels=f1, domains=userDom1,
                  field='l', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',uels=f1,form='full',field='L'))
  chk <- chkRgdxRes (e1, e1wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'L',full,filtered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',uels=f1,form='full',field='L'),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'L',full,filtered,squeeze=F) failed",chk$msg))
  }
  t3 <- array(0,c(iCard,1,kCard),dimnames=list(iUels,c('j1'),kUels))
  t3['i1','j1','k1'] <- 3
  e3wantL <- list(name='e3', type='equation', dim=3L,
                  val=t3,
                  form='full', uels=f3, domains=userDom3,
                  field='l', typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',uels=f3,form='full',field='L'))
  chk <- chkRgdxRes (e3, e3wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'L',full,filtered) failed",chk$msg))
  }
  e3 <- rgdx(fnIn,list(name='e3',uels=f3,form='full',field='L'),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'L',full,filtered,squeeze=F) failed",chk$msg))
  }
  # marginal
  e0wantM <- list(name='e0', type='equation', dim=0L,
                  val=1,
                  form='full', uels=list(), domains=character(0),
                  field='m', typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='M',uels=f0))
  chk <- chkRgdxRes (e0, e0wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'M',full,filtered) failed",chk$msg))
  }
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='M',uels=f0),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'M',full,filtered,squeeze=F) failed",chk$msg))
  }
  t <- array(0,c(kCard,1),dimnames=cartK)
  t['k1',1] <- -3.5
  e1wantM <- list(name='e1', type='equation', dim=1L,
                  val=t,
                  form='full', uels=f1, domains=userDom1,
                  field='m', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',uels=f1,form='full',field='M'))
  chk <- chkRgdxRes (e1, e1wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'M',full,filtered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',uels=f1,form='full',field='M'),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'M',full,filtered,squeeze=F) failed",chk$msg))
  }
  t3 <- array(0,c(iCard,1,kCard),dimnames=list(iUels,c('j1'),kUels))
  t3['i1','j1','k1'] <- 0
  e3wantM <- list(name='e3', type='equation', dim=3L,
                  val=t3,
                  form='full', uels=f3, domains=userDom3,
                  field='m', typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',uels=f3,form='full',field='M'))
  chk <- chkRgdxRes (e3, e3wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'M',full,filtered) failed",chk$msg))
  }
  e3 <- rgdx(fnIn,list(name='e3',uels=f3,form='full',field='M'),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'M',full,filtered,squeeze=F) failed",chk$msg))
  }
  # lower
  e0wantLo <- list(name='e0', type='equation', dim=0L,
                   val=0,
                   form='full', uels=list(), domains=character(0),
                   field='lo', typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='lo',uels=f0))
  chk <- chkRgdxRes (e0, e0wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'lo',full,filtered) failed",chk$msg))
  }
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='lo',uels=f0),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'lo',full,filtered,squeeze=F) failed",chk$msg))
  }
  t <- array(0,c(kCard,1),dimnames=cartK)
  t['k1',1] <- -2
  e1wantLo <- list(name='e1', type='equation', dim=1L,
                   val=t,
                   form='full', uels=f1, domains=userDom1,
                   field='lo', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',uels=f1,form='full',field='Lo'))
  chk <- chkRgdxRes (e1, e1wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'LO',full,filtered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',uels=f1,form='full',field='Lo'),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'LO',full,filtered,squeeze=F) failed",chk$msg))
  }
  t3 <- array(0,c(iCard,1,kCard),dimnames=list(iUels,c('j1'),kUels))
  t3['i1','j1','k1'] <- -Inf
  e3wantLo <- list(name='e3', type='equation', dim=3L,
                   val=t3,
                   form='full', uels=f3, domains=userDom3,
                   field='lo', typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',uels=f3,form='full',field='LO'))
  chk <- chkRgdxRes (e3, e3wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'LO',full,filtered) failed",chk$msg))
  }
  e3 <- rgdx(fnIn,list(name='e3',uels=f3,form='full',field='LO'),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'LO',full,filtered,squeeze=F) failed",chk$msg))
  }
  # upper
  e0wantUp <- list(name='e0', type='equation', dim=0L,
                   val=0,
                   form='full', uels=list(), domains=character(0),
                   field='up', typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='up',uels=f0))
  chk <- chkRgdxRes (e0, e0wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'up',full,filtered) failed",chk$msg))
  }
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='up',uels=f0),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'up',full,filtered,squeeze=F) failed",chk$msg))
  }
  t <- array(0,c(kCard,1),dimnames=cartK)
  t['k1',1] <- Inf
  e1wantUp <- list(name='e1', type='equation', dim=1L,
                   val=t,
                   form='full', uels=f1, domains=userDom1,
                   field='up', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',uels=f1,form='full',field='Up'))
  chk <- chkRgdxRes (e1, e1wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'UP',full,filtered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',uels=f1,form='full',field='Up'),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'UP',full,filtered,squeeze=F) failed",chk$msg))
  }
  t3 <- array(0,c(iCard,1,kCard),dimnames=list(iUels,c('j1'),kUels))
  t3['i1','j1','k1'] <- 4
  e3wantUp <- list(name='e3', type='equation', dim=3L,
                   val=t3,
                   form='full', uels=f3, domains=userDom3,
                   field='up', typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',uels=f3,form='full',field='UP'))
  chk <- chkRgdxRes (e3, e3wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'UP',full,filtered) failed",chk$msg))
  }
  e3 <- rgdx(fnIn,list(name='e3',uels=f3,form='full',field='UP'),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'UP',full,filtered,squeeze=F) failed",chk$msg))
  }
  # scale
  e0wantS <- list(name='e0', type='equation', dim=0L,
                  val=1,
                  form='full', uels=list(), domains=character(0),
                  field='s', typeCode=GMS_EQUTYPE$E)
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='s',uels=f0))
  chk <- chkRgdxRes (e0, e0wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'s',full,filtered) failed",chk$msg))
  }
  e0 <- rgdx(fnIn,list(name='e0',form='full',field='s',uels=f0),squeeze=F)
  chk <- chkRgdxRes (e0, e0wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e0,'s',full,filtered,squeeze=F) failed",chk$msg))
  }
  t <- array(0,c(kCard,1),dimnames=cartK)
  t['k1',1] <- 2
  e1wantS <- list(name='e1', type='equation', dim=1L,
                  val=t,
                  form='full', uels=f1, domains=userDom1,
                  field='s', typeCode=GMS_EQUTYPE$G)
  e1 <- rgdx(fnIn,list(name='e1',uels=f1,form='full',field='s'))
  chk <- chkRgdxRes (e1, e1wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'s',full,filtered) failed",chk$msg))
  }
  e1 <- rgdx(fnIn,list(name='e1',uels=f1,form='full',field='s'),squeeze=F)
  chk <- chkRgdxRes (e1, e1wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e1,'s',full,filtered,squeeze=F) failed",chk$msg))
  }
  t3 <- array(0,c(iCard,1,kCard),dimnames=list(iUels,c('j1'),kUels))
  t3['i1','j1','k1'] <- 1
  e3wantS <- list(name='e3', type='equation', dim=3L,
                  val=t3,
                  form='full', uels=f3, domains=userDom3,
                  field='s', typeCode=GMS_EQUTYPE$L)
  e3 <- rgdx(fnIn,list(name='e3',uels=f3,form='full',field='S'))
  chk <- chkRgdxRes (e3, e3wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'S',full,filtered) failed",chk$msg))
  }
  e3 <- rgdx(fnIn,list(name='e3',uels=f3,form='full',field='S'),squeeze=F)
  chk <- chkRgdxRes (e3, e3wantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(e3,'S',full,filtered,squeeze=F) failed",chk$msg))
  }


  print ("test of rgdx on equation reads passed")
  TRUE   ## all tests passed: return TRUE
},

error = function(ex) { print ("test of rgdx on equation reads failed"); print(ex) ; FALSE }
)
