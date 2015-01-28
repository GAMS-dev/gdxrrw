#### test rgdx reading a 0-dim variable
#### test form=['sparse','full'] X [filtered,unfiltered]
#### ['l','m','lo','up','s']

#### wanted lists can be produced with    dump("listName",file="")

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

source ("chkSame.R")
reqIdent <- TRUE

fields <- c('l','m','lo','up','s')
nFields <- length(fields)
userDomf <- c('_field')
lev <- 1
mar <- 2
low <- 3
upp <- 4
sca <- 5

tryCatch({
  print ("testing rgdx on variable reads")
  rgdx('?')
  fnIn <- "tReadVar0.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }

  ### ---------- reading form=sparse, no filter
  # all
  t <- matrix(c( lev,  24
                ,mar,  -1
                ,low,  0
                ,upp,  100
                ,sca,  10
               ), nrow=nFields, ncol=2, byrow=T)
  xwantA <- list(name='xpos0', type='variable', dim=0L,
                 val=t,
                 form='sparse', uels=list(fields), domains=userDomf,
                 field='all',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='all'))
  chk <- chkRgdxRes (x, xwantA, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'all',unfiltered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='all'),squeeze=F)
  chk <- chkRgdxRes (x, xwantA, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'all',unfiltered,squeeze=F) failed",chk$msg))
  }
  t <- matrix(c( lev,  1
                ,mar,  0.5
                ,low,  1
                ,upp,  1
                ,sca,  1
               ), nrow=nFields, ncol=2, byrow=T)
  ywantA <- list(name='y0', type='variable', dim=0L,
                 val=t,
                 form='sparse', uels=list(fields), domains=userDomf,
                 field='all',
                 varTypeText='binary', typeCode=GMS_VARTYPE$BINARY)
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='alL'))
  chk <- chkRgdxRes (y, ywantA, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'all',unfiltered) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='alL'),squeeze=F)
  chk <- chkRgdxRes (y, ywantA, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'all',unfiltered,squeeze=F) failed",chk$msg))
  }
  t <- matrix(c( lev,  26
                ,mar,  0
                ,low,  -Inf
                ,upp,  +Inf
                ,sca,  1
               ), nrow=nFields, ncol=2, byrow=T)
  zwantA <- list(name='z', type='variable', dim=0L,
                 val=t,
                 form='sparse', uels=list(fields), domains=userDomf,
                 field='all',
                 varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx(fnIn,list(name='z',form='sparse',field='all'))
  chk <- chkRgdxRes (z, zwantA, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'all',unfiltered) failed",chk$msg))
  }
  z <- rgdx(fnIn,list(name='z',form='sparse',field='all'),squeeze=F)
  chk <- chkRgdxRes (z, zwantA, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'all',unfiltered,squeeze=F) failed",chk$msg))
  }
  # level
  xwantL <- list(name='xpos0', type='variable', dim=0L,
                 val=matrix(24, nrow=1, ncol=1),
                 form='sparse', uels=list(), domains=character(0),
                 field='l',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='L'))
  chk <- chkRgdxRes (x, xwantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'L',unfiltered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='L'),squeeze=F)
  chk <- chkRgdxRes (x, xwantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'L',unfiltered,squeeze=F) failed",chk$msg))
  }
  ywantL <- list(name='y0', type='variable', dim=0L,
                 val=matrix(1, nrow=1, ncol=1),
                 form='sparse', uels=list(), domains=character(0),
                 field='l',
                 varTypeText='binary', typeCode=GMS_VARTYPE$BINARY)
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='L'))
  chk <- chkRgdxRes (y, ywantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'L',unfiltered) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='L'),squeeze=F)
  chk <- chkRgdxRes (y, ywantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'L',unfiltered,squeeze=F) failed",chk$msg))
  }
  zwantL <- list(name='z', type='variable', dim=0L,
                 val=matrix(26, nrow=1, ncol=1),
                 form='sparse', uels=list(), domains=character(0),
                 field='l',
                 varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx(fnIn,list(name='z',form='sparse',field='L'))
  chk <- chkRgdxRes (z, zwantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'L',unfiltered) failed",chk$msg))
  }
  z <- rgdx(fnIn,list(name='z',form='sparse',field='L'),squeeze=F)
  chk <- chkRgdxRes (z, zwantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'L',unfiltered,squeeze=F) failed",chk$msg))
  }
  # marginal
  xwantM <- list(name='xpos0', type='variable', dim=0L,
                 val=matrix(-1, nrow=1, ncol=1),
                 form='sparse', uels=list(), domains=character(0),
                 field='m',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='M'))
  chk <- chkRgdxRes (x, xwantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'M',unfiltered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='M'),squeeze=F)
  chk <- chkRgdxRes (x, xwantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'M',unfiltered,squeeze=F) failed",chk$msg))
  }
  ywantM <- list(name='y0', type='variable', dim=0L,
                 val=matrix(0.5, nrow=1, ncol=1),
                 form='sparse', uels=list(), domains=character(0),
                 field='m',
                 varTypeText='binary', typeCode=GMS_VARTYPE$BINARY)
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='M'))
  chk <- chkRgdxRes (y, ywantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'M',unfiltered) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='M'),squeeze=F)
  chk <- chkRgdxRes (y, ywantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'M',unfiltered,squeeze=F) failed",chk$msg))
  }
  zwantM <- list(name='z', type='variable', dim=0L,
                 val=matrix(0, nrow=0, ncol=1),
                 form='sparse', uels=list(), domains=character(0),
                 field='m',
                 varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx(fnIn,list(name='z',form='sparse',field='M'))
  chk <- chkRgdxRes (z, zwantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'M',unfiltered) failed",chk$msg))
  }
  zwantM$val <- matrix(0, nrow=1, ncol=1)
  z <- rgdx(fnIn,list(name='z',form='sparse',field='M'),squeeze=F)
  chk <- chkRgdxRes (z, zwantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'M',unfiltered,squeeze=F) failed",chk$msg))
  }
  # lower
  xwantLo <- list(name='xpos0', type='variable', dim=0L,
                  val=matrix(0, nrow=0, ncol=1),
                  form='sparse', uels=list(), domains=character(0),
                  field='lo',
                  varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='lo'))
  chk <- chkRgdxRes (x, xwantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'lo',unfiltered) failed",chk$msg))
  }
  xwantLo$val <- matrix(0, nrow=1, ncol=1)
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='lo'),squeeze=F)
  chk <- chkRgdxRes (x, xwantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'lo',unfiltered,squeeze=F) failed",chk$msg))
  }
  ywantLo <- list(name='y0', type='variable', dim=0L,
                  val=matrix(1, nrow=1, ncol=1),
                  form='sparse', uels=list(), domains=character(0),
                  field='lo',
                  varTypeText='binary', typeCode=GMS_VARTYPE$BINARY)
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='lo'))
  chk <- chkRgdxRes (y, ywantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'lo',unfiltered) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='lo'),squeeze=F)
  chk <- chkRgdxRes (y, ywantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'lo',unfiltered,squeeze=F) failed",chk$msg))
  }
  zwantLo <- list(name='z', type='variable', dim=0L,
                  val=matrix(-Inf, nrow=0, ncol=1),
                  form='sparse', uels=list(), domains=character(0),
                  field='lo',
                  varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx(fnIn,list(name='z',form='sparse',field='lo'))
  chk <- chkRgdxRes (z, zwantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'lo',unfiltered) failed",chk$msg))
  }
  zwantLo$val <- matrix(-Inf, nrow=1, ncol=1)
  z <- rgdx(fnIn,list(name='z',form='sparse',field='lo'),squeeze=F)
  chk <- chkRgdxRes (z, zwantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'lo',unfiltered,squeeze=F) failed",chk$msg))
  }
  # upper
  xwantUp <- list(name='xpos0', type='variable', dim=0L,
                  val=matrix(100, nrow=1, ncol=1),
                  form='sparse', uels=list(), domains=character(0),
                  field='up',
                  varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='up'))
  chk <- chkRgdxRes (x, xwantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'up',unfiltered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='up'),squeeze=F)
  chk <- chkRgdxRes (x, xwantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'up',unfiltered,squeeze=F) failed",chk$msg))
  }
  ywantUp <- list(name='y0', type='variable', dim=0L,
                  val=matrix(1, nrow=0, ncol=1),
                  form='sparse', uels=list(), domains=character(0),
                  field='up',
                  varTypeText='binary', typeCode=GMS_VARTYPE$BINARY)
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='up'))
  chk <- chkRgdxRes (y, ywantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'up',unfiltered) failed",chk$msg))
  }
  ywantUp$val <- matrix(1, nrow=1, ncol=1)
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='up'),squeeze=F)
  chk <- chkRgdxRes (y, ywantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'up',unfiltered,squeeze=F) failed",chk$msg))
  }
  zwantUp <- list(name='z', type='variable', dim=0L,
                  val=matrix(Inf, nrow=0, ncol=1),
                  form='sparse', uels=list(), domains=character(0),
                  field='up',
                  varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx(fnIn,list(name='z',form='sparse',field='up'))
  chk <- chkRgdxRes (z, zwantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'up',unfiltered) failed",chk$msg))
  }
  zwantUp$val <- matrix(Inf, nrow=1, ncol=1)
  z <- rgdx(fnIn,list(name='z',form='sparse',field='up'),squeeze=F)
  chk <- chkRgdxRes (z, zwantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'up',unfiltered,squeeze=F) failed",chk$msg))
  }
  # scale
  xwantS <- list(name='xpos0', type='variable', dim=0L,
                 val=matrix(10, nrow=1, ncol=1),
                 form='sparse', uels=list(), domains=character(0),
                 field='s',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='s'))
  chk <- chkRgdxRes (x, xwantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'s',unfiltered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='s'),squeeze=F)
  chk <- chkRgdxRes (x, xwantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'s',unfiltered,squeeze=F) failed",chk$msg))
  }
  ywantS <- list(name='y0', type='variable', dim=0L,
                 val=matrix(1, nrow=0, ncol=1),
                 form='sparse', uels=list(), domains=character(0),
                 field='s',
                 varTypeText='binary', typeCode=GMS_VARTYPE$BINARY)
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='s'))
  chk <- chkRgdxRes (y, ywantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'s',unfiltered) failed",chk$msg))
  }
  ywantS$val <- matrix(1, nrow=1, ncol=1)
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='s'),squeeze=F)
  chk <- chkRgdxRes (y, ywantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'s',unfiltered,squeeze=F) failed",chk$msg))
  }
  zwantS <- list(name='z', type='variable', dim=0L,
                 val=matrix(1, nrow=0, ncol=1),
                 form='sparse', uels=list(), domains=character(0),
                 field='s',
                 varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx(fnIn,list(name='z',form='sparse',field='s'))
  chk <- chkRgdxRes (z, zwantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'s',unfiltered) failed",chk$msg))
  }
  zwantS$val <- matrix(1, nrow=1, ncol=1)
  z <- rgdx(fnIn,list(name='z',form='sparse',field='s'),squeeze=F)
  chk <- chkRgdxRes (z, zwantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'s',unfiltered,squeeze=F) failed",chk$msg))
  }

  ### ---------- reading form=sparse, filtered
  f <- list()
  # all
  t <- matrix(c( lev,  24
                ,mar,  -1
                ,low,  0
                ,upp,  100
                ,sca,  10
               ), nrow=nFields, ncol=2, byrow=T)
  xwantA <- list(name='xpos0', type='variable', dim=0L,
                 val=t,
                 form='sparse', uels=list(fields), domains=userDomf,
                 field='all',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',uels=f,field='all'))
  chk <- chkRgdxRes (x, xwantA, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'all',filtered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',uels=f,field='all'),squeeze=F)
  chk <- chkRgdxRes (x, xwantA, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'all',filtered,squeeze=F) failed",chk$msg))
  }
  # level
  xwantL <- list(name='xpos0', type='variable', dim=0L,
                 val=matrix(24, nrow=1, ncol=1),
                 form='sparse', uels=f, domains=character(0),
                 field='l',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',uels=f))
  chk <- chkRgdxRes (x, xwantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'L',filtered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',uels=f),squeeze=F)
  chk <- chkRgdxRes (x, xwantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'L',filtered,squeeze=F) failed",chk$msg))
  }
  ywantL <- list(name='y0', type='variable', dim=0L,
                 val=matrix(1, nrow=1, ncol=1),
                 form='sparse', uels=f, domains=character(0),
                 field='l',
                 varTypeText='binary', typeCode=GMS_VARTYPE$BINARY)
  y <- rgdx(fnIn,list(name='y0',form='sparse',uels=f))
  chk <- chkRgdxRes (y, ywantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'L',filtered) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='sparse',uels=f),squeeze=F)
  chk <- chkRgdxRes (y, ywantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'L',filtered,squeeze=F) failed",chk$msg))
  }
  zwantL <- list(name='z', type='variable', dim=0L,
                 val=matrix(26, nrow=1, ncol=1),
                 form='sparse', uels=f, domains=character(0),
                 field='l',
                 varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx(fnIn,list(name='z',form='sparse',uels=f))
  chk <- chkRgdxRes (z, zwantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'L',filtered) failed",chk$msg))
  }
  z <- rgdx(fnIn,list(name='z',form='sparse',uels=f),squeeze=F)
  chk <- chkRgdxRes (z, zwantL, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'L',filtered,squeeze=F) failed",chk$msg))
  }
  # marginal
  xwantM <- list(name='xpos0', type='variable', dim=0L,
                 val=matrix(-1, nrow=1, ncol=1),
                 form='sparse', uels=f, domains=character(0),
                 field='m',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',uels=f,field='M'))
  chk <- chkRgdxRes (x, xwantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'M',filtered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',uels=f,field='M'))
  chk <- chkRgdxRes (x, xwantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'M',filtered) failed",chk$msg))
  }
  ywantM <- list(name='y0', type='variable', dim=0L,
                 val=matrix(0.5, nrow=1, ncol=1),
                 form='sparse', uels=f, domains=character(0),
                 field='m',
                 varTypeText='binary', typeCode=GMS_VARTYPE$BINARY)
  y <- rgdx(fnIn,list(name='y0',form='sparse',uels=f,field='M'))
  chk <- chkRgdxRes (y, ywantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'M',filtered) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='sparse',uels=f,field='M'),squeeze=F)
  chk <- chkRgdxRes (y, ywantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'M',filtered,squeeze=F) failed",chk$msg))
  }
  zwantM <- list(name='z', type='variable', dim=0L,
                 val=matrix(0, nrow=0, ncol=1),
                 form='sparse', uels=f, domains=character(0),
                 field='m',
                 varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx(fnIn,list(name='z',form='sparse',uels=f,field='M'))
  chk <- chkRgdxRes (z, zwantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'M',filtered) failed",chk$msg))
  }
  zwantM$val <- matrix(0, nrow=1, ncol=1)
  z <- rgdx(fnIn,list(name='z',form='sparse',uels=f,field='M'),squeeze=F)
  chk <- chkRgdxRes (z, zwantM, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'M',filtered,squeeze=F) failed",chk$msg))
  }
  # lower
  xwantLo <- list(name='xpos0', type='variable', dim=0L,
                  val=matrix(0, nrow=0, ncol=1),
                  form='sparse', uels=f, domains=character(0),
                  field='lo',
                  varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',uels=f,field='lo'))
  chk <- chkRgdxRes (x, xwantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'lo',filtered) failed",chk$msg))
  }
  xwantLo$val <- matrix(0, nrow=1, ncol=1)
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',uels=f,field='lo'),squeeze=F)
  chk <- chkRgdxRes (x, xwantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'lo',filtered,squeeze=F) failed",chk$msg))
  }
  ywantLo <- list(name='y0', type='variable', dim=0L,
                  val=matrix(1, nrow=1, ncol=1),
                  form='sparse', uels=f, domains=character(0),
                  field='lo',
                  varTypeText='binary', typeCode=GMS_VARTYPE$BINARY)
  y <- rgdx(fnIn,list(name='y0',form='sparse',uels=f,field='lo'))
  chk <- chkRgdxRes (y, ywantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'lo',filtered) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='sparse',uels=f,field='lo'),squeeze=F)
  chk <- chkRgdxRes (y, ywantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'lo',filtered,squeeze=F) failed",chk$msg))
  }
  zwantLo <- list(name='z', type='variable', dim=0L,
                  val=matrix(-Inf, nrow=0, ncol=1),
                  form='sparse', uels=f, domains=character(0),
                  field='lo',
                  varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx(fnIn,list(name='z',form='sparse',uels=f,field='lo'))
  chk <- chkRgdxRes (z, zwantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'lo',filtered) failed",chk$msg))
  }
  zwantLo$val <- matrix(-Inf, nrow=1, ncol=1)
  z <- rgdx(fnIn,list(name='z',form='sparse',uels=f,field='lo'),squeeze=F)
  chk <- chkRgdxRes (z, zwantLo, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'lo',filtered,squeeze=F) failed",chk$msg))
  }
  # upper
  xwantUp <- list(name='xpos0', type='variable', dim=0L,
                  val=matrix(100, nrow=1, ncol=1),
                  form='sparse', uels=f, domains=character(0),
                  field='up',
                  varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',uels=f,field='UP'))
  chk <- chkRgdxRes (x, xwantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'up',filtered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',uels=f,field='UP'),squeeze=F)
  chk <- chkRgdxRes (x, xwantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'up',filtered,squeeze=F) failed",chk$msg))
  }
  ywantUp <- list(name='y0', type='variable', dim=0L,
                  val=matrix(1, nrow=0, ncol=1),
                  form='sparse', uels=f, domains=character(0),
                  field='up',
                  varTypeText='binary', typeCode=GMS_VARTYPE$BINARY)
  y <- rgdx(fnIn,list(name='y0',form='sparse',uels=f,field='UP'))
  chk <- chkRgdxRes (y, ywantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'up',filtered) failed",chk$msg))
  }
  ywantUp$val <- matrix(1, nrow=1, ncol=1)
  y <- rgdx(fnIn,list(name='y0',form='sparse',uels=f,field='UP'),squeeze=F)
  chk <- chkRgdxRes (y, ywantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'up',filtered,squeeze=F) failed",chk$msg))
  }
  zwantUp <- list(name='z', type='variable', dim=0L,
                  val=matrix(+Inf, nrow=0, ncol=1),
                  form='sparse', uels=f, domains=character(0),
                  field='up',
                  varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx(fnIn,list(name='z',form='sparse',uels=f,field='UP'))
  chk <- chkRgdxRes (z, zwantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'up',filtered) failed",chk$msg))
  }
  zwantUp$val <- matrix(+Inf, nrow=1, ncol=1)
  z <- rgdx(fnIn,list(name='z',form='sparse',uels=f,field='UP'),squeeze=F)
  chk <- chkRgdxRes (z, zwantUp, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'up',filtered,squeeze=F) failed",chk$msg))
  }
  # scale
  xwantS <- list(name='xpos0', type='variable', dim=0L,
                 val=matrix(10, nrow=1, ncol=1),
                 form='sparse', uels=f, domains=character(0),
                 field='s',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='S',uels=f))
  chk <- chkRgdxRes (x, xwantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'S',filtered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='S',uels=f),squeeze=F)
  chk <- chkRgdxRes (x, xwantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'S',filtered,squeeze=F) failed",chk$msg))
  }
  ywantS <- list(name='y0', type='variable', dim=0L,
                 val=matrix(1, nrow=0, ncol=1),
                 form='sparse', uels=f, domains=character(0),
                 field='s',
                 varTypeText='binary', typeCode=GMS_VARTYPE$BINARY)
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='S',uels=f))
  chk <- chkRgdxRes (y, ywantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'S',filtered) failed",chk$msg))
  }
  ywantS$val <- matrix(1, nrow=1, ncol=1)
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='S',uels=f),squeeze=F)
  chk <- chkRgdxRes (y, ywantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'S',filtered,squeeze=F) failed",chk$msg))
  }
  zwantS <- list(name='z', type='variable', dim=0L,
                 val=matrix(1, nrow=0, ncol=1),
                 form='sparse', uels=f, domains=character(0),
                 field='s',
                 varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx(fnIn,list(name='z',form='sparse',field='S',uels=f))
  chk <- chkRgdxRes (z, zwantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'S',filtered) failed",chk$msg))
  }
  zwantS$val <- matrix(1, nrow=1, ncol=1)
  z <- rgdx(fnIn,list(name='z',form='sparse',field='S',uels=f),squeeze=F)
  chk <- chkRgdxRes (z, zwantS, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'S',filtered,squeeze=F) failed",chk$msg))
  }

  ### ---------- reading form=full, no filter
  # all
  t <- array(0,c(nFields),dimnames=list('_field'=fields))
  t[['l' ]] <- 24
  t[['m' ]] <- -1
  t[['up']] <- 100
  t[['s' ]] <- 10
  xwantA <- list(name='xpos0', type='variable', dim=0L,
                 val=t,
                 form='full',
                 uels=list('_field'=fields),
                 domains=userDomf,
                 field='all',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='full',field='all'))
  chk <- chkRgdxRes (x, xwantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'all',full,unfiltered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='xpos0',form='full',field='all'),squeeze=F)
  chk <- chkRgdxRes (x, xwantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'all',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  t <- array(0,c(nFields),dimnames=list('_field'=fields))
  t[['l' ]] <- 1
  t[['m' ]] <- 0.5
  t[['lo']] <- 1
  t[['up']] <- 1
  t[['s' ]] <- 1
  ywantA <- list(name='y0', type='variable', dim=0L,
                 val=t,
                 form='full',
                 uels=list('_field'=fields),
                 domains=userDomf,
                 field='all',
                 varTypeText='binary', typeCode=GMS_VARTYPE$BINARY)
  y <- rgdx(fnIn,list(name='y0',form='full',field='all'))
  chk <- chkRgdxRes (y, ywantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'all',full,unfiltered) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='full',field='all'),squeeze=F)
  chk <- chkRgdxRes (y, ywantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'all',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  t <- array(0,c(nFields),dimnames=list('_field'=fields))
  t[['l' ]] <- 26
  t[['m' ]] <- 0
  t[['lo']] <- -Inf
  t[['up']] <- +Inf
  t[['s' ]] <- 1
  zwantA <- list(name='z', type='variable', dim=0L,
                 val=t,
                 form='full', uels=list('_field'=fields), domains=userDomf,
                 field='all',
                 varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx(fnIn,list(name='z',form='full',field='all'))
  chk <- chkRgdxRes (z, zwantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'all',full,unfiltered) failed",chk$msg))
  }
  z <- rgdx(fnIn,list(name='z',form='full',field='all'),squeeze=F)
  chk <- chkRgdxRes (z, zwantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'all',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  # level
  xwantL <- list(name='xpos0', type='variable', dim=0L,
                 val=24,
                 form='full', uels=list(), domains=character(0),
                 field='l',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='full'))
  chk <- chkRgdxRes (x, xwantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'L',full,unfiltered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='xpos0',form='full'),squeeze=F)
  chk <- chkRgdxRes (x, xwantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'L',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  ywantL <- list(name='y0', type='variable', dim=0L,
                 val=1,
                 form='full', uels=list(), domains=character(0),
                 field='l',
                 varTypeText='binary', typeCode=GMS_VARTYPE$BINARY)
  y <- rgdx(fnIn,list(name='y0',form='full'))
  chk <- chkRgdxRes (y, ywantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'L',full,unfiltered) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='full'),squeeze=F)
  chk <- chkRgdxRes (y, ywantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'L',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  zwantL <- list(name='z', type='variable', dim=0L,
                 val=26,
                 form='full', uels=list(), domains=character(0),
                 field='l',
                 varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx(fnIn,list(name='z',form='full'))
  chk <- chkRgdxRes (z, zwantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'L',full,unfiltered) failed",chk$msg))
  }
  z <- rgdx(fnIn,list(name='z',form='full'),squeeze=F)
  chk <- chkRgdxRes (z, zwantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'L',full,unfiltered,squeeze=F) failed",chk$msg))
  }
  # marginal
  xwantM <- list(name='xpos0', type='variable', dim=0L,
                 val=-1,
                 form='full', uels=list(), domains=character(0),
                 field='m',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='full',field='m'))
  chk <- chkRgdxRes (x, xwantM, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'M',full,unfiltered) failed",chk$msg))
  }
  ywantM <- list(name='y0', type='variable', dim=0L,
                 val=0.5,
                 form='full', uels=list(), domains=character(0),
                 field='m',
                 varTypeText='binary', typeCode=GMS_VARTYPE$BINARY)
  y <- rgdx(fnIn,list(name='y0',form='full',field='m'))
  chk <- chkRgdxRes (y, ywantM, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'M',full,unfiltered) failed",chk$msg))
  }
  zwantM <- list(name='z', type='variable', dim=0L,
                 val=0,
                 form='full', uels=list(), domains=character(0),
                 field='m',
                 varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx(fnIn,list(name='z',form='full',field='m'))
  chk <- chkRgdxRes (z, zwantM, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'M',full,unfiltered) failed",chk$msg))
  }
  # lower
  xwantLo <- list(name='xpos0', type='variable', dim=0L,
                  val=0,
                  form='full', uels=list(), domains=character(0),
                  field='lo',
                  varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='full',field='lo'))
  chk <- chkRgdxRes (x, xwantLo, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'lo',full,unfiltered) failed",chk$msg))
  }
  ywantLo <- list(name='y0', type='variable', dim=0L,
                  val=1,
                  form='full', uels=list(), domains=character(0),
                  field='lo',
                 varTypeText='binary', typeCode=GMS_VARTYPE$BINARY)
  y <- rgdx(fnIn,list(name='y0',form='full',field='lo'))
  chk <- chkRgdxRes (y, ywantLo, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'lo',full,unfiltered) failed",chk$msg))
  }
  zwantLo <- list(name='z', type='variable', dim=0L,
                  val=-Inf,
                  form='full', uels=list(), domains=character(0),
                  field='lo',
                  varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx(fnIn,list(name='z',form='full',field='lo'))
  chk <- chkRgdxRes (z, zwantLo, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'lo',full,unfiltered) failed",chk$msg))
  }
  # upper
  xwantUp <- list(name='xpos0', type='variable', dim=0L,
                  val=100,
                  form='full', uels=list(), domains=character(0),
                  field='up',
                  varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='full',field='up'))
  chk <- chkRgdxRes (x, xwantUp, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'up',full,unfiltered) failed",chk$msg))
  }
  ywantUp <- list(name='y0', type='variable', dim=0L,
                  val=1,
                  form='full', uels=list(), domains=character(0),
                  field='up',
                  varTypeText='binary', typeCode=GMS_VARTYPE$BINARY)
  y <- rgdx(fnIn,list(name='y0',form='full',field='up'))
  chk <- chkRgdxRes (y, ywantUp, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'up',full,unfiltered) failed",chk$msg))
  }
  zwantUp <- list(name='z', type='variable', dim=0L,
                  val=+Inf,
                  form='full', uels=list(), domains=character(0),
                  field='up',
                  varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx(fnIn,list(name='z',form='full',field='up'))
  chk <- chkRgdxRes (z, zwantUp, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'up',full,unfiltered) failed",chk$msg))
  }
  # scale
  xwantS <- list(name='xpos0', type='variable', dim=0L,
                 val=10,
                 form='full', uels=list(), domains=character(0),
                 field='s',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='full',field='s'))
  chk <- chkRgdxRes (x, xwantS, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'s',full,unfiltered) failed",chk$msg))
  }
  ywantS <- list(name='y0', type='variable', dim=0L,
                 val=1,
                 form='full', uels=list(), domains=character(0),
                 field='s',
                 varTypeText='binary', typeCode=GMS_VARTYPE$BINARY)
  y <- rgdx(fnIn,list(name='y0',form='full',field='s'))
  chk <- chkRgdxRes (y, ywantS, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'s',full,unfiltered) failed",chk$msg))
  }
  zwantS <- list(name='z', type='variable', dim=0L,
                 val=1,
                 form='full', uels=list(), domains=character(0),
                 field='s',
                 varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx(fnIn,list(name='z',form='full',field='s'))
  chk <- chkRgdxRes (z, zwantS, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'s',full,unfiltered) failed",chk$msg))
  }

  ### ---------- reading form=full, filtered
  # all
  x <- rgdx(fnIn,list(name='xpos0',form='full',uels=f,field='all'))
  chk <- chkRgdxRes (x, xwantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'all',full,filtered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='xpos0',form='full',uels=f,field='all'),squeeze=F)
  chk <- chkRgdxRes (x, xwantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'all',full,filtered,squeeze=F) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='full',uels=f,field='all'))
  chk <- chkRgdxRes (y, ywantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'all',full,filtered) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='full',uels=f,field='all'),squeeze=F)
  chk <- chkRgdxRes (y, ywantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'all',full,filtered,squeeze=F) failed",chk$msg))
  }
  z <- rgdx(fnIn,list(name='z',form='full',uels=f,field='all'))
  chk <- chkRgdxRes (z, zwantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'all',full,filtered) failed",chk$msg))
  }
  z <- rgdx(fnIn,list(name='z',form='full',uels=f,field='all'),squeeze=F)
  chk <- chkRgdxRes (z, zwantA, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'all',full,filtered,squeeze=F) failed",chk$msg))
  }
  # level
  xwantL <- list(name='xpos0', type='variable', dim=0L,
                 val=24,
                 form='full', uels=f, domains=character(0),
                 field='l',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='full',uels=f))
  chk <- chkRgdxRes (x, xwantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'L',full,filtered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='xpos0',form='full',uels=f),squeeze=F)
  chk <- chkRgdxRes (x, xwantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'L',full,filtered,squeeze=F) failed",chk$msg))
  }
  ywantL <- list(name='y0', type='variable', dim=0L,
                 val=1,
                 form='full', uels=f, domains=character(0),
                 field='l',
                 varTypeText='binary', typeCode=GMS_VARTYPE$BINARY)
  y <- rgdx(fnIn,list(name='y0',form='full',uels=f))
  chk <- chkRgdxRes (y, ywantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'L',full,filtered) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='full',uels=f),squeeze=F)
  chk <- chkRgdxRes (y, ywantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'L',full,filtered,squeeze=F) failed",chk$msg))
  }
  zwantL <- list(name='z', type='variable', dim=0L,
                 val=26,
                 form='full', uels=f, domains=character(0),
                 field='l',
                 varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx(fnIn,list(name='z',form='full',uels=f))
  chk <- chkRgdxRes (z, zwantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'L',full,filtered) failed",chk$msg))
  }
  z <- rgdx(fnIn,list(name='z',form='full',uels=f),squeeze=F)
  chk <- chkRgdxRes (z, zwantL, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'L',full,filtered,squeeze=F) failed",chk$msg))
  }
  # marginal
  xwantM <- list(name='xpos0', type='variable', dim=0L,
                 val=-1,
                 form='full', uels=f, domains=character(0),
                 field='m',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='full',uels=f,field='M'))
  chk <- chkRgdxRes (x, xwantM, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'M',full,filtered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='xpos0',form='full',uels=f,field='M'),squeeze=F)
  chk <- chkRgdxRes (x, xwantM, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'M',full,filtered,squeeze=F) failed",chk$msg))
  }
  ywantM <- list(name='y0', type='variable', dim=0L,
                 val=0.5,
                 form='full', uels=f, domains=character(0),
                 field='m',
                 varTypeText='binary', typeCode=GMS_VARTYPE$BINARY)
  y <- rgdx(fnIn,list(name='y0',form='full',uels=f,field='M'))
  chk <- chkRgdxRes (y, ywantM, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'M',full,filtered) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='full',uels=f,field='M'),squeeze=F)
  chk <- chkRgdxRes (y, ywantM, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'M',full,filtered,squeeze=F) failed",chk$msg))
  }
  zwantM <- list(name='z', type='variable', dim=0L,
                 val=0,
                 form='full', uels=f, domains=character(0),
                 field='m',
                 varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx(fnIn,list(name='z',form='full',uels=f,field='M'))
  chk <- chkRgdxRes (z, zwantM, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'M',full,filtered) failed",chk$msg))
  }
  z <- rgdx(fnIn,list(name='z',form='full',uels=f,field='M'),squeeze=F)
  chk <- chkRgdxRes (z, zwantM, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'M',full,filtered,squeeze=F) failed",chk$msg))
  }
  # lower
  xwantLo <- list(name='xpos0', type='variable', dim=0L,
                  val=0,
                  form='full', uels=f, domains=character(0),
                  field='lo',
                  varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='full',uels=f,field='lo'))
  chk <- chkRgdxRes (x, xwantLo, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'lo',full,filtered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='xpos0',form='full',uels=f,field='lo'),squeeze=F)
  chk <- chkRgdxRes (x, xwantLo, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'lo',full,filtered,squeeze=F) failed",chk$msg))
  }
  ywantLo <- list(name='y0', type='variable', dim=0L,
                  val=1,
                  form='full', uels=f, domains=character(0),
                  field='lo',
                  varTypeText='binary', typeCode=GMS_VARTYPE$BINARY)
  y <- rgdx(fnIn,list(name='y0',form='full',uels=f,field='lo'))
  chk <- chkRgdxRes (y, ywantLo, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'lo',full,filtered) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='full',uels=f,field='lo'),squeeze=F)
  chk <- chkRgdxRes (y, ywantLo, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'lo',full,filtered,squeeze=F) failed",chk$msg))
  }
  zwantLo <- list(name='z', type='variable', dim=0L,
                  val=-Inf,
                  form='full', uels=f, domains=character(0),
                  field='lo',
                  varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx(fnIn,list(name='z',form='full',uels=f,field='lo'))
  chk <- chkRgdxRes (z, zwantLo, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'lo',full,filtered) failed",chk$msg))
  }
  z <- rgdx(fnIn,list(name='z',form='full',uels=f,field='lo'),squeeze=F)
  chk <- chkRgdxRes (z, zwantLo, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'lo',full,filtered,squeeze=F) failed",chk$msg))
  }
  # upper
  xwantUp <- list(name='xpos0', type='variable', dim=0L,
                  val=100,
                  form='full', uels=f, domains=character(0),
                  field='up',
                  varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='full',uels=f,field='up'))
  chk <- chkRgdxRes (x, xwantUp, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'up',full,filtered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='xpos0',form='full',uels=f,field='up'),squeeze=F)
  chk <- chkRgdxRes (x, xwantUp, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'up',full,filtered,squeeze=F) failed",chk$msg))
  }
  ywantUp <- list(name='y0', type='variable', dim=0L,
                  val=1,
                  form='full', uels=f, domains=character(0),
                  field='up',
                  varTypeText='binary', typeCode=GMS_VARTYPE$BINARY)
  y <- rgdx(fnIn,list(name='y0',form='full',uels=f,field='up'))
  chk <- chkRgdxRes (y, ywantUp, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'up',full,filtered) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='full',uels=f,field='up'),squeeze=F)
  chk <- chkRgdxRes (y, ywantUp, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'up',full,filtered,squeeze=F) failed",chk$msg))
  }
  zwantUp <- list(name='z', type='variable', dim=0L,
                  val=+Inf,
                  form='full', uels=f, domains=character(0),
                  field='up',
                  varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx(fnIn,list(name='z',form='full',uels=f,field='up'))
  chk <- chkRgdxRes (z, zwantUp, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'up',full,filtered) failed",chk$msg))
  }
  z <- rgdx(fnIn,list(name='z',form='full',uels=f,field='up'),squeeze=F)
  chk <- chkRgdxRes (z, zwantUp, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'up',full,filtered,squeeze=F) failed",chk$msg))
  }
  # scale
  xwantS <- list(name='xpos0', type='variable', dim=0L,
                 val=10,
                 form='full', uels=f, domains=character(0),
                 field='s',
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx(fnIn,list(name='xpos0',form='full',uels=f,field='s'))
  chk <- chkRgdxRes (x, xwantS, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'s',full,filtered) failed",chk$msg))
  }
  x <- rgdx(fnIn,list(name='xpos0',form='full',uels=f,field='s'),squeeze=F)
  chk <- chkRgdxRes (x, xwantS, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'s',full,filtered,squeeze=F) failed",chk$msg))
  }
  ywantS <- list(name='y0', type='variable', dim=0L,
                 val=1,
                 form='full', uels=f, domains=character(0),
                 field='s',
                 varTypeText='binary', typeCode=GMS_VARTYPE$BINARY)
  y <- rgdx(fnIn,list(name='y0',form='full',uels=f,field='s'))
  chk <- chkRgdxRes (y, ywantS, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'s',full,filtered) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='full',uels=f,field='s'),squeeze=F)
  chk <- chkRgdxRes (y, ywantS, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'s',full,filtered,squeeze=F) failed",chk$msg))
  }
  zwantS <- list(name='z', type='variable', dim=0L,
                 val=1,
                 form='full', uels=f, domains=character(0),
                 field='s',
                 varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx(fnIn,list(name='z',form='full',uels=f,field='s'))
  chk <- chkRgdxRes (z, zwantS, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'s',full,filtered) failed",chk$msg))
  }
  z <- rgdx(fnIn,list(name='z',form='full',uels=f,field='s'),squeeze=F)
  chk <- chkRgdxRes (z, zwantS, T, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'s',full,filtered,squeeze=F) failed",chk$msg))
  }


  print ("test of rgdx on variable reads passed")
  TRUE   ## all tests passed: return TRUE
},

error = function(ex) { print ("test of rgdx on variable reads failed"); print(ex) ; FALSE }
)
