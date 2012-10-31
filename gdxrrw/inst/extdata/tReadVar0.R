#### test rgdx reading a 0-dim variable
#### test form=['sparse','full'] X [filtered,unfiltered] X compress=[T,F]
#### ['l','m','lo','up','s']

#### wanted lists can be produced with    dump("listName",file="")

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

source ("chkSame.R")

tryCatch({
  print ("testing rgdx on variable reads")
  rgdx('?')
  fnIn <- "tReadVar0.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }

  ### ---------- reading form=sparse, no filter, no compress
  # level
  xwantL <- list(name='xpos0', type='variable', dim=0,
                 val=matrix(24, nrow=1, ncol=1),
                 form='sparse', uels=list(), domains=character(0),
                 field='l')
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='L'))
  chk <- chkRgdxRes (x, xwantL)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'L',unfiltered,uncompressed) failed",chk$msg))
  }
  ywantL <- list(name='y0', type='variable', dim=0,
                 val=matrix(1, nrow=1, ncol=1),
                 form='sparse', uels=list(), domains=character(0),
                 field='l')
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='L'))
  chk <- chkRgdxRes (y, ywantL)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'L',unfiltered,uncompressed) failed",chk$msg))
  }
  zwantL <- list(name='z', type='variable', dim=0,
                 val=matrix(26, nrow=1, ncol=1),
                 form='sparse', uels=list(), domains=character(0),
                 field='l')
  z <- rgdx(fnIn,list(name='z',form='sparse',field='L'))
  chk <- chkRgdxRes (z, zwantL)
  if (!chk$same) {
    stop (paste("test rgdx(z,'L',unfiltered,uncompressed) failed",chk$msg))
  }
  # marginal
  xwantM <- list(name='xpos0', type='variable', dim=0,
                 val=matrix(-1, nrow=1, ncol=1),
                 form='sparse', uels=list(), domains=character(0),
                 field='m')
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='M'))
  chk <- chkRgdxRes (x, xwantM)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'M',unfiltered,uncompressed) failed",chk$msg))
  }
  ywantM <- list(name='y0', type='variable', dim=0,
                 val=matrix(0.5, nrow=1, ncol=1),
                 form='sparse', uels=list(), domains=character(0),
                 field='m')
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='M'))
  chk <- chkRgdxRes (y, ywantM)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'M',unfiltered,uncompressed) failed",chk$msg))
  }
  zwantM <- list(name='z', type='variable', dim=0,
                 val=matrix(0, nrow=0, ncol=1),
                 form='sparse', uels=list(), domains=character(0),
                 field='m')
  z <- rgdx(fnIn,list(name='z',form='sparse',field='M'))
  chk <- chkRgdxRes (z, zwantM)
  if (!chk$same) {
    stop (paste("test rgdx(z,'M',unfiltered,uncompressed) failed",chk$msg))
  }
  # lower
  xwantLo <- list(name='xpos0', type='variable', dim=0,
                  val=matrix(0, nrow=0, ncol=1),
                  form='sparse', uels=list(), domains=character(0),
                  field='lo')
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='lo'))
  chk <- chkRgdxRes (x, xwantLo)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'lo',unfiltered,uncompressed) failed",chk$msg))
  }
  ywantLo <- list(name='y0', type='variable', dim=0,
                  val=matrix(1, nrow=1, ncol=1),
                  form='sparse', uels=list(), domains=character(0),
                  field='lo')
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='lo'))
  chk <- chkRgdxRes (y, ywantLo)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'lo',unfiltered,uncompressed) failed",chk$msg))
  }
  zwantLo <- list(name='z', type='variable', dim=0,
                  val=matrix(-Inf, nrow=1, ncol=1),
                  form='sparse', uels=list(), domains=character(0),
                  field='lo')
  z <- rgdx(fnIn,list(name='z',form='sparse',field='lo'))
  chk <- chkRgdxRes (z, zwantLo)
  if (!chk$same) {
    stop (paste("test rgdx(z,'lo',unfiltered,uncompressed) failed",chk$msg))
  }
  # upper
  xwantUp <- list(name='xpos0', type='variable', dim=0,
                  val=matrix(100, nrow=1, ncol=1),
                  form='sparse', uels=list(), domains=character(0),
                  field='up')
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='up'))
  chk <- chkRgdxRes (x, xwantUp)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'up',unfiltered,uncompressed) failed",chk$msg))
  }
  ywantUp <- list(name='y0', type='variable', dim=0,
                  val=matrix(1, nrow=1, ncol=1),
                  form='sparse', uels=list(), domains=character(0),
                  field='up')
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='up'))
  chk <- chkRgdxRes (y, ywantUp)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'up',unfiltered,uncompressed) failed",chk$msg))
  }
  zwantUp <- list(name='z', type='variable', dim=0,
                  val=matrix(Inf, nrow=1, ncol=1),
                  form='sparse', uels=list(), domains=character(0),
                  field='up')
  z <- rgdx(fnIn,list(name='z',form='sparse',field='up'))
  chk <- chkRgdxRes (z, zwantUp)
  if (!chk$same) {
    stop (paste("test rgdx(z,'up',unfiltered,uncompressed) failed",chk$msg))
  }
  # scale
  xwantS <- list(name='xpos0', type='variable', dim=0,
                 val=matrix(10, nrow=1, ncol=1),
                 form='sparse', uels=list(), domains=character(0),
                 field='s')
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='s'))
  chk <- chkRgdxRes (x, xwantS)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'s',unfiltered,uncompressed) failed",chk$msg))
  }
  ywantS <- list(name='y0', type='variable', dim=0,
                 val=matrix(1, nrow=1, ncol=1),
                 form='sparse', uels=list(), domains=character(0),
                 field='s')
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='s'))
  chk <- chkRgdxRes (y, ywantS)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'s',unfiltered,uncompressed) failed",chk$msg))
  }
  zwantS <- list(name='z', type='variable', dim=0,
                 val=matrix(1, nrow=1, ncol=1),
                 form='sparse', uels=list(), domains=character(0),
                 field='s')
  z <- rgdx(fnIn,list(name='z',form='sparse',field='s'))
  chk <- chkRgdxRes (z, zwantS)
  if (!chk$same) {
    stop (paste("test rgdx(z,'s',unfiltered,uncompressed) failed",chk$msg))
  }

  ### ---------- reading form=sparse, no filter, compress=T
  # level
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='L',compress=T))
  chk <- chkRgdxRes (x, xwantL)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'L',unfiltered,compress=T) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='L',compress=T))
  chk <- chkRgdxRes (y, ywantL)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'L',unfiltered,compress=T) failed",chk$msg))
  }
  z <- rgdx(fnIn,list(name='z',form='sparse',field='L',compress=T))
  chk <- chkRgdxRes (z, zwantL)
  if (!chk$same) {
    stop (paste("test rgdx(z,'L',unfiltered,compress=T) failed",chk$msg))
  }
  # marginal
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='M',compress=T))
  chk <- chkRgdxRes (x, xwantM)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'M',unfiltered,compress=T) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='M',compress=T))
  chk <- chkRgdxRes (y, ywantM)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'M',unfiltered,compress=T) failed",chk$msg))
  }
  z <- rgdx(fnIn,list(name='z',form='sparse',field='M',compress=T))
  chk <- chkRgdxRes (z, zwantM)
  if (!chk$same) {
    stop (paste("test rgdx(z,'M',unfiltered,compress=T) failed",chk$msg))
  }
  # lower
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='lo',compress=T))
  chk <- chkRgdxRes (x, xwantLo)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'lo',unfiltered,compress=T) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='lo',compress=T))
  chk <- chkRgdxRes (y, ywantLo)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'lo',unfiltered,compress=T) failed",chk$msg))
  }
  z <- rgdx(fnIn,list(name='z',form='sparse',field='lo',compress=T))
  chk <- chkRgdxRes (z, zwantLo)
  if (!chk$same) {
    stop (paste("test rgdx(z,'lo',unfiltered,compress=T) failed",chk$msg))
  }
  # upper
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='up',compress=T))
  chk <- chkRgdxRes (x, xwantUp)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'up',unfiltered,compress=T) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='up',compress=T))
  chk <- chkRgdxRes (y, ywantUp)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'up',unfiltered,compress=T) failed",chk$msg))
  }
  z <- rgdx(fnIn,list(name='z',form='sparse',field='up',compress=T))
  chk <- chkRgdxRes (z, zwantUp)
  if (!chk$same) {
    stop (paste("test rgdx(z,'up',unfiltered,compress=T) failed",chk$msg))
  }
  # scale
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='s',compress=T))
  chk <- chkRgdxRes (x, xwantS)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'s',unfiltered,compress=T) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='s',compress=T))
  chk <- chkRgdxRes (y, ywantS)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'s',unfiltered,compress=T) failed",chk$msg))
  }
  z <- rgdx(fnIn,list(name='z',form='sparse',field='s',compress=T))
  chk <- chkRgdxRes (z, zwantS)
  if (!chk$same) {
    stop (paste("test rgdx(z,'s',unfiltered,compress=T) failed",chk$msg))
  }

  ### ---------- reading form=sparse, filtered, compress=F
  # level
  f <- list()
  xwantL <- list(name='xpos0', type='variable', dim=0,
                 val=matrix(24, nrow=1, ncol=1),
                 form='sparse', uels=f, domains=character(0),
                 field='l')
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',uels=f))
  chk <- chkRgdxRes (x, xwantL)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'L',filtered,compress=F) failed",chk$msg))
  }
  ywantL <- list(name='y0', type='variable', dim=0,
                 val=matrix(1, nrow=1, ncol=1),
                 form='sparse', uels=f, domains=character(0),
                 field='l')
  y <- rgdx(fnIn,list(name='y0',form='sparse',uels=f))
  chk <- chkRgdxRes (y, ywantL)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'L',filtered,compress=F) failed",chk$msg))
  }
  zwantL <- list(name='z', type='variable', dim=0,
                 val=matrix(26, nrow=1, ncol=1),
                 form='sparse', uels=f, domains=character(0),
                 field='l')
  z <- rgdx(fnIn,list(name='z',form='sparse',uels=f))
  chk <- chkRgdxRes (z, zwantL)
  if (!chk$same) {
    stop (paste("test rgdx(z,'L',filtered,compress=F) failed",chk$msg))
  }
  # marginal
  xwantM <- list(name='xpos0', type='variable', dim=0,
                 val=matrix(-1, nrow=1, ncol=1),
                 form='sparse', uels=f, domains=character(0),
                 field='m')
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',uels=f,field='M'))
  chk <- chkRgdxRes (x, xwantM)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'M',filtered,compress=F) failed",chk$msg))
  }
  ywantM <- list(name='y0', type='variable', dim=0,
                 val=matrix(0.5, nrow=1, ncol=1),
                 form='sparse', uels=f, domains=character(0),
                 field='m')
  y <- rgdx(fnIn,list(name='y0',form='sparse',uels=f,field='M'))
  chk <- chkRgdxRes (y, ywantM)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'M',filtered,compress=F) failed",chk$msg))
  }
  zwantM <- list(name='z', type='variable', dim=0,
                 val=matrix(0, nrow=1, ncol=1),
                 form='sparse', uels=f, domains=character(0),
                 field='m')
  z <- rgdx(fnIn,list(name='z',form='sparse',uels=f,field='M'))
  chk <- chkRgdxRes (z, zwantM)
  if (!chk$same) {
    stop (paste("test rgdx(z,'M',filtered,compress=F) failed",chk$msg))
  }
  # lower
  xwantLo <- list(name='xpos0', type='variable', dim=0,
                  val=matrix(0, nrow=1, ncol=1),
                  form='sparse', uels=f, domains=character(0),
                  field='lo')
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',uels=f,field='lo'))
  chk <- chkRgdxRes (x, xwantLo)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'lo',filtered,compress=F) failed",chk$msg))
  }
  ywantLo <- list(name='y0', type='variable', dim=0,
                  val=matrix(1, nrow=1, ncol=1),
                  form='sparse', uels=f, domains=character(0),
                  field='lo')
  y <- rgdx(fnIn,list(name='y0',form='sparse',uels=f,field='lo'))
  chk <- chkRgdxRes (y, ywantLo)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'lo',filtered,compress=F) failed",chk$msg))
  }
  zwantLo <- list(name='z', type='variable', dim=0,
                  val=matrix(-Inf, nrow=1, ncol=1),
                  form='sparse', uels=f, domains=character(0),
                  field='lo')
  z <- rgdx(fnIn,list(name='z',form='sparse',uels=f,field='lo'))
  chk <- chkRgdxRes (z, zwantLo)
  if (!chk$same) {
    stop (paste("test rgdx(z,'lo',filtered,compress=F) failed",chk$msg))
  }
  # upper
  xwantUp <- list(name='xpos0', type='variable', dim=0,
                  val=matrix(100, nrow=1, ncol=1),
                  form='sparse', uels=f, domains=character(0),
                  field='up')
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',uels=f,field='UP'))
  chk <- chkRgdxRes (x, xwantUp)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'up',filtered,compress=F) failed",chk$msg))
  }
  ywantUp <- list(name='y0', type='variable', dim=0,
                  val=matrix(1, nrow=1, ncol=1),
                  form='sparse', uels=f, domains=character(0),
                  field='up')
  y <- rgdx(fnIn,list(name='y0',form='sparse',uels=f,field='UP'))
  chk <- chkRgdxRes (y, ywantUp)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'up',filtered,compress=F) failed",chk$msg))
  }
  zwantUp <- list(name='z', type='variable', dim=0,
                  val=matrix(+Inf, nrow=1, ncol=1),
                  form='sparse', uels=f, domains=character(0),
                  field='up')
  z <- rgdx(fnIn,list(name='z',form='sparse',uels=f,field='UP'))
  chk <- chkRgdxRes (z, zwantUp)
  if (!chk$same) {
    stop (paste("test rgdx(z,'up',filtered,compress=F) failed",chk$msg))
  }
  # scale
  xwantS <- list(name='xpos0', type='variable', dim=0,
                 val=matrix(10, nrow=1, ncol=1),
                 form='sparse', uels=f, domains=character(0),
                 field='s')
  x <- rgdx(fnIn,list(name='xpos0',form='sparse',field='S',uels=f))
  chk <- chkRgdxRes (x, xwantS)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'S',filtered,compress=F) failed",chk$msg))
  }
  ywantS <- list(name='y0', type='variable', dim=0,
                 val=matrix(1, nrow=1, ncol=1),
                 form='sparse', uels=f, domains=character(0),
                 field='s')
  y <- rgdx(fnIn,list(name='y0',form='sparse',field='S',uels=f))
  chk <- chkRgdxRes (y, ywantS)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'S',filtered,compress=F) failed",chk$msg))
  }
  zwantS <- list(name='z', type='variable', dim=0,
                 val=matrix(1, nrow=1, ncol=1),
                 form='sparse', uels=f, domains=character(0),
                 field='s')
  z <- rgdx(fnIn,list(name='z',form='sparse',field='S',uels=f))
  chk <- chkRgdxRes (z, zwantS)
  if (!chk$same) {
    stop (paste("test rgdx(z,'S',filtered,compress=F) failed",chk$msg))
  }

  ### ---------- reading form=full, no filter, compress=F
  # level
  xwantL <- list(name='xpos0', type='variable', dim=0,
                 val=24,
                 form='full', uels=list(), domains=character(0),
                 field='l')
  x <- rgdx(fnIn,list(name='xpos0',form='full'))
  chk <- chkRgdxRes (x, xwantL, T)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'L',full,unfiltered,compress=F) failed",chk$msg))
  }
  ywantL <- list(name='y0', type='variable', dim=0,
                 val=1,
                 form='full', uels=list(), domains=character(0),
                 field='l')
  y <- rgdx(fnIn,list(name='y0',form='full'))
  chk <- chkRgdxRes (y, ywantL, T)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'L',full,unfiltered,compress=F) failed",chk$msg))
  }
  zwantL <- list(name='z', type='variable', dim=0,
                 val=26,
                 form='full', uels=list(), domains=character(0),
                 field='l')
  z <- rgdx(fnIn,list(name='z',form='full'))
  chk <- chkRgdxRes (z, zwantL, T)
  if (!chk$same) {
    stop (paste("test rgdx(z,'L',full,unfiltered,compress=F) failed",chk$msg))
  }
  # marginal
  xwantM <- list(name='xpos0', type='variable', dim=0,
                 val=-1,
                 form='full', uels=list(), domains=character(0),
                 field='m')
  x <- rgdx(fnIn,list(name='xpos0',form='full',field='m'))
  chk <- chkRgdxRes (x, xwantM, T)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'M',full,unfiltered,compress=F) failed",chk$msg))
  }
  ywantM <- list(name='y0', type='variable', dim=0,
                 val=0.5,
                 form='full', uels=list(), domains=character(0),
                 field='m')
  y <- rgdx(fnIn,list(name='y0',form='full',field='m'))
  chk <- chkRgdxRes (y, ywantM, T)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'M',full,unfiltered,compress=F) failed",chk$msg))
  }
  zwantM <- list(name='z', type='variable', dim=0,
                 val=0,
                 form='full', uels=list(), domains=character(0),
                 field='m')
  z <- rgdx(fnIn,list(name='z',form='full',field='m'))
  chk <- chkRgdxRes (z, zwantM, T)
  if (!chk$same) {
    stop (paste("test rgdx(z,'M',full,unfiltered,compress=F) failed",chk$msg))
  }
  # lower
  xwantLo <- list(name='xpos0', type='variable', dim=0,
                  val=0,
                  form='full', uels=list(), domains=character(0),
                  field='lo')
  x <- rgdx(fnIn,list(name='xpos0',form='full',field='lo'))
  chk <- chkRgdxRes (x, xwantLo, T)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'lo',full,unfiltered,compress=F) failed",chk$msg))
  }
  ywantLo <- list(name='y0', type='variable', dim=0,
                  val=1,
                  form='full', uels=list(), domains=character(0),
                  field='lo')
  y <- rgdx(fnIn,list(name='y0',form='full',field='lo'))
  chk <- chkRgdxRes (y, ywantLo, T)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'lo',full,unfiltered,compress=F) failed",chk$msg))
  }
  zwantLo <- list(name='z', type='variable', dim=0,
                  val=-Inf,
                  form='full', uels=list(), domains=character(0),
                  field='lo')
  z <- rgdx(fnIn,list(name='z',form='full',field='lo'))
  chk <- chkRgdxRes (z, zwantLo, T)
  if (!chk$same) {
    stop (paste("test rgdx(z,'lo',full,unfiltered,compress=F) failed",chk$msg))
  }
  # upper
  xwantUp <- list(name='xpos0', type='variable', dim=0,
                  val=100,
                  form='full', uels=list(), domains=character(0),
                  field='up')
  x <- rgdx(fnIn,list(name='xpos0',form='full',field='up'))
  chk <- chkRgdxRes (x, xwantUp, T)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'up',full,unfiltered,compress=F) failed",chk$msg))
  }
  ywantUp <- list(name='y0', type='variable', dim=0,
                  val=1,
                  form='full', uels=list(), domains=character(0),
                  field='up')
  y <- rgdx(fnIn,list(name='y0',form='full',field='up'))
  chk <- chkRgdxRes (y, ywantUp, T)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'up',full,unfiltered,compress=F) failed",chk$msg))
  }
  zwantUp <- list(name='z', type='variable', dim=0,
                  val=+Inf,
                  form='full', uels=list(), domains=character(0),
                  field='up')
  z <- rgdx(fnIn,list(name='z',form='full',field='up'))
  chk <- chkRgdxRes (z, zwantUp, T)
  if (!chk$same) {
    stop (paste("test rgdx(z,'up',full,unfiltered,compress=F) failed",chk$msg))
  }
  # scale
  xwantS <- list(name='xpos0', type='variable', dim=0,
                 val=10,
                 form='full', uels=list(), domains=character(0),
                 field='s')
  x <- rgdx(fnIn,list(name='xpos0',form='full',field='s'))
  chk <- chkRgdxRes (x, xwantS, T)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'s',full,unfiltered,compress=F) failed",chk$msg))
  }
  ywantS <- list(name='y0', type='variable', dim=0,
                 val=1,
                 form='full', uels=list(), domains=character(0),
                 field='s')
  y <- rgdx(fnIn,list(name='y0',form='full',field='s'))
  chk <- chkRgdxRes (y, ywantS, T)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'s',full,unfiltered,compress=F) failed",chk$msg))
  }
  zwantS <- list(name='z', type='variable', dim=0,
                 val=1,
                 form='full', uels=list(), domains=character(0),
                 field='s')
  z <- rgdx(fnIn,list(name='z',form='full',field='s'))
  chk <- chkRgdxRes (z, zwantS, T)
  if (!chk$same) {
    stop (paste("test rgdx(z,'s',full,unfiltered,compress=F) failed",chk$msg))
  }

  ### ---------- reading form=full, no filter, compress=T
  # level
  x <- rgdx(fnIn,list(name='xpos0',form='full',compress=T))
  chk <- chkRgdxRes (x, xwantL, T)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'L',full,unfiltered,compress=T) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='full',compress=T))
  chk <- chkRgdxRes (y, ywantL, T)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'L',full,unfiltered,compress=T) failed",chk$msg))
  }
  z <- rgdx(fnIn,list(name='z',form='full',compress=T))
  chk <- chkRgdxRes (z, zwantL, T)
  if (!chk$same) {
    stop (paste("test rgdx(z,'L',full,unfiltered,compress=T) failed",chk$msg))
  }
  # marginal
  x <- rgdx(fnIn,list(name='xpos0',form='full',field='M',compress=T))
  chk <- chkRgdxRes (x, xwantM, T)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'M',filtered,compress=T) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='full',field='M',compress=T))
  chk <- chkRgdxRes (y, ywantM, T)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'M',filtered,compress=T) failed",chk$msg))
  }
  z <- rgdx(fnIn,list(name='z',form='full',field='M',compress=T))
  chk <- chkRgdxRes (z, zwantM, T)
  if (!chk$same) {
    stop (paste("test rgdx(z,'M',filtered,compress=T) failed",chk$msg))
  }
  # lower
  x <- rgdx(fnIn,list(name='xpos0',form='full',field='lo',compress=T))
  chk <- chkRgdxRes (x, xwantLo, T)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'lo',filtered,compress=T) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='full',field='lo',compress=T))
  chk <- chkRgdxRes (y, ywantLo, T)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'lo',filtered,compress=T) failed",chk$msg))
  }
  z <- rgdx(fnIn,list(name='z',form='full',field='lo',compress=T))
  chk <- chkRgdxRes (z, zwantLo, T)
  if (!chk$same) {
    stop (paste("test rgdx(z,'lo',filtered,compress=T) failed",chk$msg))
  }
  # upper
  x <- rgdx(fnIn,list(name='xpos0',form='full',field='up',compress=T))
  chk <- chkRgdxRes (x, xwantUp, T)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'up',filtered,compress=T) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='full',field='up',compress=T))
  chk <- chkRgdxRes (y, ywantUp, T)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'up',filtered,compress=T) failed",chk$msg))
  }
  z <- rgdx(fnIn,list(name='z',form='full',field='up',compress=T))
  chk <- chkRgdxRes (z, zwantUp, T)
  if (!chk$same) {
    stop (paste("test rgdx(z,'up',filtered,compress=T) failed",chk$msg))
  }
  # scale
  x <- rgdx(fnIn,list(name='xpos0',form='full',field='s',compress=T))
  chk <- chkRgdxRes (x, xwantS, T)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'s',filtered,compress=T) failed",chk$msg))
  }
  y <- rgdx(fnIn,list(name='y0',form='full',field='s',compress=T))
  chk <- chkRgdxRes (y, ywantS, T)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'s',filtered,compress=T) failed",chk$msg))
  }
  z <- rgdx(fnIn,list(name='z',form='full',field='s',compress=T))
  chk <- chkRgdxRes (z, zwantS, T)
  if (!chk$same) {
    stop (paste("test rgdx(z,'s',filtered,compress=T) failed",chk$msg))
  }

  ### ---------- reading form=full, filtered, compress=F
  # level
  xwantL <- list(name='xpos0', type='variable', dim=0,
                 val=24,
                 form='full', uels=f, domains=character(0),
                 field='l')
  x <- rgdx(fnIn,list(name='xpos0',form='full',uels=f))
  chk <- chkRgdxRes (x, xwantL, T)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'L',full,filtered) failed",chk$msg))
  }
  ywantL <- list(name='y0', type='variable', dim=0,
                 val=1,
                 form='full', uels=f, domains=character(0),
                 field='l')
  y <- rgdx(fnIn,list(name='y0',form='full',uels=f))
  chk <- chkRgdxRes (y, ywantL, T)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'L',full,filtered) failed",chk$msg))
  }
  zwantL <- list(name='z', type='variable', dim=0,
                 val=26,
                 form='full', uels=f, domains=character(0),
                 field='l')
  z <- rgdx(fnIn,list(name='z',form='full',uels=f))
  chk <- chkRgdxRes (z, zwantL, T)
  if (!chk$same) {
    stop (paste("test rgdx(z,'L',full,filtered) failed",chk$msg))
  }
  # marginal
  xwantM <- list(name='xpos0', type='variable', dim=0,
                 val=-1,
                 form='full', uels=f, domains=character(0),
                 field='m')
  x <- rgdx(fnIn,list(name='xpos0',form='full',uels=f,field='M'))
  chk <- chkRgdxRes (x, xwantM, T)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'M',full,filtered) failed",chk$msg))
  }
  ywantM <- list(name='y0', type='variable', dim=0,
                 val=0.5,
                 form='full', uels=f, domains=character(0),
                 field='m')
  y <- rgdx(fnIn,list(name='y0',form='full',uels=f,field='M'))
  chk <- chkRgdxRes (y, ywantM, T)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'M',full,filtered) failed",chk$msg))
  }
  zwantM <- list(name='z', type='variable', dim=0,
                 val=0,
                 form='full', uels=f, domains=character(0),
                 field='m')
  z <- rgdx(fnIn,list(name='z',form='full',uels=f,field='M'))
  chk <- chkRgdxRes (z, zwantM, T)
  if (!chk$same) {
    stop (paste("test rgdx(z,'M',full,filtered) failed",chk$msg))
  }
  # lower
  xwantLo <- list(name='xpos0', type='variable', dim=0,
                  val=0,
                  form='full', uels=f, domains=character(0),
                  field='lo')
  x <- rgdx(fnIn,list(name='xpos0',form='full',uels=f,field='lo'))
  chk <- chkRgdxRes (x, xwantLo, T)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'lo',full,filtered) failed",chk$msg))
  }
  ywantLo <- list(name='y0', type='variable', dim=0,
                  val=1,
                  form='full', uels=f, domains=character(0),
                  field='lo')
  y <- rgdx(fnIn,list(name='y0',form='full',uels=f,field='lo'))
  chk <- chkRgdxRes (y, ywantLo, T)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'lo',full,filtered) failed",chk$msg))
  }
  zwantLo <- list(name='z', type='variable', dim=0,
                  val=-Inf,
                  form='full', uels=f, domains=character(0),
                  field='lo')
  z <- rgdx(fnIn,list(name='z',form='full',uels=f,field='lo'))
  chk <- chkRgdxRes (z, zwantLo, T)
  if (!chk$same) {
    stop (paste("test rgdx(z,'lo',full,filtered) failed",chk$msg))
  }
  # upper
  xwantUp <- list(name='xpos0', type='variable', dim=0,
                  val=100,
                  form='full', uels=f, domains=character(0),
                  field='up')
  x <- rgdx(fnIn,list(name='xpos0',form='full',uels=f,field='up'))
  chk <- chkRgdxRes (x, xwantUp, T)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'up',full,filtered) failed",chk$msg))
  }
  ywantUp <- list(name='y0', type='variable', dim=0,
                  val=1,
                  form='full', uels=f, domains=character(0),
                  field='up')
  y <- rgdx(fnIn,list(name='y0',form='full',uels=f,field='up'))
  chk <- chkRgdxRes (y, ywantUp, T)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'up',full,filtered) failed",chk$msg))
  }
  zwantUp <- list(name='z', type='variable', dim=0,
                  val=+Inf,
                  form='full', uels=f, domains=character(0),
                  field='up')
  z <- rgdx(fnIn,list(name='z',form='full',uels=f,field='up'))
  chk <- chkRgdxRes (z, zwantUp, T)
  if (!chk$same) {
    stop (paste("test rgdx(z,'up',full,filtered) failed",chk$msg))
  }
  # scale
  xwantS <- list(name='xpos0', type='variable', dim=0,
                 val=10,
                 form='full', uels=f, domains=character(0),
                 field='s')
  x <- rgdx(fnIn,list(name='xpos0',form='full',uels=f,field='s'))
  chk <- chkRgdxRes (x, xwantS, T)
  if (!chk$same) {
    stop (paste("test rgdx(xpos0,'s',full,filtered) failed",chk$msg))
  }
  ywantS <- list(name='y0', type='variable', dim=0,
                 val=1,
                 form='full', uels=f, domains=character(0),
                 field='s')
  y <- rgdx(fnIn,list(name='y0',form='full',uels=f,field='s'))
  chk <- chkRgdxRes (y, ywantS, T)
  if (!chk$same) {
    stop (paste("test rgdx(y0,'s',full,filtered) failed",chk$msg))
  }
  zwantS <- list(name='z', type='variable', dim=0,
                 val=1,
                 form='full', uels=f, domains=character(0),
                 field='s')
  z <- rgdx(fnIn,list(name='z',form='full',uels=f,field='s'))
  chk <- chkRgdxRes (z, zwantS, T)
  if (!chk$same) {
    stop (paste("test rgdx(z,'s',full,filtered) failed",chk$msg))
  }


  print ("test of rgdx on variable reads passed")
  TRUE   ## all tests passed: return TRUE
},

error = function(ex) { print ("test of rgdx on variable reads failed"); print(ex) ; FALSE }
)
