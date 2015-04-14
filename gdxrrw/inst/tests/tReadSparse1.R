### Test rgdx
# We read the transport data using defaults: form=sparse and a full universe
# This test does near-exhaustive checks on the returned structure

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

source ("chkSame.R")
reqIdent <- TRUE

iUels <- c("seattle", "san-diego")
iCard <- length(iUels)
jUels <- c("new-york", "chicago", "topeka")
jCard <- length(jUels)
uUels <- c(iUels, jUels)
uCard <- length(uUels)

iVals <- matrix(c(1,2), nrow=iCard, ncol=1)
jVals <- matrix(iCard+c(1,2,3), nrow=jCard, ncol=1)
aVals <- matrix(c(1:iCard,350,600), nrow=iCard, ncol=2)
bVals <- matrix(c(1:jCard,325,300,275), nrow=jCard, ncol=2)
dVals <- matrix(c(1,1, 2.5,
                  1,2, 1.7,
                  1,3, 1.8,
                  2,1, 2.5,
                  2,2, 1.8,
                  2,3, 1.4) , c(6,3), byrow=TRUE)
cVals <- dVals
cVals[,3] <- dVals[,3] * 90 / 1000
xVals <- matrix(c(1,1, 50,
                  1,2, 300,
                  2,1, 275,
                  2,3, 275) , c(4,3), byrow=TRUE)

fields <- c('l','m','lo','up','s')
nFields <- length(fields)
lev <- 1
mar <- 2
low <- 3
upp <- 4
sca <- 5

tryCatch({
  print ("Test rgdx with defaults (form='sparse' and no filter)")
  print ("using the transport data as input")
  rgdx('?')

  u <- rgdx('trnsport')
  uwant <- list(name="*", type="set", dim=1L,
                val=NULL,
                form=NULL,
                uels=uUels)
  chk <- chkRgdxRes (u, uwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx('gdxname') to read universe failed:",chk$msg))
  }
  print ("Done reading universe")

  i <- rgdx('trnsport',list(name='i'))
  iwant <- list(name="i", type="set", dim=1L,
                val=iVals,
                form="sparse",
                uels=list(uUels),
                domains=c("*"),
                domInfo="none")
  chk <- chkRgdxRes (i, iwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(i,form=sparse) failed:",chk$msg))
  }
  print ("Done reading set i")

  j <- rgdx('trnsport',list(name='j'))
  jwant <- list(name="j", type="set", dim=1L,
                val=jVals,
                form="sparse",
                uels=list(uUels),
                domains=c("*"),
                domInfo="none")
  chk <- chkRgdxRes (j, jwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(j,form=sparse) failed:",chk$msg))
  }
  print ("Done reading set j")

  f <- rgdx('trnsport',list(name='f'))
  fwant <- list(name="f", type="parameter", dim=0L,
                val=matrix(90,c(1,1)),
                form="sparse",
                uels=list(),
                domains=character(0),
                domInfo="NA")
  chk <- chkRgdxRes (f, fwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(f,form=sparse) failed",chk$msg))
  }
  print ("Done reading scalar f")

  a <- rgdx('trnsport',list(name='a'))
  awant <- list(name="a", type="parameter", dim=1L,
                val=aVals,
                form="sparse",
                uels=list(iUels),
                domains=c("i"),
                domInfo="full")
  chk <- chkRgdxRes (a, awant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(a,form='sparse') failed:",chk$msg))
  }
  print ("Done reading parameter a")

  b <- rgdx('trnsport',list(name='b'))
  bwant <- list(name="b", type="parameter", dim=1L,
                val=bVals,
                form="sparse",
                uels=list(jUels),
                domains=c('j'),
                domInfo='full')
  chk <- chkRgdxRes (b, bwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(b,form='sparse') failed:",chk$msg))
  }
  print ("Done reading parameter b")

  c <- rgdx('trnsport',list(name='c'))
  cwant <- list(name="c", type="parameter", dim=2L,
                val=cVals,
                form="sparse",
                uels=list(iUels,jUels),
                domains=c("i","j"),
                domInfo="full")
  # cVals is not bitwise correct
  chk <- chkRgdxRes (c, cwant, reqIdent=F)
  if (!chk$same) {
    stop (paste("test rgdx(c,form='sparse') failed:",chk$msg))
  }
  print ("Done reading parameter c")

  d <- rgdx('trnsport',list(name='d'))
  dwant <- list(name="d", type="parameter", dim=2L,
                val=dVals,
                form="sparse",
                uels=list(iUels,jUels),
                domains=c("i","j"),
                domInfo="full")
  chk <- chkRgdxRes (d, dwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(d,form='sparse') failed:",chk$msg))
  }
  print ("Done reading parameter d")

  xwant <- list(name="x", type="variable", dim=2L,
                val=xVals,
                form="sparse",
                uels=list(iUels,jUels),
                domains=c("i","j"),
                domInfo="full",
                field="l",
                varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx('trnsport',list(name='x'))
  chk <- chkRgdxRes (x, xwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(x,form='sparse') failed:",chk$msg))
  }
  t <- matrix(c( 1, 1, lev,   50
                ,1, 1, mar,    0
                ,1, 1, low,    0
                ,1, 1, upp,  Inf
                ,1, 1, sca,    1
                ,1, 2, lev,  300
                ,1, 2, mar,    0
                ,1, 2, low,    0
                ,1, 2, upp,  Inf
                ,1, 2, sca,    1
                ,1, 3, lev,    0
                ,1, 3, mar,    0.036
                ,1, 3, low,    0
                ,1, 3, upp,  Inf
                ,1, 3, sca,    1
                ,2, 1, lev,  275
                ,2, 1, mar,    0
                ,2, 1, low,    0
                ,2, 1, upp,  Inf
                ,2, 1, sca,    1
                ,2, 2, lev,    0
                ,2, 2, mar,    0.00900000000000001
                ,2, 2, low,    0
                ,2, 2, upp,  Inf
                ,2, 2, sca,    1
                ,2, 3, lev,  275
                ,2, 3, mar,    0
                ,2, 3, low,    0
                ,2, 3, upp,  Inf
                ,2, 3, sca,    1
               ), c(30,4), byrow=TRUE)
  xwantA <- list(name="x", type="variable", dim=2L,
                 val=t,
                 form="sparse",
                 uels=list(iUels,jUels,fields),
                 domains=c("i","j","_field"),
                 domInfo="full",
                 field="all",
                 varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  x <- rgdx('trnsport',list(name='x',field='all'))
  # the marginals are not bit-exact, so not identical
  chk <- chkRgdxRes (x, xwantA, reqIdent=F)
  if (!chk$same) {
    stop (paste("test rgdx(x,'all',form='sparse') failed:",chk$msg))
  }
  print ("Done reading variable x")

  zwant <- list(name="z", type="variable", dim=0L,
                val=matrix(153.675,c(1,1)),
                form="sparse",
                uels=list(),
                domains=character(0),
                domInfo='NA',
                field="l",
                varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx('trnsport',list(name='z'))
  chk <- chkRgdxRes (z, zwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,form='sparse') failed:",chk$msg))
  }
  t <- matrix(c( lev,  153.675
                ,mar,  0
                ,low,  -Inf
                ,upp,  +Inf
                ,sca,  1
               ), nrow=nFields, ncol=2, byrow=T)
  zwantA <- list(name="z", type="variable", dim=0L,
                 val=t,
                 form="sparse",
                 uels=list(fields),
                 domains=c('_field'),
                 domInfo="NA",
                 field="all",
                 varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  z <- rgdx('trnsport',list(name='z',field='all'))
  chk <- chkRgdxRes (z, zwantA, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(z,'all',form='sparse') failed:",chk$msg))
  }
  print ("Done reading variable z")

  print ("tReadSparse1 successfully completed")
  TRUE
}

, error = function(ex) { print(ex) ; FALSE }
)
