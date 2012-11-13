### Test rgdx
# We read the transport data using defaults: form=sparse and a full universe
# This test does near-exhaustive checks on the returned structure

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

source ("chkSame.R")

iUels <- c("seattle", "san-diego")
iCard <- length(iUels)
jUels <- c("new-york", "chicago", "topeka")
jCard <- length(jUels)
uUels <- c(iUels, jUels)
uCard <- length(uUels)

iVals <- matrix(1:iCard, nrow=iCard, ncol=1)
jVals <- matrix(iCard+(1:jCard), nrow=jCard, ncol=1)
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

tryCatch({
  print ("Test rgdx with defaults (form='sparse' and no filter)")
  print ("using the transport data as input")
  rgdx('?')

  u <- rgdx('trnsport')
  uwant <- list(name="*", type="set", dim=1,
                val=NULL,
                form=NULL,
                uels=uUels)
  chk <- chkRgdxRes (u, uwant)
  if (!chk$same) {
    stop (paste("test rgdx('gdxname') to read universe failed",chk$msg))
  }
  print ("Done reading universe")

  i <- rgdx('trnsport',list(name='i'))
  iwant <- list(name="i", type="set", dim=1,
                val=iVals,
                form="sparse",
                uels=list(uUels),
                domains=c("*"))
  chk <- chkRgdxRes (i, iwant)
  if (!chk$same) {
    stop (paste("test rgdx(i,form=sparse) failed",chk$msg))
  }
  print ("Done reading set i")

  j <- rgdx('trnsport',list(name='j'))
  jwant <- list(name="j", type="set", dim=1,
                val=jVals,
                form="sparse",
                uels=list(uUels),
                domains=c("*"))
  chk <- chkRgdxRes (j, jwant)
  if (!chk$same) {
    stop (paste("test rgdx(j,form=sparse) failed",chk$msg))
  }
  print ("Done reading set j")

  f <- rgdx('trnsport',list(name='f'))
  fwant <- list(name="f", type="parameter", dim=0,
                val=matrix(90,c(1,1)),
                form="sparse",
                uels=list(),
                domains=character(0) )
  chk <- chkRgdxRes (f, fwant)
  if (!chk$same) {
    stop (paste("test rgdx(f,form=sparse) failed",chk$msg))
  }
  print ("Done reading scalar f")

  a <- rgdx('trnsport',list(name='a'))
  awant <- list(name="a", type="parameter", dim=1,
                val=aVals,
                form="sparse",
                uels=list(iUels),
                domains=c("i") )
  chk <- chkRgdxRes (a, awant)
  if (!chk$same) {
    stop (paste("test rgdx(a,form='sparse') failed",chk$msg))
  }
  print ("Done reading parameter a")

  b <- rgdx('trnsport',list(name='b'))
  bwant <- list(name="b", type="parameter", dim=1,
                val=bVals,
                form="sparse",
                uels=list(jUels),
                domains=c('j') )
  chk <- chkRgdxRes (b, bwant)
  if (!chk$same) {
    stop (paste("test rgdx(b,form='sparse') failed",chk$msg))
  }
  print ("Done reading parameter b")

  c <- rgdx('trnsport',list(name='c'))
  cwant <- list(name="c", type="parameter", dim=2,
                val=cVals,
                form="sparse",
                uels=list(iUels,jUels),
                domains=c("i","j") )
  chk <- chkRgdxRes (c, cwant)
  if (!chk$same) {
    stop (paste("test rgdx(c,form='sparse') failed",chk$msg))
  }
  print ("Done reading parameter c")

  d <- rgdx('trnsport',list(name='d'))
  dwant <- list(name="d", type="parameter", dim=2,
                val=dVals,
                form="sparse",
                uels=list(iUels,jUels),
                domains=c("i","j") )
  chk <- chkRgdxRes (d, dwant)
  if (!chk$same) {
    stop (paste("test rgdx(d,form='sparse') failed",chk$msg))
  }
  print ("Done reading parameter d")

  x <- rgdx('trnsport',list(name='x'))
  xwant <- list(name="x", type="variable", dim=2,
                val=xVals,
                form="sparse",
                uels=list(iUels,jUels),
                domains=c("i","j"),
                field="l",
                varTypeText='positive', typeCode=GMS_VARTYPE$POSITIVE)
  chk <- chkRgdxRes (x, xwant)
  if (!chk$same) {
    stop (paste("test rgdx(x,form='sparse') failed",chk$msg))
  }
  print ("Done reading variable x")

  z <- rgdx('trnsport',list(name='z'))
  zwant <- list(name="z", type="variable", dim=0,
                val=matrix(153.675,c(1,1)),
                form="sparse",
                uels=list(),
                domains=character(0),
                field="l",
                varTypeText='free', typeCode=GMS_VARTYPE$FREE)
  chk <- chkRgdxRes (z, zwant)
  if (!chk$same) {
    stop (paste("test rgdx(z,form='sparse') failed",chk$msg))
  }
  print ("Done reading variable z")


  print ("Successfully completed tests")
  return (TRUE)
}

, error = function(ex) { print(ex) ; return (FALSE) }
)
