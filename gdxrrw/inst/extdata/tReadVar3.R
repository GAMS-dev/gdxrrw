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

tryCatch({
  print ("testing rgdx on variable reads")
  rgdx('?')
  fnIn <- "tReadVar3.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }

  ### ---------- reading form=sparse, no filter, no compress
  # level
  xwant <- list(name="x", type="variable", dim=3,
                val=matrix(c( 1,1,2,   1
		             ,1,2,1,  10
		             ,1,2,2,  11
		             ,2,1,1, 100
		             ,2,1,2, 101
		             ,2,2,1, 110
		             ,2,2,2,   6), nrow=7, ncol=4,byrow=T),
                form="sparse",
                uels=list(iUels,jUels,kUels), domains=c("i","j","k"),
		field='l')
  x <- rgdx(fnIn,list(name='x',form='sparse',field='L'))
  chk <- chkRgdxRes (x, xwant)
  if (!chk$same) {
    stop (paste("test rgdx(x,'L',unfiltered,uncompressed) failed",chk$msg))
  }
  # marginal
  xwant <- list(name="x", type="variable", dim=3,
                val=matrix(c( 1,1,2,  .25
		             ,1,2,2,  .25), nrow=2, ncol=4,byrow=T),
                form="sparse",
                uels=list(iUels,jUels,kUels), domains=c("i","j","k"),
		field='m')
  x <- rgdx(fnIn,list(name='x',form='sparse',field='M'))
  chk <- chkRgdxRes (x, xwant)
  if (!chk$same) {
    stop (paste("test rgdx(x,'M',unfiltered,uncompressed) failed",chk$msg))
  }
  # lower
  xwant <- list(name="x", type="variable", dim=3,
                val=matrix(c( 1,2,1, -Inf
		             ,1,2,2,  100
		             ,2,2,2,    6), nrow=3, ncol=4,byrow=T),
                form="sparse",
                uels=list(iUels,jUels,kUels), domains=c("i","j","k"),
		field='lo')
  x <- rgdx(fnIn,list(name='x',form='sparse',field='lo'))
  chk <- chkRgdxRes (x, xwant)
  if (!chk$same) {
    stop (paste("test rgdx(x,'lo',unfiltered,uncompressed) failed",chk$msg))
  }
  # upper
  xwant <- list(name="x", type="variable", dim=3,
                val=matrix(c( 1,1,1,  525
		             ,1,1,2, +Inf
		             ,1,2,1, +Inf
		             ,1,2,2, +Inf
		             ,2,1,2, +Inf
		             ,2,2,1, +Inf
		             ,2,2,2,    6), nrow=7, ncol=4,byrow=T),
                form="sparse",
                uels=list(iUels,jUels,kUels), domains=c("i","j","k"),
		field='up')
  x <- rgdx(fnIn,list(name='x',form='sparse',field='up'))
  chk <- chkRgdxRes (x, xwant)
  if (!chk$same) {
    stop (paste("test rgdx(x,'lo',unfiltered,uncompressed) failed",chk$msg))
  }
  # scale
  xwant <- list(name="x", type="variable", dim=3,
                val=matrix(c( 1,1,1,    1
		             ,1,1,2,    1
		             ,1,2,1,    1
		             ,1,2,2,    1
		             ,2,1,1,    1
		             ,2,1,2,    1
		             ,2,2,1,   10
		             ,2,2,2,    1), nrow=8, ncol=4,byrow=T),
                form="sparse",
                uels=list(iUels,jUels,kUels), domains=c("i","j","k"),
		field='s')
  x <- rgdx(fnIn,list(name='x',form='sparse',field='s'))
  chk <- chkRgdxRes (x, xwant)
  if (!chk$same) {
    stop (paste("test rgdx(x,'lo',unfiltered,uncompressed) failed",chk$msg))
  }


  print ("test of rgdx on variable reads passed")
  TRUE   ## all tests passed: return TRUE
},

error = function(ex) { print ("test of rgdx on variable reads failed"); print(ex) ; FALSE }
)
