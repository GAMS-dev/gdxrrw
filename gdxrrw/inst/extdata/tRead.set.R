### Test rgdx.set
# We read 3 sets I,J,K and a subset over their Cartesian product,
# first from GDX and then from .csv files produced by GAMS,
# and compare the contents to be sure we get the same results

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

tryCatch({
  ix <- rgdx.set('ijk','i',names="I")
  jx <- rgdx.set('ijk','j',names="J")
  kx <- rgdx.set('ijk','k',names="K")

  iv <- read.csv("I.csv")
  jv <- read.csv("J.csv")
  kv <- read.csv("K.csv")

  # both rgdx.set and read.csv return a data frame with one factor for
  # each set/column.  We extract the factors and compare them.  Note
  # that we compare the characters in the factors, not the scheme to
  # encode these characters.

  ixf <- ix$I
  jxf <- jx$J
  kxf <- kx$K
  ivf <- iv$I
  jvf <- jv$J
  kvf <- kv$K
  ixfc <- as.character(ixf)
  jxfc <- as.character(jxf)
  kxfc <- as.character(kxf)
  ivfc <- as.character(ivf)
  jvfc <- as.character(jvf)
  kvfc <- as.character(kvf)

  if (prod(ixfc != ivfc)) stop ("mismatch in set I")
  if (prod(jxfc != jvfc)) stop ("mismatch in set J")
  if (prod(kxfc != kvfc)) stop ("mismatch in set K")

  ijkx <- rgdx.set('ijk','ijk',names=c("I","J","K"))
  ijkv <- read.csv("ijk.csv")

  ixf <- ijkx$I
  jxf <- ijkx$J
  kxf <- ijkx$K
  ivf <- ijkv$I
  jvf <- ijkv$J
  kvf <- ijkv$K
  ixfc <- as.character(ixf)
  jxfc <- as.character(jxf)
  kxfc <- as.character(kxf)
  ivfc <- as.character(ivf)
  jvfc <- as.character(jvf)
  kvfc <- as.character(kvf)

  if (prod(ixfc != ivfc)) stop ("mismatch in set ijk, index I")
  if (prod(jxfc != jvfc)) stop ("mismatch in set ijk, index J")
  if (prod(kxfc != kvfc)) stop ("mismatch in set ijk, index K")

  print ("All tests for rgdx.set passed")
  return (TRUE)
}

, error = function(ex) { print(ex) ; return (FALSE) }
)
