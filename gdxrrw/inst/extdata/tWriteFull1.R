### Test wgdx
# We write all the transport data using full writes and a full universe

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

tryCatch({

  fn <- "twd1.gdx"

  print ("Test wgdx with form='full' and a full universe,")
  print ("using the transport data as the output target")
  wgdx('?')

  # this is the universe for this GDX file
  uu <- list(c("seattle", "san-diego", "new-york", "chicago", "topeka"))

  iv <- as.array(c(1,1,0,0,0))
  ilst <- list (name='i', type='set', dim=1, form='full', ts='canning plants', val=iv, uels=c(uu))

  jv <- as.array(c(0,0,1,1,1))
  jlst <- list (name='j', type='set', form='full', ts='markets', val=jv, uels=c(uu))

  av <- as.array(c(350,600,0,0,0))
  alst <- list (name='a', type='parameter', dim=1, form='full',
                ts='capacity of plant i in cases', val=av, uels=c(uu))

  bv <- as.array(c(0,0,325,300,275))
  blst <- list (name='b', type='parameter', form='full',
                ts='demand at market j in cases', val=bv, uels=c(uu))

  f <- 90
  d <- matrix(0,nrow=5,ncol=5)
  dd <- matrix(c(2.5, 1.7, 1.8,
                 2.5, 1.8, 1.4),  c(2,3), byrow=TRUE)
  d[(1:2),(3:5)] <- dd
  c <- d * f / 1000

  clst <- list (name='c', type='parameter', dim=2, form='full',
                ts='transport cost in thousands of dollars per case', val=c, uels=c(uu,uu))
  dlst <- list (name='d', type='parameter', form='full',
                ts='distance in thousands of miles', val=d, uels=c(uu,uu))

  flst <- list (name='f', type='parameter', form='full',
                ts='freight in dollars per case per thousand miles', val=f)

  wgdx (fn, ilst, jlst, alst, blst, clst, dlst, flst)

  if (file_test ('-f', fn) == TRUE) {
    print (paste("File", fn, "was created"))
  } else {
    stop (paste("FAIL: File", fn, "is not readable"))
  }
  rc <- system (paste("gdxdiff trnsport.gdx",fn,"releps=1e-15 id=i,j,a,b,c,d,f"))
  if (0 != rc) {
    stop(paste("Bad return from gdxdiff: wanted 0, got",rc))
  } else {
    print ("gdxdiff call succeeded")
  }
  return (TRUE)
}

, error = function(ex) { print(ex) ; return (FALSE) }
)
