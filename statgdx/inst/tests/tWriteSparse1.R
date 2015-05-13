### Test wgdx
# We write all the transport data using sparse writes and a full universe

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

testName <- 'writing trnsport with form="sparse" and full universe'
logFile <- 'diffLog.txt'

errFunc <- function(ex) {
  print (paste0("test of wgdx on ",testName,": FAILED"))
  print (paste("Check file", logFile, "for possible gdxdiff output"))
  print (ex)
  FALSE
} # errFunc

tryCatch({
  print (paste("testing wgdx on", testName))
  wgdx('?')
  fn <- "tws1.gdx"

  # this is the universe for this GDX file
  uu <- list(c("seattle", "san-diego", "new-york", "chicago", "topeka"))

  iv <- matrix(c(1:2), c(2,1))
  ilst <- list (name='i', type='set', dim=1, form='sparse', ts='canning plants', val=iv, uels=c(uu))

  jv <- matrix(c(3:5), c(3,1))
  jlst <- list (name='j', type='set', dim=1, form='sparse', ts='markets', val=jv, uels=c(uu))

  av <- matrix(c(1:2,350,600), c(2,2))
  alst <- list (name='a', type='parameter', dim=1, form='sparse',
                ts='capacity of plant i in cases', val=av, uels=c(uu))

  bv <- matrix(c(3, 325,
                 4, 300,
                 5, 275),  c(3,1), byrow=TRUE)
  blst <- list (name='b', type='parameter', dim=1, form='sparse',
                ts='demand at market j in cases', val=bv, uels=c(uu))

  f <- 90
  fv <- matrix(c(90),c(1,1))
  dv <- matrix(c(1, 3, 2.5,
                 1, 4, 1.7,
                 1, 5, 1.8,
                 2, 3, 2.5,
                 2, 4, 1.8,
                 2, 5, 1.4),  c(6,3), byrow=TRUE)
  cv <- dv
  cv[,3] <- dv[,3] * f / 1000

  clst <- list (name='c', type='parameter', dim=2, form='sparse',
                ts='transport cost in thousands of dollars per case', val=cv, uels=c(uu,uu))
  dlst <- list (name='d', type='parameter', dim=2, form='sparse',
                ts='distance in thousands of miles', val=dv, uels=c(uu,uu))

  flst <- list (name='f', type='parameter', dim=0, form='sparse',
                ts='freight in dollars per case per thousand miles', val=fv)

  wgdx (fn, ilst, jlst, alst, blst, clst, dlst, flst)

  if (file_test ('-f', fn) == TRUE) {
    print (paste("File", fn, "was created"))
  } else {
    stop (paste("FAIL: File", fn, "is not readable"))
  }
  rc <- system2 ("gdxdiff",args=c("trnsport.gdx", fn, "releps=1e-15",
                               "id=i,j,a,b,c,d,f"), stdout=logFile)
  if (0 != rc) {
    stop(paste("Bad return from gdxdiff: wanted 0, got",rc))
  } else {
    print ("gdxdiff call succeeded")
  }

  print (paste0("test of wgdx on ", testName, ": PASSED"))
  suppressWarnings(file.remove(logFile))
  invisible(TRUE)   ## all tests passed: return TRUE
},

error = errFunc
)
