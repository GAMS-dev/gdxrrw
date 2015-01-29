## test wgdx with form='full' and a full universe
## We write all the transport data

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

testName <- 'writing trnsport with form="full" and full universe'
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
  fn <- "twd1.gdx"

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
  rc <- system2 ("gdxdiff",args=c("trnsport.gdx", fn,
                               "releps=1e-15", "id=i,j,a,b,c,d,f"), stdout=logFile)
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
