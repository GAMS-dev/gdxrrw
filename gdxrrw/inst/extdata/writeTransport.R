### Example showing one way to write the data (sets and parameters)
### for the transport model

tryCatch({

  fn <- "mytransport.gdx"
  if (file_test ('-f', fn) == TRUE) {
    file.remove (fn)
  }

  print ("Example showing how to reproduce the transport data using wgdx")

  # set i: supply nodes
  si <- list(c("seattle", "san-diego"))

  # set j: demand nodes
  sj <- list(c("new-york", "chicago", "topeka"))

  ilst <- list (name='i', type='set', ts='canning plants', uels=c(si))

  jlst <- list (name='j', type='set', ts='markets', uels=c(sj))

  av <- as.array(c(350,600))
  alst <- list (name='a', type='parameter', form='full',
                ts='capacity of plant i in cases', val=av, uels=c(si))

  bv <- as.array(c(325, 300, 275))
  blst <- list (name='b', type='parameter', form='full',
                ts='demand at market j in cases', val=bv, uels=c(sj))

  f <- 90
  d <- matrix(c(2.5, 1.7, 1.8,
                2.5, 1.8, 1.4),  c(2,3), byrow=TRUE)
  c <- d * f / 1000

  clst <- list (name='c', type='parameter', form='full',
                ts='transport cost in thousands of dollars per case', val=c, uels=c(si,sj))
  dlst <- list (name='d', type='parameter', form='full',
                ts='distance in thousands of miles', val=d, uels=c(si,sj))

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
}

, error = function(ex) {print(ex)}
)
