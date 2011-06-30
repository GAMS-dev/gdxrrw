### Test wgdx
# We write all the transport data using sparse writes and universe
# subsets, with or without values


tryCatch({

  fn <- "tws2.gdx"

  print ("Test wgdx with form='sparse' and universe subsets,")
  print ("using the transport data as the output target")
  wgdx('?')

  # set i: supply nodes
  si <- list(c("seattle", "san-diego"))

  # set j: demand nodes
  sj <- list(c("new-york", "chicago", "topeka"))

  ilst <- list (name='i', type='set', dim=1, form='sparse', ts='canning plants',
                uels=c(si))

  jv <- matrix(c(1:3), c(3,1))
  jlst <- list (name='j', type='set', dim=1, form='sparse', ts='markets',
          val=jv, uels=c(sj))

  av <- matrix(c(1:2,350,600), c(2,2))
  alst <- list (name='a', type='parameter', dim=1, form='sparse',
                ts='capacity of plant i in cases', val=av, uels=c(si))

  bv <- matrix(c(1, 325,
                 2, 300,
                 3, 275),  c(3,1), byrow=TRUE)
  blst <- list (name='b', type='parameter', form='sparse',
                ts='demand at market j in cases', val=bv, uels=c(sj))

  f <- 90
  fv <- matrix(c(f),c(1,1))
  dv <- matrix(c(1, 1, 2.5,
                 1, 2, 1.7,
                 1, 3, 1.8,
                 2, 1, 2.5,
                 2, 2, 1.8,
                 2, 3, 1.4),  c(6,3), byrow=TRUE)
  cv <- dv
  cv[,3] <- dv[,3] * f / 1000

  clst <- list (name='c', type='parameter', dim=2, form='sparse',
                ts='transport cost in thousands of dollars per case', val=cv, uels=c(si,sj))
  dlst <- list (name='d', type='parameter', form='sparse',
                ts='distance in thousands of miles', val=dv, uels=c(si,sj))

  flst <- list (name='f', type='parameter', dim=0, form='sparse',
                ts='freight in dollars per case per thousand miles', val=fv)

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
