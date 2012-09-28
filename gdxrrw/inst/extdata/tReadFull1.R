### Test rgdx on all the transport data using full reads and no filter
### N.B.  This test depends on the domain info in the GDX file
### wanted lists produced with    dump("listName",file="")

source ("chkSame.R")

# compare the set s to the universe, return TRUE if the same, FALSE o/w
chkUni <- function(uni,s) {
  if (! is.vector(s))     return (FALSE)
  if (! is.vector(uni))   return (FALSE)
  n <- length(uni)
  if (n != length(s))     return (FALSE)
  for (k in c(1:n)) {
    if (uni[k] != s[k]) {
      print ("chkUni: UEL with index ", k, " is wrong")
      return (FALSE)
    }
  }
  return (TRUE)
}


tryCatch({
  fn <- "trnsport.gdx"
  print ("Test rgdx with form='full' and no filter,")
  print ("using the transport data as the input")
  rgdx('?')

  u <- rgdx('trnsport')
  if (!is.list(u))
    stop ("Expected rgdx output to be in list form")

  lst <- list(name='i',form='full')
  i <- rgdx('trnsport',lst)
  if (i$name != 'i')
    stop ("Expected i$name to be 'i', got ", i$name)
  if (i$type != "set")
    stop ("Expected i$type to be 'set', got ", i$type)
  if (i$dim != 1)
    stop ("Expected i$dim to be 1, got ", i$dim)
  if (i$form != "full")
    stop ("Expected i$form to be 'full', got ", i$form)
  if (length(i$uels) != i$dim)
    stop ("Expected length of i$uels to be equal i$dim, got ", length(i$uels))
  if (! chkUni(u$uels,i$uels[[1]])) {
    stop ("Expected universe in dim 1 of i$uels")
  }
  iv <- matrix(c(1,1,0,0,0), c(5,1))
  dd <- iv == i$val
  if (! prod(dd)) {
    stop ("Bad data in i$val: ", dd)
  }
  print ("Done reading set i")

  lst <- list(name='j',form='full')
  j <- rgdx('trnsport',lst)
  if (j$name != 'j')
    stop ("Expected j$name to be 'j', got ", j$name)
  if (j$type != "set")
    stop ("Expected j$type to be 'set', got ", j$type)
  if (j$dim != 1)
    stop ("Expected j$dim to be 1, got ", j$dim)
  if (j$form != "full")
    stop ("Expected j$form to be 'full', got ", j$form)
  if (length(j$uels) != j$dim)
    stop ("Expected length of j$uels to be equal j$dim, got ", length(j$uels))
  if (! chkUni(u$uels,j$uels[[1]])) {
    stop ("Expected universe in dim 1 of j$uels")
  }
  jv <- matrix(c(0,0,1,1,1), c(5,1))
  dd <- jv == j$val
  if (! prod(dd)) {
    stop ("Bad data in j$val: ", dd)
  }
  print ("Done reading set j")

  lst <- list(name='f',form='full')
  f <- rgdx('trnsport',lst)
  if (f$name != 'f')
    stop ("Expected f$name to be 'f', got ", f$name)
  if (f$type != "parameter")
    stop ("Expected f$type to be 'parameter', got ", f$type)
  if (f$dim != 0)
    stop ("Expected f$dim to be 0, got ", f$dim)
  if (f$form != "full")
    stop ("Expected f$form to be 'full', got ", f$form)
  if (! is.vector(f$val))
    stop ("Expected f$val to be a vector")
  dd <- 90 == f$val
  if (! prod(dd)) {
    stop ("Bad data in f$val: ", dd)
  }
  if (length(f$uels) != 0)
    stop ("Expected f$uels to be empty")
  print ("Done reading scalar f")

  a <- rgdx('trnsport',list(name='a',form='full'))
  av <- matrix(c(350,600), c(2,1))
  awant <- list(name="a", type="parameter", dim=1,
                val=av,
                form="full",
                uels=list(c("seattle","san-diego")))
  chk <- chkRgdxRes (a, awant)
  if (!chk$same) {
    stop (paste("test rgdx(a,form='full') failed",chk$msg))
  }
  print ("Done reading parameter a")

  b <- rgdx('trnsport',list(name='b',form='full'))
  bv <- matrix(c(325,300,275), c(3,1))
  bwant <- list(name="b", type="parameter", dim=1,
                val=bv,
                form="full",
                uels=list(c("new-york","chicago","topeka")))
  chk <- chkRgdxRes (b, bwant)
  if (!chk$same) {
    stop (paste("test rgdx(b,form='full') failed",chk$msg))
  }
  print ("Done reading parameter b")

  c <- rgdx('trnsport',list(name='c',form='full'))
  dv <- matrix(c(2.5, 1.7, 1.8,
                 2.5, 1.8, 1.4),  c(2,3), byrow=TRUE)
  cv <- dv * 90 / 1000
  cwant <- list(name="c", type="parameter", dim=2,
                val=cv,
                form="full",
                uels=list(c("seattle","san-diego"),c("new-york","chicago","topeka")))
  chk <- chkRgdxRes (c, cwant)
  if (!chk$same) {
    stop (paste("test rgdx(c,form='full') failed",chk$msg))
  }
  print ("Done reading parameter c")


  print ("Successfully completed tests")
  return (TRUE)
}

, error = function(ex) { print(ex) ; return (FALSE) }
)
