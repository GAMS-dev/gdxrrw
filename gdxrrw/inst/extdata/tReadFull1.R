### Test rgdx
# We read all the transport data using full reads and no filter

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

  lst <- list(name='a',form='full')
  a <- rgdx('trnsport',lst)
  if (a$name != 'a')
    stop ("Expected a$name to be 'a', got ", a$name)
  if (a$type != "parameter")
    stop ("Expected a$type to be 'parameter', got ", a$type)
  if (a$dim != 1)
    stop ("Expected a$dim to be 1, got ", a$dim)
  if (a$form != "full")
    stop ("Expected a$form to be 'full', got ", a$form)
  if (! is.matrix(a$val))
    stop ("Expected a$val to be a matrix")
  av <- matrix(c(350,600,0,0,0), c(5,1))
  dd <- av == a$val
  if (! prod(dd)) {
    stop ("Bad data in a$val: ", dd)
  }
  if (length(a$uels) != 1)
    stop ("Expected length of a$uels to be 1, got ", length(a$uels))
  if (! chkUni(u$uels,a$uels[[1]])) {
    stop ("Expected universe in dim 1 of a$uels")
  }
  print ("Done reading parameter a")

  lst <- list(name='b',form='full')
  b <- rgdx('trnsport',lst)
  if (b$name != 'b')
    stop ("Expected b$name to be 'b', got ", b$name)
  if (b$type != "parameter")
    stop ("Expected b$type to be 'parameter', got ", b$type)
  if (b$dim != 1)
    stop ("Expected b$dim to be 1, got ", b$dim)
  if (b$form != "full")
    stop ("Expected b$form to be 'full', got ", b$form)
  if (! is.matrix(b$val))
    stop ("Expected b$val to be a matrix")
  bv <- matrix(c(0,0,325,300,275), c(5,1))
  dd <- bv == b$val
  if (! prod(dd)) {
    stop ("Bad data in b$val: ", dd)
  }
  if (length(b$uels) != 1)
    stop ("Expected length of b$uels to be 1, got ", length(b$uels))
  if (! chkUni(u$uels,b$uels[[1]])) {
    stop ("Expected universe in dim 1 of b$uels")
  }
  print ("Done reading parameter b")

  lst <- list(name='c',form='full')
  c <- rgdx('trnsport',lst)
  if (c$name != 'c')
    stop ("Expected c$name to be 'c', got ", c$name)
  if (c$type != "parameter")
    stop ("Expected c$type to be 'parameter', got ", c$type)
  if (c$dim != 2)
    stop ("Expected c$dim to be 2, got ", c$dim)
  if (c$form != "full")
    stop ("Expected c$form to be 'full', got ", c$form)
  if (length(dim(c$val)) != 2)
    stop ("Expected c$val to be a matrix")
  dv <- matrix(0,nrow=5,ncol=5)
  dvp <- matrix(c(2.5, 1.7, 1.8,
                  2.5, 1.8, 1.4),  c(2,3), byrow=TRUE)
  dv[(1:2),(3:5)] <- dvp
  cv <- dv * 90 / 1000
  dd <- abs(cv - c$val) < 1e-13
  if (! prod(dd)) {
    stop ("Bad data in c$val: ", dd)
  }
  for (k in c(1:length(c$uels))) {
    if (! chkUni(u$uels,c$uels[[k]])) {
      stop ("Expected universe in dim ", k, " of c$uels")
    }
  }
  print ("Done reading parameter c")


  print ("Successfully completed tests")
  return (TRUE)
}

, error = function(ex) { print(ex) ; return (FALSE) }
)
