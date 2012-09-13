### Test rgdx
# We read the transport data using defaults: form=sparse and a full universe
# This test does near-exhaustive checks on the returned structure

tryCatch({
  print ("Test rgdx with defaults (form='sparse' and no filter)")
  print ("using the transport data as input")
  rgdx('?')

  u <- rgdx('trnsport')
  if (!is.list(u))
    stop ("Expected rgdx output to be in list form")
  if (length(u) != 6)
    stop ("Expected the list returned to have 6 components")
  if (u$name != '*')
    stop ("Expected u$name to be 'I', got ", u$name)
  if (u$type != "set")
    stop ("Expected u$type to be 'set', got ", u$type)
  if (u$dim != 1)
    stop ("Expected u$dim to be 1, got ", u$dim)
  if (typeof(u$val) != "NULL")
    stop ("Expected u$val to be NULL")
  if (typeof(u$form) != "NULL")
    stop ("Expected u$form to be NULL")
  allUels <- c('seattle', "san-diego", "new-york", "chicago", "topeka")
  for (k in 1:length(u$uels)) {
    if (u$uels[k] != allUels[k]) {
      stop ("UEL with index ", k, " is wrong")
    }
  }

  lst <- list(name='i')
  i <- rgdx('trnsport',lst)
  if (!is.list(i))
    stop ("Expected rgdx output to be in list form")
  if (length(i) != 6)
    stop ("Expected the list returned to have 6 components")
  if (i$name != 'i')
    stop ("Expected i$name to be 'i', got ", i$name)
  if (i$type != "set")
    stop ("Expected i$type to be 'set', got ", i$type)
  if (i$dim != 1)
    stop ("Expected i$dim to be 1, got ", i$dim)
  if (i$form != "sparse")
    stop ("Expected i$form to be 'sparse', got ", i$form)
  if (length(dim(i$val)) != 2)
    stop ("Expected i$val to be a matrix")
  if (dim(i$val)[1] != 2)
    stop ("Expected dim(i$val)[1] to be 2")
  if (dim(i$val)[2] != 1)
    stop ("Expected dim(i$val)[2] to be 1")
  iv <- matrix(c(1:2), c(2,1))
  for (k in dim(i$val)[1] ) {
    if (i$val[k,1] != iv[k,1]) {
      stop ("Bad data in i$val, row ", k)
    }
  }
  lst <- list(name='j')
  j <- rgdx('trnsport',lst)
  if (!is.list(j))
    stop ("Expected rgdx output to be in list form")
  if (length(j) != 6)
    stop ("Expected the list returned to have 6 components")
  if (j$name != 'j')
    stop ("Expected j$name to be 'j', got ", j$name)
  if (j$type != "set")
    stop ("Expected j$type to be 'set', got ", j$type)
  if (j$dim != 1)
    stop ("Expected j$dim to be 1, got ", j$dim)
  if (j$form != "sparse")
    stop ("Expected j$form to be 'sparse', got ", j$form)
  if (length(dim(j$val)) != 2)
    stop ("Expected j$val to be a matrix")
  if (dim(j$val)[1] != 3)
    stop ("Expected dim(j$val)[1] to be 3")
  if (dim(j$val)[2] != 1)
    stop ("Expected dim(j$val)[2] to be 1")
  jv <- matrix(c(3:5), c(3,1))
  for (k in dim(j$val)[1] ) {
    if (j$val[k,1] != jv[k,1]) {
      stop ("Bad data in j$val, row ", k)
    }
  }

  lst <- list(name='f')
  f <- rgdx('trnsport',lst)
  if (!is.list(f))
    stop ("Expected rgdx output to be in list form")
  if (length(f) != 6)
    stop ("Expected the list returned to have 6 components")
  if (f$name != 'f')
    stop ("Expected f$name to be 'f', got ", f$name)
  if (f$type != "parameter")
    stop ("Expected f$type to be 'parameter', got ", f$type)
  if (f$dim != 0)
    stop ("Expected f$dim to be 0, got ", f$dim)
  if (f$form != "sparse")
    stop ("Expected f$form to be 'sparse', got ", f$form)
  if (length(dim(f$val)) != 2)
    stop ("Expected f$val to be a matrix")
  if (dim(f$val)[1] != 1)
    stop ("Expected dim(f$val)[1] to be 1")
  if (dim(f$val)[2] != 1)
    stop ("Expected dim(f$val)[2] to be 1")
  if (f$val[1,1] != 90)
    stop ("Bad data in f$val, row 1")

  lst <- list(name='a')
  a <- rgdx('trnsport',lst)
  if (!is.list(a))
    stop ("Expected rgdx output to be in list form")
  if (length(a) != 6)
    stop ("Expected the list returned to have 6 components")
  if (a$name != 'a')
    stop ("Expected a$name to be 'a', got ", a$name)
  if (a$type != "parameter")
    stop ("Expected a$type to be 'parameter', got ", a$type)
  if (a$dim != 1)
    stop ("Expected a$dim to be 1, got ", a$dim)
  if (a$form != "sparse")
    stop ("Expected a$form to be 'sparse', got ", a$form)
  if (length(dim(a$val)) != 2)
    stop ("Expected a$val to be a matrix")
  if (dim(a$val)[1] != 2)
    stop ("Expected dim(a$val)[1] to be 2")
  if (dim(a$val)[2] != 2)
    stop ("Expected dim(a$val)[2] to be 2")
  av <- matrix(c(1,2,350,600), c(2,2))
  for (ki in dim(a$val)[1] ) {
    for (kj in dim(a$val)[2] ) {
      if (a$val[ki,kj] != av[ki,kj]) {
        stop ("Bad data in a$val, row ", ki, " col ", kj)
      }
    }
  }

  lst <- list(name='b')
  b <- rgdx('trnsport',lst)
  if (!is.list(b))
    stop ("Expected rgdx output to be in list form")
  if (length(b) != 6)
    stop ("Expected the list returned to have 6 components")
  if (b$name != 'b')
    stop ("Expected b$name to be 'b', got ", b$name)
  if (b$type != "parameter")
    stop ("Expected b$type to be 'parameter', got ", b$type)
  if (b$dim != 1)
    stop ("Expected b$dim to be 1, got ", b$dim)
  if (b$form != "sparse")
    stop ("Expected b$form to be 'sparse', got ", b$form)
  if (length(dim(b$val)) != 2)
    stop ("Expected b$val to be a matrix")
  if (dim(b$val)[1] != 3)
    stop ("Expected dim(b$val)[1] to be 3")
  if (dim(b$val)[2] != 2)
    stop ("Expected dim(b$val)[2] to be 2")
  bv <- matrix(c(3:5,325,300,275), c(3,2))
  for (ki in dim(b$val)[1] ) {
    for (kj in dim(b$val)[2] ) {
      if (b$val[ki,kj] != bv[ki,kj]) {
        stop ("Bad data in b$val, row ", ki, " col ", kj)
      }
    }
  }

  lst <- list(name='c')
  c <- rgdx('trnsport',lst)
  if (!is.list(c))
    stop ("Expected rgdx output to be in list form")
  if (length(c) != 6)
    stop ("Expected the list returned to have 6 components")
  if (c$name != 'c')
    stop ("Expected c$name to be 'c', got ", c$name)
  if (c$type != "parameter")
    stop ("Expected c$type to be 'parameter', got ", c$type)
  if (c$dim != 2)
    stop ("Expected c$dim to be 2, got ", c$dim)
  if (c$form != "sparse")
    stop ("Expected c$form to be 'sparse', got ", c$form)
  if (length(dim(c$val)) != 2)
    stop ("Expected c$val to be a matrix")
  if (dim(c$val)[1] != 6)
    stop ("Expected dim(c$val)[1] to be 6")
  if (dim(c$val)[2] != 3)
    stop ("Expected dim(c$val)[2] to be 3")
  cv <- matrix(c(1,3,.225,
                 1,4,.153,
                 1,5,.162,
                 2,3,.225,
                 2,4,.162,
                 2,5,.126) , c(6,3), byrow=TRUE)
  for (ki in dim(c$val)[1] ) {
    for (kj in dim(c$val)[2] ) {
      if (c$val[ki,kj] != cv[ki,kj]) {
        stop ("Bad data in c$val, row ", ki, " col ", kj)
      }
    }
  }

  lst <- list(name='d')
  d <- rgdx('trnsport',lst)
  if (!is.list(d))
    stop ("Expected rgdx output to be in list form")
  if (length(d) != 6)
    stop ("Expected the list returned to have 6 components")
  if (d$name != 'd')
    stop ("Expected d$name to be 'd', got ", d$name)
  if (d$type != "parameter")
    stop ("Expected d$type to be 'parameter', got ", d$type)
  if (d$dim != 2)
    stop ("Expected d$dim to be 2, got ", d$dim)
  if (d$form != "sparse")
    stop ("Expected d$form to be 'sparse', got ", d$form)
  if (length(dim(d$val)) != 2)
    stop ("Expected d$val to be a matrix")
  if (dim(d$val)[1] != 6)
    stop ("Expected dim(d$val)[1] to be 6")
  if (dim(d$val)[2] != 3)
    stop ("Expected dim(d$val)[2] to be 3")
  dv <- matrix(c(1,3,2.5,
                 1,4,1.7,
                 1,5,1.8,
                 2,3,2.5,
                 2,4,1.8,
                 2,5,1.4) , c(6,3), byrow=TRUE)
  for (ki in dim(d$val)[1] ) {
    for (kj in dim(d$val)[2] ) {
      if (d$val[ki,kj] != dv[ki,kj]) {
        stop ("Bad data in d$val, row ", ki, " col ", kj)
      }
    }
  }

  lst <- list(name='x')
  x <- rgdx('trnsport',lst)
  if (!is.list(x))
    stop ("Expected rgdx output to be in list form")
  if (length(x) != 7)
    stop ("Expected the list returned to have 7 components")
  if (x$name != 'x')
    stop ("Expected x$name to be 'x', got ", x$name)
  if (x$type != "variable")
    stop ("Expected x$type to be 'variable', got ", x$type)
  if (x$dim != 2)
    stop ("Expected x$dim to be 2, got ", x$dim)
  if (x$form != "sparse")
    stop ("Expected x$form to be 'sparse', got ", x$form)
  if (x$field != "l")
    stop ("Expected x$field to be 'l', got ", x$field)
  if (length(dim(x$val)) != 2)
    stop ("Expected x$val to be a matrix")
  if (dim(x$val)[1] != 4)
    stop ("Expected dim(x$val)[1] to be 4")
  if (dim(x$val)[2] != 3)
    stop ("Expected dim(x$val)[2] to be 3")
  xlv <- matrix(c(1,3, 50,
                  1,4,300,
                  2,3,275,
                  2,5,275) , c(4,3), byrow=TRUE)
  for (ki in dim(x$val)[1] ) {
    for (kj in dim(x$val)[2] ) {
      if (x$val[ki,kj] != xlv[ki,kj]) {
        stop ("Bad data in x$val, row ", ki, " col ", kj)
      }
    }
  }

  lst <- list(name='z')
  z <- rgdx('trnsport',lst)
  if (!is.list(z))
    stop ("Expected rgdx output to be in list form")
  if (length(z) != 7)
    stop ("Expected the list returned to have 7 components")
  if (z$name != 'z')
    stop ("Expected z$name to be 'z', got ", z$name)
  if (z$type != "variable")
    stop ("Expected z$type to be 'variable', got ", z$type)
  if (z$dim != 0)
    stop ("Expected z$dim to be 0, got ", z$dim)
  if (z$form != "sparse")
    stop ("Expected z$form to be 'sparse', got ", z$form)
  if (z$field != "l")
    stop ("Expected z$field to be 'l', got ", z$field)
  if (length(dim(z$val)) != 2)
    stop ("Expected z$val to be a matrix")
  if (dim(z$val)[1] != 1)
    stop ("Expected dim(z$val)[1] to be 1")
  if (dim(z$val)[2] != 1)
    stop ("Expected dim(z$val)[2] to be 1")
  if (z$val[1,1] != 153.675)
    stop ("Bad data in z$val")


  print ("Successfully completed tests")
  return (TRUE)
}

, error = function(ex) { print(ex) ; return (FALSE) }
)
