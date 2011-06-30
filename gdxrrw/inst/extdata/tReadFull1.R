### Test rgdx
# We read all the transport data using full reads and no filter

# compare the set s to the universe, return TRUE if the same, FALSE o/w
chkUni <- function(uni,s) {
  if (! is.vector(s))     return (FALSE);
  if (! is.vector(uni))   return (FALSE);
  n <- length(uni)    
  if (n != length(s))     return (FALSE);
  for (k in c(1:n)) {
    if (uni[k] != s[k]) {
      print ("chkUni: UEL with index ", k, " is wrong");
      return (FALSE);
    }
  }
  return (TRUE);
}


tryCatch({
  fn <- "trnsport.gdx"
  print ("Test rgdx with form='full' and no filter,")
  print ("using the transport data as the input")
  rgdx('?')

  u <- rgdx('trnsport');
  if (!is.list(u))
    stop ("Expected rgdx output to be in list form");

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
  for (d in dim(i$uels)) {
    if (! chkUni(u$uels,i$uels[[d]])) {
      stop ("Expected universe in dim ", d, " of i$uels");
    }
  }
  iv <- matrix(c(1,1,0,0,0), c(5,1));
  dd <- iv == i$val
  if (! prod(dd)) {
    stop ("Bad data in i$val: ", dd);
  }
  print ("Done reading set i");

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
  for (d in dim(j$uels)) {
    if (! chkUni(u$uels,j$uels[[d]])) {
      stop ("Expected universe in dim ", d, " of j$uels");
    }
  }
  jv <- matrix(c(0,0,1,1,1), c(5,1));
  dd <- jv == j$val
  if (! prod(dd)) {
    stop ("Bad data in j$val: ", dd);
  }
  print ("Done reading set j");

  print ("Successfully completed tests");
  return (TRUE)
}

, error = function(ex) { print(ex) ; return (FALSE) }
)
