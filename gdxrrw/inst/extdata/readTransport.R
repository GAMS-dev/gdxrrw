### Test rgdx
# e <- simpleError ("rgdx test error");

tryCatch({
  print ("Test rgdx using the transport data as input");
  rgdx('?');

  u <- rgdx('trnsport');
  if (!is.list(u))
    stop ("Expected rgdx output to be in list form");
  if (length(u) != 6)
    stop ("Expected the list returned to have 6 components");
  if (typeof(u$name) != "NULL")
    stop ("Expected u$name to be NULL");	
  if (typeof(u$type) != "NULL")
    stop ("Expected u$type to be NULL");	
  if (typeof(u$dim) != "NULL")
    stop ("Expected u$dim to be NULL");	
  if (typeof(u$val) != "NULL")
    stop ("Expected u$val to be NULL");	
  if (typeof(u$form) != "NULL")
    stop ("Expected u$form to be NULL");	
  allUels <- c('seattle', "san-diego", "new-york", "chicago", "topeka");
  for (i in 1:length(u$uels)) {
    if (u$uels[i] != allUels[i]) {
      stop ("UEL with index ", i, " is wrong");	
    }
  }

  lst <- list(name='j');
  j <- rgdx('trnsport',lst);
  if (!is.list(j))
    stop ("Expected rgdx output to be in list form");
  if (length(j) != 6)
    stop ("Expected the list returned to have 6 components");
  if (j$name != 'j')
    stop ("Expected j$name to be 'j', got ", j$name);
  if (j$type != "set")
    stop ("Expected j$type to be 'set', got ", j$type);
  if (j$dim != 1)
    stop ("Expected j$dim to be 1, got ", j$dim);
  if (j$form != "sparse")
    stop ("Expected j$form to be 'sparse', got ", j$form);
  if (length(dim(j$val)) != 2)
    stop ("Expected j$val to be a matrix");
  if (dim(j$val)[1] != 3)
    stop ("Expected dim(j$val)[1] to be 3");
  if (dim(j$val)[2] != 1)
    stop ("Expected dim(j$val)[2] to be 1");

  jv <- matrix(c(3:5), c(3,1));
  for (i in dim(j$val)[1] ) {
    if (j$val[i,1] != jv[i,1]) {
      stop ("Bad data in j$val, row", i);
    }
  }

  print ("Successfully completed tests");
}
, error = function(ex) {print(ex)}
#, finally = print ("All done with readTransport.R test" )
 );
