### Test wgdx

tryCatch({

  fn <- "output.gdx"

  print ("Test wgdx using the transport data as the output target");
  # wgdx('?');

  # this is the universe for this GDX file
  uu <- list(c("seattle", "san-diego", "new-york", "chicago", "topeka"))

  iv <- matrix(c(1:2), c(2,1));
  ilst <- list (name='i', type='set', dim=1, form='sparse', ts='canning plants', val=iv, uels=c(uu));

  jv <- matrix(c(3:5), c(3,1));
  jlst <- list (name='j', type='set', dim=1, form='sparse', ts='markets', val=jv, uels=c(uu));

  wgdx (fn, ilst, jlst);

  if (file_test ('-f', fn) == TRUE) {
    print (paste("File", fn, "was created"))
  } else {
    stop (paste("FAIL: File", fn, "is not readable"))
  }
  rc <- system (paste("gdxdiff trnsport.gdx",fn,"id=i,j,c"))
  if (0 != rc) {
    stop(paste("Bad return from gdxdiff: wanted 0, got",rc))
  } else {
    print ("gdxdiff call succeeded")
  }
}

, error = function(ex) {print(ex)}
);
