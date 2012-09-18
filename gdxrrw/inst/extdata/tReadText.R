#### test rgdx and reading set text data, i.e. I.te(I) in gams
#### test form=['sparse','full'] X [filtered,unfiltered] X compress=[T,F]

#### wanted lists produced with    dump("listName",file="")

source ("chkSame.R")

tryCatch({
  print ("testing rgdx handling of set text")
  rgdx('?')
  fnIn <- "teTest.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }
  iuels <- c("i1", "i2", "i3", "i4")
  icard <- length(iuels)
  itext <- iuels                        # since no text is in GDX
  juels <- c("j1", "j2", "j3")
  jcard <- length(juels)
  jtext <- c("j1 text", "j2 text", "j3 text")
  cuels <- c("berlin", "paris", "vienna")
  ccard <- length(cuels)
  ctext <- c("city of airport delays", "city of light", "city of dreams")
  u <- c(iuels, juels, cuels)
  ucard <- length(u)


  ## ---------- reading form=sparse, no filter, no compress

  iwant <- list(name="I", type="set", dim=1,
                val=matrix(c(1:4), nrow=4, ncol=1),
                form="sparse",
                uels=list(u), te=itext)
  i <- rgdx(fnIn,list(name='i',form='sparse',te=TRUE))
  chk <- chkRgdxRes (i, iwant)
  if (!chk$same) {
    stop (paste("test rgdx(i,unfiltered,uncompressed) failed",chk$msg))
  }

  jwant <- list(name="J", type="set", dim=1,
                val=matrix(c(5:7), nrow=3, ncol=1),
                form="sparse",
                uels=list(u),
                te=jtext)
  j <- rgdx(fnIn,list(name='j',form='sparse',te=TRUE))
  chk <- chkRgdxRes (j, jwant)
  if (!chk$same) {
    stop (paste("test rgdx(j,unfiltered,uncompressed) failed",chk$msg))
  }

  cwant <- list(name="c", type="set", dim=1,
                val=matrix(c(8:10), nrow=3, ncol=1),
                form="sparse",
                uels=list(u),
                ts='cities',
                te=ctext)
  c <- rgdx(fnIn,list(name='c',form='sparse',te=TRUE,ts=TRUE))
  chk <- chkRgdxRes (c, cwant)
  if (!chk$same) {
    stop (paste("test rgdx(c,unfiltered,uncompressed) failed",chk$msg))
  }

  ijwant <- list(name="IJ", type="set", dim=2,
                 val=matrix(c(1,5, 1,7, 2,6, 2,7, 3,7), nrow=5, ncol=2, byrow=TRUE),
                 form="sparse",
                 uels=list(u,u),
                 ts='',
                 te=c("one.one", "one.three", "two.two", "two.three", "three.three"))
  ij <- rgdx(fnIn,list(name='ij',form='sparse',te=TRUE,ts=TRUE))
  chk <- chkRgdxRes (ij, ijwant)
  if (!chk$same) {
    stop (paste("test rgdx(ij,unfiltered,uncompressed) failed",chk$msg))
  }

  ijcwant <- list(name="IJc", type="set", dim=3,
                  val=matrix(c(1,5,8, 1,7,8, 2,6,9, 2,7,9, 3,7,10),
                             nrow=5, ncol=3, byrow=TRUE),
                  form="sparse",
                  uels=list(u,u,u),
                  te=c("eins eins tempelhof", "eins drei tempelhof", "deux deux orly",
                    "deux trois orly", "drei drei schwechat"))
  ijc <- rgdx(fnIn,list(name='ijc',form='sparse',te=TRUE))
  chk <- chkRgdxRes (ijc, ijcwant)
  if (!chk$same) {
    stop (paste("test rgdx(ijc,unfiltered,uncompressed) failed",chk$msg))
  }

  ## ---------- reading form=sparse, no filter, compress=TRUE

  iwant <- list(name="I", type="set", dim=1,
                val=matrix(c(1:4), nrow=4, ncol=1),
                form="sparse",
                uels=list(iuels), te=itext)
  i <- rgdx(fnIn,list(name='i',form='sparse',te=TRUE,compress=TRUE))
  chk <- chkRgdxRes (i, iwant)
  if (!chk$same) {
    stop (paste("test rgdx(i,unfiltered,compressed) failed",chk$msg))
  }

  jwant <- list(name="J", type="set", dim=1,
                val=matrix(c(1:3), nrow=3, ncol=1),
                form="sparse",
                uels=list(juels),
                te=jtext)
  j <- rgdx(fnIn,list(name='j',form='sparse',te=TRUE,compress=TRUE))
  chk <- chkRgdxRes (j, jwant)
  if (!chk$same) {
    stop (paste("test rgdx(j,unfiltered,compressed) failed",chk$msg))
  }

  ijwant <- list(name="IJ", type="set", dim=2,
                 val=matrix(c(1,1, 1,3, 2,2, 2,3, 3,3), nrow=5, ncol=2, byrow=TRUE),
                 form="sparse",
                 uels=list(iuels[1:3],juels), # i4 is compressed out
                 ts='',
                 te=c("one.one", "one.three", "two.two", "two.three", "three.three"))
  ij <- rgdx(fnIn,list(name='ij',form='sparse',te=TRUE,ts=TRUE,compress=TRUE))
  chk <- chkRgdxRes (ij, ijwant)
  if (!chk$same) {
    stop (paste("test rgdx(ij,unfiltered,compressed) failed",chk$msg))
  }

  ijcwant <- list(name="IJc", type="set", dim=3,
                  val=matrix(c(1,1,1, 1,3,1, 2,2,2, 2,3,2, 3,3,3),
                             nrow=5, ncol=3, byrow=TRUE),
                  form="sparse",
                  uels=list(iuels[1:3],juels,cuels),
                  te=c("eins eins tempelhof", "eins drei tempelhof", "deux deux orly",
                    "deux trois orly", "drei drei schwechat"))
  ijc <- rgdx(fnIn,list(name='ijc',form='sparse',te=TRUE,compress=TRUE))
  chk <- chkRgdxRes (ijc, ijcwant)
  if (!chk$same) {
    stop (paste("test rgdx(ijc,unfiltered,compressed) failed",chk$msg))
  }

  ## ---------- reading form=sparse, filtered, no compress

  iwant <- list(name="I", type="set", dim=1,
                val=matrix(c(1:3), nrow=3, ncol=1),
                form="sparse",
                uels=list(iuels[2:4]), te=itext[2:4])
  i <- rgdx(fnIn,list(name='i',form='sparse',uels=list(iuels[2:4]),te=TRUE))
  chk <- chkRgdxRes (i, iwant)
  if (!chk$same) {
    stop (paste("test rgdx(i,filtered,uncompressed) failed",chk$msg))
  }

  jwant <- list(name="J", type="set", dim=1,
                val=matrix(c(1,2), nrow=2, ncol=1),
                form="sparse",
                uels=list(juels[c(1,3)]),
                te=jtext[c(1,3)])
  j <- rgdx(fnIn,list(name='j',form='sparse',uels=list(juels[c(1,3)]),te=TRUE))
  chk <- chkRgdxRes (j, jwant)
  if (!chk$same) {
    stop (paste("test rgdx(j,filtered,uncompressed) failed",chk$msg))
  }

  cwant <- list(name="c", type="set", dim=1,
                val=matrix(c(8:10), nrow=3, ncol=1),
                form="sparse",
                uels=list(u),
                ts='cities',
                te=ctext)
  c <- rgdx(fnIn,list(name='c',form='sparse',uels=list(u),te=TRUE,ts=TRUE))
  chk <- chkRgdxRes (c, cwant)
  if (!chk$same) {
    stop (paste("test rgdx(c,filtered,uncompressed) failed",chk$msg))
  }

  ijwant <- list(name="IJ", type="set", dim=2,
                 val=matrix(c(1,2, 2,2), nrow=2, ncol=2, byrow=TRUE),
                 form="sparse",
                 uels=list(iuels[2:4],juels[c(1,3)]),
                 ts='',
                 te=c("two.three", "three.three"))
  ij <- rgdx(fnIn,list(name='ij',form='sparse',uels=list(iuels[2:4],juels[c(1,3)]),te=TRUE,ts=TRUE))
  chk <- chkRgdxRes (ij, ijwant)
  if (!chk$same) {
    stop (paste("test rgdx(ij,filtered,uncompressed) failed",chk$msg))
  }

  ijcwant <- list(name="IJc", type="set", dim=3,
                  val=matrix(c(1,2,9, 2,2,10),
                             nrow=2, ncol=3, byrow=TRUE),
                  form="sparse",
                  uels=list(iuels[2:4],juels[c(1,3)],u),
                  te=c("deux trois orly", "drei drei schwechat"))
  ijc <- rgdx(fnIn,list(name='ijc',form='sparse',uels=list(iuels[2:4],juels[c(1,3)],u),te=TRUE))
  chk <- chkRgdxRes (ijc, ijcwant)
  if (!chk$same) {
    stop (paste("test rgdx(ijc,filtered,uncompressed) failed",chk$msg))
  }

  ## ---------- reading form=full, no filter, no compress
  ## still to do

  v <- array(0,c(ucard,1))
  v[(1:icard)] = 1
  te <- array("",c(ucard,1))
  te[(1:icard)] = itext
  iwant <- list(name="I", type="set", dim=1,
                val=v,
                form="full",
                uels=list(u), te=te)
  i <- rgdx(fnIn,list(name='i',form='full',te=TRUE))
  chk <- chkRgdxRes (i, iwant)
  if (!chk$same) {
    stop (paste("test rgdx(i,full,unfiltered,uncompressed) failed",chk$msg))
  }

  jblock <- icard + (1:jcard)
  v <- array(0,c(ucard,1))
  v[jblock] = 1
  te <- array("",c(ucard,1))
  te[jblock] = jtext
  jwant <- list(name="J", type="set", dim=1,
                val=v,
                form="full",
                uels=list(u), te=te)
  j <- rgdx(fnIn,list(name='j',form='full',te=TRUE))
  chk <- chkRgdxRes (j, jwant)
  if (!chk$same) {
    stop (paste("test rgdx(j,full,unfiltered,uncompressed) failed",chk$msg))
  }

  cblock <- icard + jcard + (1:ccard)
  v <- array(0,c(ucard,1))
  v[cblock] = 1
  te <- array("",c(ucard,1))
  te[cblock] = ctext
  cwant <- list(name="c", type="set", dim=1,
                val=v,
                form="full",
                uels=list(u), te=te)
  c <- rgdx(fnIn,list(name='c',form='full',te=TRUE))
  chk <- chkRgdxRes (c, cwant)
  if (!chk$same) {
    stop (paste("test rgdx(c,full,unfiltered,uncompressed) failed",chk$msg))
  }

  ## it is helpful to put the names with the array
  ## we should consider doing this with the return $val!!
  v <- array(0,c(ucard,ucard),dimnames=list(u,u))
  v['i1','j1'] = 1;
  v['i1','j3'] = 1;
  v['i2','j2'] = 1;
  v['i2','j3'] = 1;
  v['i3','j3'] = 1;
  te <- array("",c(ucard,ucard),dimnames=list(u,u))
  te['i1','j1'] = "one.one";
  te['i1','j3'] = "one.three";
  te['i2','j2'] = "two.two";
  te['i2','j3'] = "two.three";
  te['i3','j3'] = "three.three";
  ijwant <- list(name="IJ", type="set", dim=2,
                 val=v,
                 form="full",
                 uels=list(u,u),
                 ts='',
                 te=te)
  ij <- rgdx(fnIn,list(name='ij',form='full',te=TRUE,ts=TRUE))
  chk <- chkRgdxRes (ij, ijwant)
  if (!chk$same) {
    stop (paste("test rgdx(ij,full,unfiltered,uncompressed) failed",chk$msg))
  }

  v <- array(0,c(ucard,ucard,ucard),dimnames=list(u,u,u))
  v['i1','j1','berlin'] = 1;
  v['i1','j3','berlin'] = 1;
  v['i2','j2','paris' ] = 1;
  v['i2','j3','paris' ] = 1;
  v['i3','j3','vienna'] = 1;
  te <- array("",c(ucard,ucard,ucard),dimnames=list(u,u,u))
  te['i1','j1','berlin'] = "eins eins tempelhof";
  te['i1','j3','berlin'] = "eins drei tempelhof";
  te['i2','j2','paris' ] = "deux deux orly";
  te['i2','j3','paris' ] = "deux trois orly";
  te['i3','j3','vienna'] = "drei drei schwechat";
  ijcwant <- list(name="IJc", type="set", dim=3,
                  val=v,
                  form="full",
                  uels=list(u,u,u),
                  te=te)
  ijc <- rgdx(fnIn,list(name='ijc',form='full',te=TRUE))
  chk <- chkRgdxRes (ijc, ijcwant)
  if (!chk$same) {
    stop (paste("test rgdx(ijc,full,unfiltered,uncompressed) failed",chk$msg))
  }

  ## ---------- reading form=full, no filter, compress=TRUE
  ## still to do

  ## ---------- reading form=full, filtered, no compress
  ## still to do

  print ("test of rgdx set text handling passed")
  TRUE   ## all tests passed: return TRUE
},

error = function(ex) { print ("test of rgdx set text handling failed"); print(ex) ; FALSE }
)
