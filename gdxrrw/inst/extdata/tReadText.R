#### test rgdx and reading set text data, i.e. I.te(I) in gams
#### test form=['sparse','full'] X [filtered,unfiltered] X compress=[T,F]

#### wanted lists produced with    dump("listName",file="")

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

source ("chkSame.R")
reqIdent <- TRUE

tryCatch({
  print ("testing rgdx handling of set text")
  rgdx('?')
  fnIn <- "teTest.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }
  iUels <- c("i1", "i2", "i3", "i4")
  iCard <- length(iUels)
  iText <- iUels                        # since no text is in GDX
  iVals <- matrix(c(1,2,3,4), nrow=iCard, ncol=1)
  i2uels <- iUels[1:3]                  # uels from i that appear in ij
  i2card <- length(i2uels)
  ifUels <- iUels[2:4]                  # uels from i to use in filtered read
  ifCard <- length(ifUels)
  ifText <- iText[2:4]
  ifVals <- matrix(c(1,2,3), nrow=ifCard, ncol=1)
  jUels <- c("j1", "j2", "j3")
  jCard <- length(jUels)
  jText <- c("j1 text", "j2 text", "j3 text")
  jVals <- matrix(iCard+c(1,2,3), nrow=jCard, ncol=1)
  jfUels <- jUels[1:3]                  # uels from j to use in filtered read
  jfCard <- length(jfUels)
  jfText <- jText[1:3]
  cUels <- c("berlin", "paris", "vienna")
  cCard <- length(cUels)
  cText <- c("city of airport delays", "city of light", "city of dreams")
  cVals <- matrix(c(8,9,10), nrow=cCard, ncol=1)
  u <- c(iUels, jUels, cUels)
  uCard <- length(u)


  ## ---------- reading form=sparse, no filter, no compress

  iwant <- list(name="I", type="set", dim=1L,
                val=iVals,
                form="sparse",
                uels=list(u), domains=c("*"), te=iText)
  i <- rgdx(fnIn,list(name='i',form='sparse',te=TRUE))
  chk <- chkRgdxRes (i, iwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(i,unfiltered,uncompressed) failed",chk$msg))
  }

  jwant <- list(name="J", type="set", dim=1L,
                val=jVals,
                form="sparse",
                uels=list(u), domains=c("*"),
                te=jText)
  j <- rgdx(fnIn,list(name='j',form='sparse',te=TRUE))
  chk <- chkRgdxRes (j, jwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(j,unfiltered,uncompressed) failed",chk$msg))
  }

  cwant <- list(name="c", type="set", dim=1L,
                val=cVals,
                form="sparse",
                uels=list(u), domains=c("*"),
                ts='cities',
                te=cText)
  c <- rgdx(fnIn,list(name='c',form='sparse',te=TRUE,ts=TRUE))
  chk <- chkRgdxRes (c, cwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(c,unfiltered,uncompressed) failed",chk$msg))
  }

  ijwant <- list(name="IJ", type="set", dim=2L,
                 val=matrix(c(1,1,
                              1,3,
                              2,2,
                              2,3,
                              3,3), nrow=5, ncol=2, byrow=TRUE),
                 form="sparse",
                 uels=list(iUels,jUels), domains=c("I","J"),
                 ts='',
                 te=c("one.one", "one.three", "two.two", "two.three", "three.three"))
  ij <- rgdx(fnIn,list(name='ij',form='sparse',te=TRUE,ts=TRUE))
  chk <- chkRgdxRes (ij, ijwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ij,unfiltered,uncompressed) failed",chk$msg))
  }

  ijcwant <- list(name="IJc", type="set", dim=3L,
                  val=matrix(c(1,1,1,
                               1,3,1,
                               2,2,2,
			       2,3,2,
			       3,3,3),
                             nrow=5, ncol=3, byrow=TRUE),
                  form="sparse",
                  uels=list(iUels,jUels,cUels), domains=c("I","J","c"),
                  te=c("eins eins tempelhof", "eins drei tempelhof", "deux deux orly",
                    "deux trois orly", "drei drei schwechat"))
  ijc <- rgdx(fnIn,list(name='ijc',form='sparse',te=TRUE))
  chk <- chkRgdxRes (ijc, ijcwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ijc,unfiltered,uncompressed) failed",chk$msg))
  }

  ## ---------- reading form=sparse, no filter, compress=TRUE

  iwant <- list(name="I", type="set", dim=1L,
                val=iVals,
                form="sparse",
                uels=list(iUels), domains=c("_compressed"), te=iText)
  i <- rgdx(fnIn,list(name='i',form='sparse',te=TRUE,compress=TRUE))
  chk <- chkRgdxRes (i, iwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(i,unfiltered,compressed) failed",chk$msg))
  }

  jwant <- list(name="J", type="set", dim=1L,
                val=jVals-iCard,
                form="sparse",
                uels=list(jUels), domains=c("_compressed"),
                te=jText)
  j <- rgdx(fnIn,list(name='j',form='sparse',te=TRUE,compress=TRUE))
  chk <- chkRgdxRes (j, jwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(j,unfiltered,compressed) failed",chk$msg))
  }

  ijwant <- list(name="IJ", type="set", dim=2L,
                 val=matrix(c(1,1,
                              1,3,
                              2,2,
                              2,3,
                              3,3), nrow=5, ncol=2, byrow=TRUE),
                 form="sparse",
                 uels=list(i2uels,jUels), # i4 is compressed out
                 domains=c("_compressed","_compressed"),
                 ts='',
                 te=c("one.one", "one.three", "two.two", "two.three", "three.three"))
  ij <- rgdx(fnIn,list(name='ij',form='sparse',te=TRUE,ts=TRUE,compress=TRUE))
  chk <- chkRgdxRes (ij, ijwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ij,unfiltered,compressed) failed",chk$msg))
  }

  ijcwant <- list(name="IJc", type="set", dim=3L,
                  val=matrix(c(1,1,1, 1,3,1, 2,2,2, 2,3,2, 3,3,3),
                             nrow=5, ncol=3, byrow=TRUE),
                  form="sparse",
                  uels=list(iUels[1:3],jUels,cUels),
                  domains=c("_compressed","_compressed","_compressed"),
                  te=c("eins eins tempelhof", "eins drei tempelhof", "deux deux orly",
                    "deux trois orly", "drei drei schwechat"))
  ijc <- rgdx(fnIn,list(name='ijc',form='sparse',te=TRUE,compress=TRUE))
  chk <- chkRgdxRes (ijc, ijcwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ijc,unfiltered,compressed) failed",chk$msg))
  }

  ## ---------- reading form=sparse, filtered, no compress

  iwant <- list(name="I", type="set", dim=1L,
                val=ifVals,
                form="sparse",
                uels=list(ifUels), domains=c("_user"), te=ifText)
  i <- rgdx(fnIn,list(name='i',form='sparse',uels=list(ifUels),te=TRUE))
  chk <- chkRgdxRes (i, iwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(i,filtered,uncompressed) failed",chk$msg))
  }

  jwant <- list(name="J", type="set", dim=1L,
                val=jVals-iCard,
                form="sparse",
                uels=list(jfUels), domains=c("_user"),
                te=jfText)
  j <- rgdx(fnIn,list(name='j',form='sparse',uels=list(jfUels),te=TRUE))
  chk <- chkRgdxRes (j, jwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(j,filtered,uncompressed) failed",chk$msg))
  }

  cwant <- list(name="c", type="set", dim=1L,
                val=cVals,
                form="sparse",
                uels=list(u), domains=c("_user"),
                ts='cities',
                te=cText)
  c <- rgdx(fnIn,list(name='c',form='sparse',uels=list(u),te=TRUE,ts=TRUE))
  chk <- chkRgdxRes (c, cwant, reqIdent=F)
  if (!chk$same) {
    stop (paste("test rgdx(c,filtered,uncompressed) failed",chk$msg))
  }

  ijwant <- list(name="IJ", type="set", dim=2L,
                 val=matrix(c(1,2, 1,3, 2,3), nrow=3, ncol=2, byrow=TRUE),
                 form="sparse",
                 uels=list(ifUels,jfUels), domains=c("_user","_user"),
                 ts='',
                 te=c("two.two", "two.three", "three.three"))
  ij <- rgdx(fnIn,list(name='ij',form='sparse',uels=list(ifUels,jfUels),te=TRUE,ts=TRUE))
  chk <- chkRgdxRes (ij, ijwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ij,filtered,uncompressed) failed",chk$msg))
  }

  ijcwant <- list(name="IJc", type="set", dim=3L,
                  val=matrix(c(1,2,9, 1,3,9, 2,3,10),
                             nrow=3, ncol=3, byrow=TRUE),
                  form="sparse",
                  uels=list(ifUels,jfUels,u), domains=c("_user","_user","_user"),
                  te=c("deux deux orly", "deux trois orly", "drei drei schwechat"))
  ijc <- rgdx(fnIn,list(name='ijc',form='sparse',uels=list(ifUels,jfUels,u),te=TRUE))
  chk <- chkRgdxRes (ijc, ijcwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ijc,filtered,uncompressed) failed",chk$msg))
  }

  ## ---------- reading form=full, no filter, no compress

  v <- array(0,c(uCard,1),dimnames=list(u))
  v[(1:iCard)] <- 1
  te <- array("",c(uCard,1),dimnames=list(u))
  te[(1:iCard)] <- iText
  iwant <- list(name="I", type="set", dim=1L,
                val=v,
                form="full",
                uels=list(u), domains=c("*"),
                te=te)
  i <- rgdx(fnIn,list(name='i',form='full',te=TRUE))
  chk <- chkRgdxRes (i, iwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(i,full,unfiltered,uncompressed) failed",chk$msg))
  }

  jblock <- iCard + (1:jCard)
  v <- array(0,c(uCard,1),dimnames=list(u))
  v[jblock] <- 1
  te <- array("",c(uCard,1),dimnames=list(u))
  te[jblock] <- jText
  jwant <- list(name="J", type="set", dim=1L,
                val=v,
                form="full",
                uels=list(u), domains=c("*"), te=te)
  j <- rgdx(fnIn,list(name='j',form='full',te=TRUE))
  chk <- chkRgdxRes (j, jwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(j,full,unfiltered,uncompressed) failed",chk$msg))
  }

  cblock <- iCard + jCard + (1:cCard)
  v <- array(0,c(uCard,1),dimnames=list(u))
  v[cblock] <- 1
  te <- array("",c(uCard,1),dimnames=list(u))
  te[cblock] <- cText
  cwant <- list(name="c", type="set", dim=1L,
                val=v,
                form="full",
                uels=list(u), domains=c("*"), te=te)
  c <- rgdx(fnIn,list(name='c',form='full',te=TRUE))
  chk <- chkRgdxRes (c, cwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(c,full,unfiltered,uncompressed) failed",chk$msg))
  }

  v <- array(0,c(iCard,jCard),dimnames=list('I'=iUels,'J'=jUels))
  v['i1','j1'] <- 1;
  v['i1','j3'] <- 1;
  v['i2','j2'] <- 1;
  v['i2','j3'] <- 1;
  v['i3','j3'] <- 1;
  te <- array("",c(iCard,jCard),dimnames=list('I'=iUels,'J'=jUels))
  te['i1','j1'] <- "one.one";
  te['i1','j3'] <- "one.three";
  te['i2','j2'] <- "two.two";
  te['i2','j3'] <- "two.three";
  te['i3','j3'] <- "three.three";
  ijwant <- list(name="IJ", type="set", dim=2L,
                 val=v,
                 form="full",
                 uels=list('I'=iUels,'J'=jUels), domains=c("I","J"),
                 ts='',
                 te=te)
  ij <- rgdx(fnIn,list(name='ij',form='full',te=TRUE,ts=TRUE))
  chk <- chkRgdxRes (ij, ijwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ij,full,unfiltered,uncompressed) failed",chk$msg))
  }

  v <- array(0,c(iCard,jCard,cCard),dimnames=list('I'=iUels,'J'=jUels,'c'=cUels))
  v['i1','j1','berlin'] <- 1;
  v['i1','j3','berlin'] <- 1;
  v['i2','j2','paris' ] <- 1;
  v['i2','j3','paris' ] <- 1;
  v['i3','j3','vienna'] <- 1;
  te <- array("",c(iCard,jCard,cCard),dimnames=list('I'=iUels,'J'=jUels,'c'=cUels))
  te['i1','j1','berlin'] <- "eins eins tempelhof";
  te['i1','j3','berlin'] <- "eins drei tempelhof";
  te['i2','j2','paris' ] <- "deux deux orly";
  te['i2','j3','paris' ] <- "deux trois orly";
  te['i3','j3','vienna'] <- "drei drei schwechat";
  ijcwant <- list(name="IJc", type="set", dim=3L,
                  val=v,
                  form="full",
                  uels=list('I'=iUels,'J'=jUels,'c'=cUels), domains=c("I","J","c"),
                  te=te)
  ijc <- rgdx(fnIn,list(name='ijc',form='full',te=TRUE))
  chk <- chkRgdxRes (ijc, ijcwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ijc,full,unfiltered,uncompressed) failed",chk$msg))
  }

  ## ---------- reading form=full, no filter, compress=TRUE

  v <- array(1,c(iCard,1),dimnames=list(iUels))
  te <- array("",c(iCard,1),dimnames=list(iUels))
  te[(1:iCard)] <- iText
  iwant <- list(name="I", type="set", dim=1L,
                val=v,
                form="full",
                uels=list(iUels), domains=c("_compressed"), te=te)
  i <- rgdx(fnIn,list(name='i',form='full',te=TRUE,compress=TRUE))
  chk <- chkRgdxRes (i, iwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(i,full,unfiltered,compressed) failed",chk$msg))
  }

  v <- array(1,c(jCard,1),dimnames=list(jUels))
  te <- array("",c(jCard,1),dimnames=list(jUels))
  te[(1:jCard)] <- jText
  jwant <- list(name="J", type="set", dim=1L,
                val=v,
                form="full",
                uels=list(jUels), domains=c("_compressed"),
                te=te)
  j <- rgdx(fnIn,list(name='j',form='full',te=TRUE,compress=TRUE))
  chk <- chkRgdxRes (j, jwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(j,full,unfiltered,compressed) failed",chk$msg))
  }

  v <- array(0,c(i2card,jCard),dimnames=list('_compressed'=i2uels,'_compressed'=jUels))
  v['i1','j1'] <- 1;
  v['i1','j3'] <- 1;
  v['i2','j2'] <- 1;
  v['i2','j3'] <- 1;
  v['i3','j3'] <- 1;
  te <- array("",c(i2card,jCard),dimnames=list('_compressed'=i2uels,'_compressed'=jUels))
  te['i1','j1'] <- "one.one";
  te['i1','j3'] <- "one.three";
  te['i2','j2'] <- "two.two";
  te['i2','j3'] <- "two.three";
  te['i3','j3'] <- "three.three";
  ijwant <- list(name="IJ", type="set", dim=2L,
                 val=v,
                 form="full",
                 uels=list('_compressed'=i2uels,'_compressed'=jUels), domains=c("_compressed","_compressed"),
                 ts='',
                 te=te)
  ij <- rgdx(fnIn,list(name='ij',form='full',te=TRUE,ts=TRUE,compress=TRUE))
  chk <- chkRgdxRes (ij, ijwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ij,full,unfiltered,compressed) failed",chk$msg))
  }

  v <- array(0,c(i2card,jCard,cCard),
             dimnames=list('_compressed'=i2uels,'_compressed'=jUels,'_compressed'=cUels))
  v['i1','j1','berlin'] <- 1;
  v['i1','j3','berlin'] <- 1;
  v['i2','j2','paris' ] <- 1;
  v['i2','j3','paris' ] <- 1;
  v['i3','j3','vienna'] <- 1;
  te <- array("",c(i2card,jCard,cCard),
              dimnames=list('_compressed'=i2uels,'_compressed'=jUels,'_compressed'=cUels))
  te['i1','j1','berlin'] <- "eins eins tempelhof";
  te['i1','j3','berlin'] <- "eins drei tempelhof";
  te['i2','j2','paris' ] <- "deux deux orly";
  te['i2','j3','paris' ] <- "deux trois orly";
  te['i3','j3','vienna'] <- "drei drei schwechat";
  ijcwant <- list(name="IJc", type="set", dim=3L,
                  val=v,
                  form="full",
                  uels=list('_compressed'=i2uels,'_compressed'=jUels,'_compressed'=cUels),
                  domains=c("_compressed","_compressed","_compressed"),
                  te=te)
  ijc <- rgdx(fnIn,list(name='ijc',form='full',te=TRUE,compress=TRUE))
  chk <- chkRgdxRes (ijc, ijcwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ijc,full,unfiltered,uncompressed) failed",chk$msg))
  }

  ## ---------- reading form=full, filtered, no compress

  v <- array(1,c(ifCard,1),dimnames=list(ifUels))
  te <- array("",c(ifCard,1),dimnames=list(ifUels))
  te[(1:ifCard)] <- ifText
  iwant <- list(name="I", type="set", dim=1L,
                val=v,
                form="full",
                uels=list(ifUels), domains=c("_user"), te=te)
  i <- rgdx(fnIn,list(name='i',form='full',uels=list(ifUels),te=TRUE))
  chk <- chkRgdxRes (i, iwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(i,full,filtered,uncompressed) failed",chk$msg))
  }

  v <- array(1,c(jfCard,1),dimnames=list(jfUels))
  te <- array("",c(jfCard,1),dimnames=list(jfUels))
  te[(1:jfCard)] <- jfText
  jwant <- list(name="J", type="set", dim=1L,
                val=v,
                form="full",
                uels=list(jfUels), domains=c("_user"),
                te=te)
  j <- rgdx(fnIn,list(name='j',form='full',te=TRUE,uels=list(jfUels)))
  chk <- chkRgdxRes (j, jwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(j,full,filtered,uncompressed) failed",chk$msg))
  }

  cblock <- iCard + jCard + (1:cCard)
  v <- array(0,c(uCard,1),dimnames=list(u))
  v[cblock] <- 1
  te <- array("",c(uCard,1),dimnames=list(u))
  te[cblock] <- cText
  cwant <- list(name="c", type="set", dim=1L,
                val=v,
                form="full",
                uels=list(u), domains=c("_user"),
                ts='cities',
                te=te)
  c <- rgdx(fnIn,list(name='c',form='full',uels=list(u),te=TRUE,ts=TRUE))
  chk <- chkRgdxRes (c, cwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(c,full,filtered,uncompressed) failed",chk$msg))
  }

  v <- array(0,c(ifCard,jfCard),dimnames=list(ifUels,jfUels))
  v['i2','j2'] <- 1;
  v['i2','j3'] <- 1;
  v['i3','j3'] <- 1;
  te <- array("",c(ifCard,jfCard),dimnames=list(ifUels,jfUels))
  te['i2','j2'] <- "two.two";
  te['i2','j3'] <- "two.three";
  te['i3','j3'] <- "three.three";
  ijwant <- list(name="IJ", type="set", dim=2L,
                 val=v,
                 form="full",
                 uels=list(ifUels,jfUels), domains=c("_user","_user"),
                 ts='',
                 te=te)
  ij <- rgdx(fnIn,list(name='ij',form='full',te=TRUE,ts=TRUE,uels=list(ifUels,jfUels)))
  chk <- chkRgdxRes (ij, ijwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ij,full,filtered,uncompressed) failed",chk$msg))
  }

  v <- array(0,c(ifCard,jfCard,uCard),dimnames=list(ifUels,jfUels,u))
  v['i2','j2','paris' ] <- 1;
  v['i2','j3','paris' ] <- 1;
  v['i3','j3','vienna'] <- 1;
  te <- array("",c(ifCard,jfCard,uCard),dimnames=list(ifUels,jfUels,u))
  te['i2','j2','paris' ] <- "deux deux orly";
  te['i2','j3','paris' ] <- "deux trois orly";
  te['i3','j3','vienna'] <- "drei drei schwechat";
  ijcwant <- list(name="IJc", type="set", dim=3L,
                  val=v,
                  form="full",
                  uels=list(ifUels,jfUels,u), domains=c("_user","_user","_user"),
                  te=te)
  ijc <- rgdx(fnIn,list(name='ijc',form='full',te=TRUE,uels=list(ifUels,jfUels,u)))
  chk <- chkRgdxRes (ijc, ijcwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ijc,full,filtered,uncompressed) failed",chk$msg))
  }


  print ("test of rgdx set text handling passed")
  TRUE   ## all tests passed: return TRUE
},

error = function(ex) { print ("test of rgdx set text handling failed"); print(ex) ; FALSE }
)
