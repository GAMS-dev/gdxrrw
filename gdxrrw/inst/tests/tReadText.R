#### test rgdx and reading set text data, i.e. I.te(I) in gams
#### test form=['sparse','full'] X [filtered,unfiltered] X compress=[T,F]

#### wanted lists produced with    dump("listName",file="")

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

source ("chkSame.R")
reqIdent <- TRUE

testName <- 'associated text (.te) for sets'

errFunc <- function(ex) {
  print (paste0("test of rgdx on ",testName,": FAILED"))
  print(ex)
  FALSE
} # errFunc

tryCatch({
  print (paste("testing rgdx on", testName))
  rgdx('?')
  fnIn <- "teTest.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }

  ## start test assuming the default behavior for inventing names
  options(gdx.inventSetText=NULL)

  iUels <- c("i1", "i2", "i3", "i4")
  iCard <- length(iUels)
  iText <- iUels                        # since no text is in GDX
  iTextNA <- as.character(c(NA,NA,NA,NA)) # if invented set text is NA
  iTextBL <- c("","","","")             # if invented set text is FALSE
  iVals <- matrix(c(1,2,3,4), nrow=iCard, ncol=1)
  i2uels <- iUels[1:3]                  # uels from i that appear in ij
  i2card <- length(i2uels)
  ifUels <- iUels[2:4]                  # uels from i to use in filtered read
  ifCard <- length(ifUels)
  ifText <- iText[2:4]
  ifTextNA <- iTextNA[2:4]
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
  udom2 <- c("I","J")
  udom3 <- c("I","J","c")
  i_f_u <- factor(iUels,u)
  j_f_u <- factor(jUels,u)
  c_f_u <- factor(cUels,u)

  ## ---------- reading form=sparse, no filter, no compress

  iwant <- list(name="I", type="set", dim=1L,
                val=iVals,
                form="sparse",
                uels=list(u), domains=c("*"), domInfo="none", te=iTextNA)
  i <- rgdx(fnIn,list(name='i',form='sparse',te=TRUE))
  chk <- chkRgdxRes (i, iwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(i,unfiltered,uncompressed) failed:",chk$msg))
  }
  idfwant <- data.frame(list("i"=i_f_u,".te"=iTextNA),stringsAsFactors=F)
  attr(idfwant,"symName") <- "I"
  attr(idfwant,"domains") <- c("*")
  attr(idfwant,"domInfo") <- "none"
  idf <- rgdx.set(fnIn,'i',te=TRUE)
  chk <- chkRgdxDF (idf, idfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set(i,uncompressed) failed:",chk$msg))
  }

  jwant <- list(name="J", type="set", dim=1L,
                val=jVals,
                form="sparse",
                uels=list(u), domains=c("*"), domInfo="none",
                te=jText)
  j <- rgdx(fnIn,list(name='j',form='sparse',te=TRUE))
  chk <- chkRgdxRes (j, jwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(j,unfiltered,uncompressed) failed:",chk$msg))
  }
  jdfwant <- data.frame(list("i"=j_f_u,".te"=jText),stringsAsFactors=F)
  attr(jdfwant,"symName") <- "J"
  attr(jdfwant,"domains") <- c("*")
  attr(jdfwant,"domInfo") <- "none"
  jdf <- rgdx.set(fnIn,'j',te=TRUE)
  chk <- chkRgdxDF (jdf, jdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set(j,uncompressed) failed:",chk$msg))
  }

  cwant <- list(name="c", type="set", dim=1L,
                val=cVals,
                form="sparse",
                uels=list(u), domains=c("*"), domInfo="none",
                ts='cities',
                te=cText)
  c <- rgdx(fnIn,list(name='c',form='sparse',te=TRUE,ts=TRUE))
  chk <- chkRgdxRes (c, cwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(c,unfiltered,uncompressed) failed:",chk$msg))
  }
  cdfwant <- data.frame(list("i"=c_f_u,".te"=cText),stringsAsFactors=F)
  attr(cdfwant,"symName") <- "c"
  attr(cdfwant,"domains") <- c("*")
  attr(cdfwant,"domInfo") <- "none"
  cdf <- rgdx.set(fnIn,'c',te=TRUE)
  chk <- chkRgdxDF (cdf, cdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set(c,uncompressed) failed:",chk$msg))
  }

  ijwant <- list(name="IJ", type="set", dim=2L,
                 val=matrix(c(1,1,
                              1,3,
                              2,2,
                              2,3,
                              3,3), nrow=5, ncol=2, byrow=TRUE),
                 form="sparse",
                 uels=list(iUels,jUels), domains=c("I","J"), domInfo="full",
                 ts='',
                 te=c("one.one", "one.three", "two.two", "two.three", "three.three"))
  ij <- rgdx(fnIn,list(name='ij',form='sparse',te=TRUE,ts=TRUE))
  chk <- chkRgdxRes (ij, ijwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ij,unfiltered,uncompressed) failed:",chk$msg))
  }
  f1 <- factor(as.integer(ij$val[,1]),seq(to=length(iUels)),labels=iUels)
  f2 <- factor(as.integer(ij$val[,2]),seq(to=length(jUels)),labels=jUels)
  ijdfwant <- data.frame(list("i"=f1,"j"=f2,".te"=ijwant$te),stringsAsFactors=F)
  attr(ijdfwant,"symName") <- "IJ"
  attr(ijdfwant,"domains") <- c("I","J")
  attr(ijdfwant,"domInfo") <- "full"
  ijdf <- rgdx.set(fnIn,'ij',te=TRUE)
  chk <- chkRgdxDF (ijdf, ijdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set(ij,uncompressed) failed:",chk$msg))
  }
  ijdfwantNA <- ijdfwant

  ijcwant <- list(name="IJc", type="set", dim=3L,
                  val=matrix(c(1,1,1,
                               1,3,1,
                               2,2,2,
			                         2,3,2,
			                         3,3,3),
                             nrow=5, ncol=3, byrow=TRUE),
                  form="sparse",
                  uels=list(iUels,jUels,cUels), domains=c("I","J","c"), domInfo="full",
                  te=c("eins eins tempelhof", "eins drei tempelhof", "deux deux orly",
                    "deux trois orly", "drei drei schwechat"))
  ijc <- rgdx(fnIn,list(name='ijc',form='sparse',te=TRUE))
  chk <- chkRgdxRes (ijc, ijcwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ijc,unfiltered,uncompressed) failed:",chk$msg))
  }
  f1 <- factor(as.integer(ijc$val[,1]),seq(to=length(iUels)),labels=iUels)
  f2 <- factor(as.integer(ijc$val[,2]),seq(to=length(jUels)),labels=jUels)
  f3 <- factor(as.integer(ijc$val[,3]),seq(to=length(cUels)),labels=cUels)
  ijcdfwant <- data.frame(list("i"=f1,"j"=f2,"k"=f3,".te"=ijcwant$te),stringsAsFactors=F)
  attr(ijcdfwant,"symName") <- "IJc"
  attr(ijcdfwant,"domains") <- c("I","J","c")
  attr(ijcdfwant,"domInfo") <- "full"
  ijcdf <- rgdx.set(fnIn,'ijc',te=TRUE)
  chk <- chkRgdxDF (ijcdf, ijcdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set(ijc,uncompressed) failed:",chk$msg))
  }
  ijcdfwantNA <- ijcdfwant

  ## ---------- reading form=sparse, no filter, compress=TRUE

  iwant <- list(name="I", type="set", dim=1L,
                val=iVals,
                form="sparse",
                uels=list(iUels), domains=c("*"), domInfo="compressed", te=iTextNA)
  i <- rgdx(fnIn,list(name='i',form='sparse',te=TRUE,compress=TRUE))
  chk <- chkRgdxRes (i, iwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(i,unfiltered,compressed) failed:",chk$msg))
  }
  idfwant <- data.frame(list("i"=factor(iUels),".te"=iTextNA),stringsAsFactors=F)
  attr(idfwant,"symName") <- "I"
  attr(idfwant,"domains") <- c("*")
  attr(idfwant,"domInfo") <- "compressed"
  idf <- rgdx.set(fnIn,'i',te=TRUE,compress=TRUE)
  chk <- chkRgdxDF (idf, idfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set(i,compressed) failed:",chk$msg))
  }

  jwant <- list(name="J", type="set", dim=1L,
                val=jVals-iCard,
                form="sparse",
                uels=list(jUels), domains=c("*"), domInfo="compressed",
                te=jText)
  j <- rgdx(fnIn,list(name='j',form='sparse',te=TRUE,compress=TRUE))
  chk <- chkRgdxRes (j, jwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(j,unfiltered,compressed) failed:",chk$msg))
  }
  jdfwant <- data.frame(list("i"=factor(jUels),".te"=jText),stringsAsFactors=F)
  attr(jdfwant,"symName") <- "J"
  attr(jdfwant,"domains") <- c("*")
  attr(jdfwant,"domInfo") <- "compressed"
  jdf <- rgdx.set(fnIn,'j',te=TRUE,compress=TRUE)
  chk <- chkRgdxDF (jdf, jdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set(j,compressed) failed:",chk$msg))
  }

  ijwant <- list(name="IJ", type="set", dim=2L,
                 val=matrix(c(1,1,
                              1,3,
                              2,2,
                              2,3,
                              3,3), nrow=5, ncol=2, byrow=TRUE),
                 form="sparse",
                 uels=list(i2uels,jUels), # i4 is compressed out
                 domains=c("I","J"), domInfo="compressed",
                 ts='',
                 te=c("one.one", "one.three", "two.two", "two.three", "three.three"))
  ij <- rgdx(fnIn,list(name='ij',form='sparse',te=TRUE,ts=TRUE,compress=TRUE))
  chk <- chkRgdxRes (ij, ijwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ij,unfiltered,compressed) failed:",chk$msg))
  }
  f1 <- factor(as.integer(ij$val[,1]),labels=i2uels)
  f2 <- factor(as.integer(ij$val[,2]),labels=jUels)
  ijdfwant <- data.frame(list("i"=f1,"j"=f2,".te"=ijwant$te),stringsAsFactors=F)
  attr(ijdfwant,"symName") <- "IJ"
  attr(ijdfwant,"domains") <- c("I","J")
  attr(ijdfwant,"domInfo") <- "compressed"
  ijdf <- rgdx.set(fnIn,'ij',te=TRUE,compress=TRUE)
  chk <- chkRgdxDF (ijdf, ijdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set(ij,compressed) failed:",chk$msg))
  }

  ijcwant <- list(name="IJc", type="set", dim=3L,
                  val=matrix(c(1,1,1, 1,3,1, 2,2,2, 2,3,2, 3,3,3),
                             nrow=5, ncol=3, byrow=TRUE),
                  form="sparse",
                  uels=list(iUels[1:3],jUels,cUels),
                  domains=c("I","J","c"), domInfo="compressed",
                  te=c("eins eins tempelhof", "eins drei tempelhof", "deux deux orly",
                    "deux trois orly", "drei drei schwechat"))
  ijc <- rgdx(fnIn,list(name='ijc',form='sparse',te=TRUE,compress=TRUE))
  chk <- chkRgdxRes (ijc, ijcwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ijc,unfiltered,compressed) failed:",chk$msg))
  }
  f1 <- factor(as.integer(ijcwant$val[,1]),labels=iUels[1:3])
  f2 <- factor(as.integer(ijcwant$val[,2]),labels=jUels)
  f3 <- factor(as.integer(ijcwant$val[,3]),labels=cUels)
  ijcdfwant <- data.frame(list("i"=f1,"j"=f2,"k"=f3,".te"=ijcwant$te),stringsAsFactors=F)
  attr(ijcdfwant,"symName") <- "IJc"
  attr(ijcdfwant,"domains") <- c("I","J","c")
  attr(ijcdfwant,"domInfo") <- "compressed"
  ijcdf <- rgdx.set(fnIn,'ijc',te=TRUE,compress=TRUE)
  chk <- chkRgdxDF (ijcdf, ijcdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set(ijc,compressed) failed",chk$msg))
  }

  ## ---------- reading form=sparse, filtered, no compress

  iwant <- list(name="I", type="set", dim=1L,
                val=ifVals,
                form="sparse",
                uels=list(ifUels), domains=c("*"), domInfo="filtered", te=ifTextNA)
  i <- rgdx(fnIn,list(name='i',form='sparse',uels=list(ifUels),te=TRUE))
  chk <- chkRgdxRes (i, iwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(i,filtered,uncompressed) failed:",chk$msg))
  }

  jwant <- list(name="J", type="set", dim=1L,
                val=jVals-iCard,
                form="sparse",
                uels=list(jfUels), domains=c("*"), domInfo="filtered",
                te=jfText)
  j <- rgdx(fnIn,list(name='j',form='sparse',uels=list(jfUels),te=TRUE))
  chk <- chkRgdxRes (j, jwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(j,filtered,uncompressed) failed:",chk$msg))
  }

  cwant <- list(name="c", type="set", dim=1L,
                val=cVals,
                form="sparse",
                uels=list(u), domains=c("*"), domInfo="filtered",
                ts='cities',
                te=cText)
  c <- rgdx(fnIn,list(name='c',form='sparse',uels=list(u),te=TRUE,ts=TRUE))
  chk <- chkRgdxRes (c, cwant, reqIdent=F)
  if (!chk$same) {
    stop (paste("test rgdx(c,filtered,uncompressed) failed:",chk$msg))
  }

  ijwant <- list(name="IJ", type="set", dim=2L,
                 val=matrix(c(1,2, 1,3, 2,3), nrow=3, ncol=2, byrow=TRUE),
                 form="sparse",
                 uels=list(ifUels,jfUels), domains=c("I","J"), domInfo="filtered",
                 ts='',
                 te=c("two.two", "two.three", "three.three"))
  ij <- rgdx(fnIn,list(name='ij',form='sparse',uels=list(ifUels,jfUels),te=TRUE,ts=TRUE))
  chk <- chkRgdxRes (ij, ijwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ij,filtered,uncompressed) failed:",chk$msg))
  }

  ijcwant <- list(name="IJc", type="set", dim=3L,
                  val=matrix(c(1,2,9, 1,3,9, 2,3,10),
                             nrow=3, ncol=3, byrow=TRUE),
                  form="sparse",
                  uels=list(ifUels,jfUels,u), domains=c("I","J","c"), domInfo="filtered",
                  te=c("deux deux orly", "deux trois orly", "drei drei schwechat"))
  ijc <- rgdx(fnIn,list(name='ijc',form='sparse',uels=list(ifUels,jfUels,u),te=TRUE))
  chk <- chkRgdxRes (ijc, ijcwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ijc,filtered,uncompressed) failed:",chk$msg))
  }

  ## ---------- reading form=full, no filter, no compress
  options(gdx.inventSetText=NA)

  ulst <- list('*'=u)
  v <- array(0,c(uCard,1),dimnames=ulst)
  v[(1:iCard)] <- 1
  te <- array("",c(uCard,1),dimnames=ulst)
  te[(1:iCard)] <- iTextNA
  iwant <- list(name="I", type="set", dim=1L,
                val=v,
                form="full",
                uels=ulst,
                domains=c("*"), domInfo="none",
                te=te)
  i <- rgdx(fnIn,list(name='i',form='full',te=TRUE))
  chk <- chkRgdxRes (i, iwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(i,full,unfiltered,uncompressed) failed:",chk$msg))
  }
  idfwant <- data.frame(list("i"=i_f_u,".te"=iTextNA),stringsAsFactors=F)
  attr(idfwant,"symName") <- "I"
  attr(idfwant,"domains") <- c("*")
  attr(idfwant,"domInfo") <- "none"
  idf <- rgdx.set(fnIn,'i',te=TRUE)
  chk <- chkRgdxDF (idf, idfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set(i,uncompressed) inventSetText=NA failed:",chk$msg))
  }

  jblock <- iCard + (1:jCard)
  v <- array(0,c(uCard,1),dimnames=ulst)
  v[jblock] <- 1
  te <- array("",c(uCard,1),dimnames=ulst)
  te[jblock] <- jText
  jwant <- list(name="J", type="set", dim=1L,
                val=v,
                form="full",
                uels=ulst,
                domains=c("*"), domInfo="none", te=te)
  j <- rgdx(fnIn,list(name='j',form='full',te=TRUE))
  chk <- chkRgdxRes (j, jwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(j,full,unfiltered,uncompressed) failed:",chk$msg))
  }
  jdfwant <- data.frame(list("i"=j_f_u,".te"=jText),stringsAsFactors=F)
  attr(jdfwant,"symName") <- "J"
  attr(jdfwant,"domains") <- c("*")
  attr(jdfwant,"domInfo") <- "none"
  jdf <- rgdx.set(fnIn,'j',te=TRUE)
  chk <- chkRgdxDF (jdf, jdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set(j,uncompressed) inventSetText=NA failed:",chk$msg))
  }

  cblock <- iCard + jCard + (1:cCard)
  v <- array(0,c(uCard,1),dimnames=ulst)
  v[cblock] <- 1
  te <- array("",c(uCard,1),dimnames=ulst)
  te[cblock] <- cText
  cwant <- list(name="c", type="set", dim=1L,
                val=v,
                form="full",
                uels=ulst, domains=c("*"),  domInfo="none", te=te)
  c <- rgdx(fnIn,list(name='c',form='full',te=TRUE))
  chk <- chkRgdxRes (c, cwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(c,full,unfiltered,uncompressed) failed:",chk$msg))
  }
  cdfwant <- data.frame(list("i"=c_f_u,".te"=cText),stringsAsFactors=F)
  attr(cdfwant,"symName") <- "c"
  attr(cdfwant,"domains") <- c("*")
  attr(cdfwant,"domInfo") <- "none"
  cdf <- rgdx.set(fnIn,'c',te=TRUE)
  chk <- chkRgdxDF (cdf, cdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set(c,uncompressed) inventSetText=NA failed",chk$msg))
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
                 uels=list('I'=iUels,'J'=jUels), domains=c("I","J"), domInfo="full",
                 ts='',
                 te=te)
  ij <- rgdx(fnIn,list(name='ij',form='full',te=TRUE,ts=TRUE))
  chk <- chkRgdxRes (ij, ijwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ij,full,unfiltered,uncompressed) failed",chk$msg))
  }
  ijdfwant <- ijdfwantNA
  ijdf <- rgdx.set(fnIn,'ij',te=TRUE)
  chk <- chkRgdxDF (ijdf, ijdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set(ij,uncompressed) inventSetText=NA failed:",chk$msg))
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
                  uels=list('I'=iUels,'J'=jUels,'c'=cUels),
                  domains=c("I","J","c"), domInfo="full",
                  te=te)
  ijc <- rgdx(fnIn,list(name='ijc',form='full',te=TRUE))
  chk <- chkRgdxRes (ijc, ijcwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ijc,full,unfiltered,uncompressed) failed:",chk$msg))
  }
  ijcdfwant <- ijcdfwantNA
  ijcdf <- rgdx.set(fnIn,'ijc',te=TRUE)
  chk <- chkRgdxDF (ijcdf, ijcdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set(ijc,uncompressed) inventSetText=NA failed",chk$msg))
  }

  ## ---------- reading form=full, no filter, compress=TRUE
  options(gdx.inventSetText=FALSE)

  ilst <- list('*'=iUels)
  v <- array(1,c(iCard,1),dimnames=ilst)
  te <- array("",c(iCard,1),dimnames=ilst)
  te[(1:iCard)] <- iTextBL
  iwant <- list(name="I", type="set", dim=1L,
                val=v,
                form="full",
                uels=ilst, domains=c("*"), domInfo="compressed", te=te)
  i <- rgdx(fnIn,list(name='i',form='full',te=TRUE,compress=TRUE))
  chk <- chkRgdxRes (i, iwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(i,full,unfiltered,compressed) failed:",chk$msg))
  }
  idfwant <- data.frame(list("i"=i_f_u,".te"=iTextBL),stringsAsFactors=F)
  attr(idfwant,"symName") <- "I"
  attr(idfwant,"domains") <- c("*")
  attr(idfwant,"domInfo") <- "none"
  idf <- rgdx.set(fnIn,'i',te=TRUE)
  chk <- chkRgdxDF (idf, idfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set(i,uncompressed) inventSetText=F failed:",chk$msg))
  }

  jlst <- list('*'=jUels)
  v <- array(1,c(jCard,1),dimnames=jlst)
  te <- array("",c(jCard,1),dimnames=jlst)
  te[(1:jCard)] <- jText
  jwant <- list(name="J", type="set", dim=1L,
                val=v,
                form="full",
                uels=jlst, domains=c("*"), domInfo="compressed",
                te=te)
  j <- rgdx(fnIn,list(name='j',form='full',te=TRUE,compress=TRUE))
  chk <- chkRgdxRes (j, jwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(j,full,unfiltered,compressed) failed:",chk$msg))
  }
  jdfwant <- data.frame(list("i"=j_f_u,".te"=jText),stringsAsFactors=F)
  attr(jdfwant,"symName") <- "J"
  attr(jdfwant,"domains") <- c("*")
  attr(jdfwant,"domInfo") <- "none"
  jdf <- rgdx.set(fnIn,'j',te=TRUE)
  chk <- chkRgdxDF (jdf, jdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set(j,uncompressed) inventSetText=F failed:",chk$msg))
  }

  v <- array(0,c(i2card,jCard),dimnames=list('I'=i2uels,'J'=jUels))
  v['i1','j1'] <- 1;
  v['i1','j3'] <- 1;
  v['i2','j2'] <- 1;
  v['i2','j3'] <- 1;
  v['i3','j3'] <- 1;
  te <- array("",c(i2card,jCard),dimnames=list('I'=i2uels,'J'=jUels))
  te['i1','j1'] <- "one.one";
  te['i1','j3'] <- "one.three";
  te['i2','j2'] <- "two.two";
  te['i2','j3'] <- "two.three";
  te['i3','j3'] <- "three.three";
  ijwant <- list(name="IJ", type="set", dim=2L,
                 val=v,
                 form="full",
                 uels=list('I'=i2uels,'J'=jUels),
                 domains=c("I","J"), domInfo="compressed",
                 ts='',
                 te=te)
  ij <- rgdx(fnIn,list(name='ij',form='full',te=TRUE,ts=TRUE,compress=TRUE))
  chk <- chkRgdxRes (ij, ijwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ij,full,unfiltered,compressed) failed:",chk$msg))
  }
  
  v <- array(0,c(i2card,jCard,cCard),
             dimnames=list('I'=i2uels,'J'=jUels,'c'=cUels))
  v['i1','j1','berlin'] <- 1;
  v['i1','j3','berlin'] <- 1;
  v['i2','j2','paris' ] <- 1;
  v['i2','j3','paris' ] <- 1;
  v['i3','j3','vienna'] <- 1;
  te <- array("",c(i2card,jCard,cCard),
              dimnames=list('I'=i2uels,'J'=jUels,'c'=cUels))
  te['i1','j1','berlin'] <- "eins eins tempelhof";
  te['i1','j3','berlin'] <- "eins drei tempelhof";
  te['i2','j2','paris' ] <- "deux deux orly";
  te['i2','j3','paris' ] <- "deux trois orly";
  te['i3','j3','vienna'] <- "drei drei schwechat";
  ijcwant <- list(name="IJc", type="set", dim=3L,
                  val=v,
                  form="full",
                  uels=list('I'=i2uels,'J'=jUels,'c'=cUels),
                  domains=c("I","J","c"), domInfo="compressed",
                  te=te)
  ijc <- rgdx(fnIn,list(name='ijc',form='full',te=TRUE,compress=TRUE))
  chk <- chkRgdxRes (ijc, ijcwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ijc,full,unfiltered,compressed) failed:",chk$msg))
  }
  ijdfwant <- ijdfwantNA
  ijdf <- rgdx.set(fnIn,'ij',te=TRUE)
  chk <- chkRgdxDF (ijdf, ijdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set(ij,uncompressed) inventSetText=F failed:",chk$msg))
  }

  ## ---------- reading form=full, filtered, no compress
  options(gdx.inventSetText=TRUE)

  ilst <- list('*'=ifUels)
  v <- array(1,c(ifCard,1),dimnames=ilst)
  te <- array("",c(ifCard,1),dimnames=ilst)
  te[(1:ifCard)] <- ifText
  iwant <- list(name="I", type="set", dim=1L,
                val=v,
                form="full",
                uels=ilst, domains=c('*'), domInfo="filtered", te=te)
  i <- rgdx(fnIn,list(name='i',form='full',uels=list(ifUels),te=TRUE))
  chk <- chkRgdxRes (i, iwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(i,full,filtered,uncompressed) failed:",chk$msg))
  }
  idfwant <- data.frame(list("i"=i_f_u,".te"=iText),stringsAsFactors=F)
  attr(idfwant,"symName") <- "I"
  attr(idfwant,"domains") <- c("*")
  attr(idfwant,"domInfo") <- "none"
  idf <- rgdx.set(fnIn,'i',te=TRUE)
  chk <- chkRgdxDF (idf, idfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set(i,uncompressed) inventSetText=T failed:",chk$msg))
  }

  jlst <- list('*'=jfUels)
  v <- array(1,c(jfCard,1),dimnames=jlst)
  te <- array("",c(jfCard,1),dimnames=jlst)
  te[(1:jfCard)] <- jfText
  jwant <- list(name="J", type="set", dim=1L,
                val=v,
                form="full",
                uels=jlst, domains=c('*'), domInfo="filtered",
                te=te)
  j <- rgdx(fnIn,list(name='j',form='full',te=TRUE,uels=list(jfUels)))
  chk <- chkRgdxRes (j, jwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(j,full,filtered,uncompressed) failed:",chk$msg))
  }
  jdfwant <- data.frame(list("i"=j_f_u,".te"=jText),stringsAsFactors=F)
  attr(jdfwant,"symName") <- "J"
  attr(jdfwant,"domains") <- c("*")
  attr(jdfwant,"domInfo") <- "none"
  jdf <- rgdx.set(fnIn,'J',te=TRUE)
  chk <- chkRgdxDF (jdf, jdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set(j,uncompressed) inventSetText=T failed:",chk$msg))
  }

  cblock <- iCard + jCard + (1:cCard)
  ulst <- list('*'=u)
  v <- array(0,c(uCard,1),dimnames=ulst)
  v[cblock] <- 1
  te <- array("",c(uCard,1),dimnames=ulst)
  te[cblock] <- cText
  cwant <- list(name="c", type="set", dim=1L,
                val=v,
                form="full",
                uels=ulst, domains=c("*"), domInfo="filtered",
                ts='cities',
                te=te)
  c <- rgdx(fnIn,list(name='c',form='full',uels=list(u),te=TRUE,ts=TRUE))
  chk <- chkRgdxRes (c, cwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(c,full,filtered,uncompressed) failed:",chk$msg))
  }
  cdfwant <- data.frame(list("i"=c_f_u,".te"=cText),stringsAsFactors=F)
  attr(cdfwant,"symName") <- "c"
  attr(cdfwant,"domains") <- c("*")
  attr(cdfwant,"domInfo") <- "none"
  cdf <- rgdx.set(fnIn,'C',te=TRUE)
  chk <- chkRgdxDF (cdf, cdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set(c,uncompressed) inventSetText=T failed:",chk$msg))
  }

  dnames <- list(ifUels,jfUels) ; names(dnames) <- udom2
  v <- array(0,c(ifCard,jfCard),dimnames=dnames)
  v['i2','j2'] <- 1;
  v['i2','j3'] <- 1;
  v['i3','j3'] <- 1;
  te <- array("",c(ifCard,jfCard),dimnames=dnames)
  te['i2','j2'] <- "two.two";
  te['i2','j3'] <- "two.three";
  te['i3','j3'] <- "three.three";
  ijwant <- list(name="IJ", type="set", dim=2L,
                 val=v,
                 form="full",
                 uels=dnames, domains=udom2, domInfo="filtered",
                 ts='',
                 te=te)
  ij <- rgdx(fnIn,list(name='ij',form='full',te=TRUE,ts=TRUE,uels=list(ifUels,jfUels)))
  chk <- chkRgdxRes (ij, ijwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ij,full,filtered,uncompressed) failed:",chk$msg))
  }

  dnames <- list(ifUels,jfUels,u) ; names(dnames) <- udom3
  v <- array(0,c(ifCard,jfCard,uCard),dimnames=dnames)
  v['i2','j2','paris' ] <- 1;
  v['i2','j3','paris' ] <- 1;
  v['i3','j3','vienna'] <- 1;
  te <- array("",c(ifCard,jfCard,uCard),dimnames=dnames)
  te['i2','j2','paris' ] <- "deux deux orly";
  te['i2','j3','paris' ] <- "deux trois orly";
  te['i3','j3','vienna'] <- "drei drei schwechat";
  ijcwant <- list(name="IJc", type="set", dim=3L,
                  val=v,
                  form="full",
                  uels=dnames, domains=udom3, domInfo="filtered",
                  te=te)
  ijc <- rgdx(fnIn,list(name='ijc',form='full',te=TRUE,uels=list(ifUels,jfUels,u)))
  chk <- chkRgdxRes (ijc, ijcwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx(ijc,full,filtered,uncompressed) failed:",chk$msg))
  }


  print (paste0("test of rgdx on ", testName, ": PASSED"))
  invisible(TRUE)   ## all tests passed: return TRUE
},

error = function(ex) { print ("test of rgdx set text handling failed"); print(ex) ; FALSE }
)
