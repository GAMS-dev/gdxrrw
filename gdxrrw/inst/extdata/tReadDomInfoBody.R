## body of tests for comparing behavior in two rgdx cases:
##   1. GDX file written prior to domain information
##   2. GDX file with domain information and useDomInfo=FALSE
## We expect identical behavior so we use a common test file

iUels <- c("i1", "i2", "i3", "i4")
iCard <- length(iUels)
iText <- iUels                        # since no text is in GDX
iVals <- matrix(as.double(1:4), nrow=4, ncol=1)
i2uels <- iUels[1:3]                  # uels from i that appear in ij
i2card <- length(i2uels)
ifUels <- iUels[2:4]                  # uels from i to use in filtered read
ifCard <- length(ifUels)
ifText <- iText[2:4]
jUels <- c("j1", "j2", "j3")
jCard <- length(jUels)
jText <- c("j1 text", "j2 text", "j3 text")
jVals <- matrix(as.double(5:7), nrow=3, ncol=1)
jfUels <- jUels[1:3]                  # uels from j to use in filtered read
jfCard <- length(jfUels)
jfText <- jText[1:3]
cUels <- c("berlin", "paris", "vienna")
cCard <- length(cUels)
cText <- c("city of airport delays", "city of light", "city of dreams")
cVals <- matrix(as.double(8:10), nrow=3, ncol=1)
u <- c(iUels, jUels, cUels)
uCard <- length(u)

## ---------- reading form=sparse, no filter, no compress

iwant <- list(name="I", type="set", dim=1L,
              val=iVals,
              form="sparse",
              uels=list(u), domains=c("*"), te=iText)
i <- rgdx(fnIn,list(name='i',form='sparse',te=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (i, iwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(i,unfiltered,uncompressed) failed",chk$msg))
}

jwant <- list(name="J", type="set", dim=1L,
              val=jVals,
              form="sparse",
              uels=list(u), domains=c("*"),
              te=jText)
j <- rgdx(fnIn,list(name='j',form='sparse',te=TRUE),useDomInfo=useDomInfo)
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
c <- rgdx(fnIn,list(name='c',form='sparse',te=TRUE,ts=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (c, cwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(c,unfiltered,uncompressed) failed",chk$msg))
}

ijwant <- list(name="IJ", type="set", dim=2L,
               val=matrix(c(1,5, 1,7, 2,6, 2,7, 3,7), nrow=5, ncol=2, byrow=TRUE),
               form="sparse",
               uels=list(u,u), domains=c("*","*"),
               ts='',
               te=c("one.one", "one.three", "two.two", "two.three", "three.three"))
ij <- rgdx(fnIn,list(name='ij',form='sparse',te=TRUE,ts=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (ij, ijwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(ij,unfiltered,uncompressed) failed",chk$msg))
}

ijcwant <- list(name="IJc", type="set", dim=3L,
                val=matrix(c(1,5,8, 1,7,8, 2,6,9, 2,7,9, 3,7,10),
                           nrow=5, ncol=3, byrow=TRUE),
                form="sparse",
                uels=list(u,u,u), domains=c("*","*","*"),
                te=c("eins eins tempelhof", "eins drei tempelhof", "deux deux orly",
                  "deux trois orly", "drei drei schwechat"))
ijc <- rgdx(fnIn,list(name='ijc',form='sparse',te=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (ijc, ijcwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(ijc,unfiltered,uncompressed) failed",chk$msg))
}

aicwant <- list(name="AIc", type="parameter", dim=2L,
                val=matrix(c(1,8,11, 1,9,12, 2,9,22, 2,10,23, 3,10,33),
                           nrow=5, ncol=3, byrow=TRUE),
                form="sparse",
                uels=list(u,u), domains=c("*","*") )
aic <- rgdx(fnIn,list(name='aic',form='sparse'),useDomInfo=useDomInfo)
chk <- chkRgdxRes (aic, aicwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(aic,unfiltered,uncompressed) failed",chk$msg))
}

aijcwant <- list(name="AIJc", type="parameter", dim=3L,
                 val=matrix(c(1,5,8 ,111,
                              1,7,8 ,131,
                              2,6,9 ,222,
                              2,7,9 ,232,
                              3,7,10,333),
                   nrow=5, ncol=4, byrow=TRUE),
                form="sparse",
                uels=list(u,u,u), domains=c("*","*","*")
                )
aijc <- rgdx(fnIn,list(name='aijc',form='sparse'),useDomInfo=useDomInfo)
chk <- chkRgdxRes (aijc, aijcwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(aijc,unfiltered,uncompressed) failed",chk$msg))
}

## ---------- reading form=sparse, no filter, compress=TRUE

iwant <- list(name="I", type="set", dim=1L,
              val=iVals,
              form="sparse",
              uels=list(iUels), domains=c("_compressed"), te=iText)
i <- rgdx(fnIn,list(name='i',form='sparse',te=TRUE,compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (i, iwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(i,unfiltered,compressed) failed",chk$msg))
}

jwant <- list(name="J", type="set", dim=1L,
              val=jVals-iCard,
              form="sparse",
              uels=list(jUels), domains=c("_compressed"),
              te=jText)
j <- rgdx(fnIn,list(name='j',form='sparse',te=TRUE,compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (j, jwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(j,unfiltered,compressed) failed",chk$msg))
}

ijwant <- list(name="IJ", type="set", dim=2L,
               val=matrix(c(1,1, 1,3, 2,2, 2,3, 3,3), nrow=5, ncol=2, byrow=TRUE),
               form="sparse",
               uels=list(i2uels,jUels), # i4 is compressed out
               domains=c("_compressed","_compressed"),
               ts='',
               te=c("one.one", "one.three", "two.two", "two.three", "three.three"))
ij <- rgdx(fnIn,list(name='ij',form='sparse',te=TRUE,ts=TRUE,compress=TRUE),useDomInfo=useDomInfo)
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
ijc <- rgdx(fnIn,list(name='ijc',form='sparse',te=TRUE,compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (ijc, ijcwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(ijc,unfiltered,compressed) failed",chk$msg))
}

aicwant <- list(name="AIc", type="parameter", dim=2L,
                val=matrix(c(1,1,11,
                             1,2,12,
                             2,2,22,
                             2,3,23,
                             3,3,33),
                  nrow=5, ncol=3, byrow=TRUE),
                form="sparse",
                uels=list(iUels[1:3],cUels),
                domains=c("_compressed","_compressed") )

aic <- rgdx(fnIn,list(name='aic',form='sparse',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (aic, aicwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(aic,unfiltered,compressed) failed",chk$msg))
}

aijcwant <- list(name="AIJc", type="parameter", dim=3L,
                 val=matrix(c(1,1,1,111,
                              1,3,1,131,
                              2,2,2,222,
                              2,3,2,232,
                              3,3,3,333),
                  nrow=5, ncol=4, byrow=TRUE),
                form="sparse",
                uels=list(iUels[1:3],jUels,cUels),
                domains=c("_compressed","_compressed","_compressed") )
aijc <- rgdx(fnIn,list(name='aijc',form='sparse',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (aijc, aijcwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(aijc,unfiltered,compressed) failed",chk$msg))
}

## ---------- reading form=sparse, filtered, no compress

iwant <- list(name="I", type="set", dim=1L,
              val=matrix(c(1:3), nrow=3, ncol=1),
              form="sparse",
              uels=list(ifUels), domains=c("_user"), te=ifText)
i <- rgdx(fnIn,list(name='i',form='sparse',uels=list(ifUels),te=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (i, iwant)
if (!chk$same) {
  stop (paste("test rgdx(i,filtered,uncompressed) failed",chk$msg))
}

jwant <- list(name="J", type="set", dim=1L,
              val=jVals-iCard,
              form="sparse",
              uels=list(jfUels),
              domains=c("_user"),
              te=jfText)
j <- rgdx(fnIn,list(name='j',form='sparse',uels=list(jfUels),te=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (j, jwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(j,filtered,uncompressed) failed",chk$msg))
}

cwant <- list(name="c", type="set", dim=1L,
              val=cVals,
              form="sparse",
              uels=list(u),
              domains=c("_user"),
              ts='cities',
              te=cText)
c <- rgdx(fnIn,list(name='c',form='sparse',uels=list(u),te=TRUE,ts=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (c, cwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(c,filtered,uncompressed) failed",chk$msg))
}

ijwant <- list(name="IJ", type="set", dim=2L,
               val=matrix(c(1,2, 1,3, 2,3), nrow=3, ncol=2, byrow=TRUE),
               form="sparse",
               uels=list(ifUels,jfUels),
               domains=c("_user","_user"),
               ts='',
               te=c("two.two", "two.three", "three.three"))
ij <- rgdx(fnIn,list(name='ij',form='sparse',uels=list(ifUels,jfUels),te=TRUE,ts=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (ij, ijwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(ij,filtered,uncompressed) failed",chk$msg))
}

ijcwant <- list(name="IJc", type="set", dim=3L,
                val=matrix(c(1,2,9, 1,3,9, 2,3,10),
                           nrow=3, ncol=3, byrow=TRUE),
                form="sparse",
                uels=list(ifUels,jfUels,u),
                domains=c("_user","_user","_user"),
                te=c("deux deux orly", "deux trois orly", "drei drei schwechat"))
ijc <- rgdx(fnIn,list(name='ijc',form='sparse',uels=list(ifUels,jfUels,u),te=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (ijc, ijcwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(ijc,filtered,uncompressed) failed",chk$msg))
}

aicwant <- list(name="AIc", type="parameter", dim=2L,
                 val=matrix(c(1, 9,22,
                              1,10,23,
                              2,10,33),
                           nrow=3, ncol=3, byrow=TRUE),
                form="sparse",
                uels=list(ifUels,u),
                domains=c("_user","_user") )
aic <- rgdx(fnIn,list(name='aic',form='sparse',uels=list(ifUels,u)),useDomInfo=useDomInfo)
chk <- chkRgdxRes (aic, aicwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(aic,filtered,uncompressed) failed",chk$msg))
}

aijcwant <- list(name="AIJc", type="parameter", dim=3L,
                 val=matrix(c(1,2,9,222,
                              1,3,9,232,
                              2,3,10,333),
                           nrow=3, ncol=4, byrow=TRUE),
                form="sparse",
                uels=list(ifUels,jfUels,u),
                domains=c("_user","_user","_user") )
aijc <- rgdx(fnIn,list(name='aijc',form='sparse',uels=list(ifUels,jfUels,u)),useDomInfo=useDomInfo)
chk <- chkRgdxRes (aijc, aijcwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(aijc,filtered,uncompressed) failed",chk$msg))
}

## ---------- reading form=full, no filter, no compress

v <- array(0,c(uCard,1),dimnames=list(u))
v[(1:iCard)] <- 1
te <- array("",c(uCard,1),dimnames=list(u))
te[(1:iCard)] <- iText
iwant <- list(name="I", type="set", dim=1L,
              val=v,
              form="full",
              uels=list(u), domains=c("*"), te=te)
i <- rgdx(fnIn,list(name='i',form='full',te=TRUE),useDomInfo=useDomInfo)
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
j <- rgdx(fnIn,list(name='j',form='full',te=TRUE),useDomInfo=useDomInfo)
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
c <- rgdx(fnIn,list(name='c',form='full',te=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (c, cwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(c,full,unfiltered,uncompressed) failed",chk$msg))
}

## it is helpful to put the names with the array
## we should consider doing this with the return $val!!
v <- array(0,c(uCard,uCard),dimnames=list(u,u))
v['i1','j1'] <- 1;
v['i1','j3'] <- 1;
v['i2','j2'] <- 1;
v['i2','j3'] <- 1;
v['i3','j3'] <- 1;
te <- array("",c(uCard,uCard),dimnames=list(u,u))
te['i1','j1'] <- "one.one";
te['i1','j3'] <- "one.three";
te['i2','j2'] <- "two.two";
te['i2','j3'] <- "two.three";
te['i3','j3'] <- "three.three";
ijwant <- list(name="IJ", type="set", dim=2L,
               val=v,
               form="full",
               uels=list(u,u), domains=c("*","*"),
               ts='',
               te=te)
ij <- rgdx(fnIn,list(name='ij',form='full',te=TRUE,ts=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (ij, ijwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(ij,full,unfiltered,uncompressed) failed",chk$msg))
}

v <- array(0,c(uCard,uCard,uCard),dimnames=list(u,u,u))
v['i1','j1','berlin'] <- 1;
v['i1','j3','berlin'] <- 1;
v['i2','j2','paris' ] <- 1;
v['i2','j3','paris' ] <- 1;
v['i3','j3','vienna'] <- 1;
te <- array("",c(uCard,uCard,uCard),dimnames=list(u,u,u))
te['i1','j1','berlin'] <- "eins eins tempelhof";
te['i1','j3','berlin'] <- "eins drei tempelhof";
te['i2','j2','paris' ] <- "deux deux orly";
te['i2','j3','paris' ] <- "deux trois orly";
te['i3','j3','vienna'] <- "drei drei schwechat";
ijcwant <- list(name="IJc", type="set", dim=3L,
                val=v,
                form="full",
                uels=list(u,u,u), domains=c("*","*","*"),
                te=te)
ijc <- rgdx(fnIn,list(name='ijc',form='full',te=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (ijc, ijcwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(ijc,full,unfiltered,uncompressed) failed",chk$msg))
}

v <- array(0,c(uCard,uCard),dimnames=list(u,u))
v['i1','berlin'] <- 11;
v['i1','paris' ] <- 12;
v['i2','paris' ] <- 22;
v['i2','vienna'] <- 23;
v['i3','vienna'] <- 33;
aicwant <- list(name="AIc", type="parameter", dim=2L,
                val=v,
                form="full",
                uels=list(u,u), domains=c("*","*"))
aic <- rgdx(fnIn,list(name='aic',form='full'),useDomInfo=useDomInfo)
chk <- chkRgdxRes (aic, aicwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(aic,full,unfiltered,uncompressed) failed",chk$msg))
}

v <- array(0,c(uCard,uCard,uCard),dimnames=list(u,u,u))
v['i1','j1','berlin'] <- 111;
v['i1','j3','berlin'] <- 131;
v['i2','j2','paris' ] <- 222;
v['i2','j3','paris' ] <- 232;
v['i3','j3','vienna'] <- 333;
aijcwant <- list(name="AIJc", type="parameter", dim=3L,
                val=v,
                form="full",
                uels=list(u,u,u), domains=c("*","*","*"))
aijc <- rgdx(fnIn,list(name='aijc',form='full'),useDomInfo=useDomInfo)
chk <- chkRgdxRes (aijc, aijcwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(aijc,full,unfiltered,uncompressed) failed",chk$msg))
}

## ---------- reading form=full, no filter, compress=TRUE

v <- array(1,c(iCard,1),dimnames=list(iUels))
te <- array("",c(iCard,1),dimnames=list(iUels))
te[(1:iCard)] <- iText
iwant <- list(name="I", type="set", dim=1L,
              val=v,
              form="full",
              uels=list(iUels), domains=c("_compressed"), te=te)
i <- rgdx(fnIn,list(name='i',form='full',te=TRUE,compress=TRUE),useDomInfo=useDomInfo)
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
j <- rgdx(fnIn,list(name='j',form='full',te=TRUE,compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (j, jwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(j,full,unfiltered,compressed) failed",chk$msg))
}

v <- array(1,c(cCard,1),dimnames=list(cUels))
te <- array("",c(cCard,1),dimnames=list(cUels))
te[(1:cCard)] <- cText
cwant <- list(name="c", type="set", dim=1L,
              val=v,
              form="full",
              uels=list(cUels), domains=c("_compressed"),
              te=te)
c <- rgdx(fnIn,list(name='c',form='full',te=TRUE,compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (c, cwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(c,full,unfiltered,compressed) failed",chk$msg))
}

v <- array(0,c(i2card,jCard),dimnames=list(i2uels,jUels))
v['i1','j1'] <- 1;
v['i1','j3'] <- 1;
v['i2','j2'] <- 1;
v['i2','j3'] <- 1;
v['i3','j3'] <- 1;
te <- array("",c(i2card,jCard),dimnames=list(i2uels,jUels))
te['i1','j1'] <- "one.one";
te['i1','j3'] <- "one.three";
te['i2','j2'] <- "two.two";
te['i2','j3'] <- "two.three";
te['i3','j3'] <- "three.three";
ijwant <- list(name="IJ", type="set", dim=2L,
               val=v,
               form="full",
               uels=list(i2uels,jUels),
               domains=c("_compressed","_compressed"),
               ts='',
               te=te)
ij <- rgdx(fnIn,list(name='ij',form='full',te=TRUE,ts=TRUE,compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (ij, ijwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(ij,full,unfiltered,compressed) failed",chk$msg))
}

v <- array(0,c(i2card,jCard,cCard),dimnames=list(i2uels,jUels,cUels))
v['i1','j1','berlin'] <- 1;
v['i1','j3','berlin'] <- 1;
v['i2','j2','paris' ] <- 1;
v['i2','j3','paris' ] <- 1;
v['i3','j3','vienna'] <- 1;
te <- array("",c(i2card,jCard,cCard),dimnames=list(i2uels,jUels,cUels))
te['i1','j1','berlin'] <- "eins eins tempelhof";
te['i1','j3','berlin'] <- "eins drei tempelhof";
te['i2','j2','paris' ] <- "deux deux orly";
te['i2','j3','paris' ] <- "deux trois orly";
te['i3','j3','vienna'] <- "drei drei schwechat";
ijcwant <- list(name="IJc", type="set", dim=3L,
                val=v,
                form="full",
                uels=list(i2uels,jUels,cUels),
                domains=c("_compressed","_compressed","_compressed"),
                te=te)
ijc <- rgdx(fnIn,list(name='ijc',form='full',te=TRUE,compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (ijc, ijcwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(ijc,full,unfiltered,compressed) failed",chk$msg))
}

v <- array(0,c(i2card,cCard),dimnames=list(i2uels,cUels))
v['i1','berlin'] <- 11;
v['i1','paris' ] <- 12;
v['i2','paris' ] <- 22;
v['i2','vienna'] <- 23;
v['i3','vienna'] <- 33;
aicwant <- list(name="AIc", type="parameter", dim=2L,
                val=v,
                form="full",
                uels=list(i2uels,cUels),
                domains=c("_compressed","_compressed"),
                ts='')
aic <- rgdx(fnIn,list(name='aic',form='full',ts=TRUE,compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (aic, aicwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(aic,full,unfiltered,compressed) failed",chk$msg))
}

v <- array(0,c(i2card,jCard,cCard),dimnames=list(i2uels,jUels,cUels))
v['i1','j1','berlin'] <- 111;
v['i1','j3','berlin'] <- 131;
v['i2','j2','paris' ] <- 222;
v['i2','j3','paris' ] <- 232;
v['i3','j3','vienna'] <- 333;
aijcwant <- list(name="AIJc", type="parameter", dim=3L,
                val=v,
                form="full",
                uels=list(i2uels,jUels,cUels),
                domains=c("_compressed","_compressed","_compressed") )
aijc <- rgdx(fnIn,list(name='aijc',form='full',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (aijc, aijcwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(aijc,full,unfiltered,compressed) failed",chk$msg))
}

## ---------- reading form=full, filtered, no compress

v <- array(1,c(ifCard,1),dimnames=list(ifUels))
te <- array("",c(ifCard,1),dimnames=list(ifUels))
te[(1:ifCard)] <- ifText
iwant <- list(name="I", type="set", dim=1L,
              val=v,
              form="full",
              uels=list(ifUels), domains=c("_user"), te=te)
i <- rgdx(fnIn,list(name='i',form='full',uels=list(ifUels),te=TRUE),useDomInfo=useDomInfo)
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
              uels=list(jfUels),
              domains=c("_user"),
              te=te)
j <- rgdx(fnIn,list(name='j',form='full',te=TRUE,uels=list(jfUels)),useDomInfo=useDomInfo)
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
              uels=list(u),
              domains=c("_user"),
              ts='cities',
              te=te)
c <- rgdx(fnIn,list(name='c',form='full',uels=list(u),te=TRUE,ts=TRUE),useDomInfo=useDomInfo)
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
               uels=list(ifUels,jfUels),
               domains=c("_user","_user"),
               ts='',
               te=te)
ij <- rgdx(fnIn,list(name='ij',form='full',te=TRUE,ts=TRUE,uels=list(ifUels,jfUels)),useDomInfo=useDomInfo)
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
                uels=list(ifUels,jfUels,u),
                domains=c("_user","_user","_user"),
                te=te)
ijc <- rgdx(fnIn,list(name='ijc',form='full',te=TRUE,uels=list(ifUels,jfUels,u)),useDomInfo=useDomInfo)
chk <- chkRgdxRes (ijc, ijcwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(ijc,full,filtered,uncompressed) failed",chk$msg))
}

v <- array(0,c(ifCard,uCard),dimnames=list(ifUels,u))
v['i2','paris' ] <- 22;
v['i2','vienna'] <- 23;
v['i3','vienna'] <- 33;
aicwant <- list(name="AIc", type="parameter", dim=2L,
                val=v,
                form="full",
                uels=list(ifUels,u),
                domains=c("_user","_user"),
                ts='')
aic <- rgdx(fnIn,list(name='aic',form='full',ts=TRUE,uels=list(ifUels,u)),useDomInfo=useDomInfo)
chk <- chkRgdxRes (aic, aicwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(aic,full,filtered,uncompressed) failed",chk$msg))
}

v <- array(0,c(ifCard,jfCard,cCard),dimnames=list(ifUels,jfUels,cUels))
v['i2','j2','paris' ] <- 222;
v['i2','j3','paris' ] <- 232;
v['i3','j3','vienna'] <- 333;
aijcwant <- list(name="AIJc", type="parameter", dim=3L,
                val=v,
                form="full",
                uels=list(ifUels,jfUels,cUels),
                domains=c("_user","_user","_user"),
                ts='')
aijc <- rgdx(fnIn,list(name='aijc',form='full',ts=TRUE,uels=list(ifUels,jfUels,cUels)),useDomInfo=useDomInfo)
chk <- chkRgdxRes (aijc, aijcwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(aijc,full,filtered,uncompressed) failed",chk$msg))
}

