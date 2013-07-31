## body of tests for comparing behavior in two rgdx cases:
##   1. compress=TRUE and useDomInfo=FALSE
##   2. compress=TRUE and useDomInfo=TRUE
## We expect identical behavior so we use a common test file

iUels <- c("i1", "i2", "i3", "i4", "i5", "i6", "i7")
iCard <- length(iUels)
iVals <- matrix(as.double(c(1:iCard)), nrow=iCard, ncol=1)
iOnes <- matrix(1, nrow=iCard, ncol=1,dimnames=list(iUels))
jUels <- c("j1", "j2", "j3", "j4", "j5")
jCard <- length(jUels)
jVals <- matrix(as.double(c(1:jCard)), nrow=jCard, ncol=1)
jOnes <- matrix(1, nrow=jCard, ncol=1,dimnames=list(jUels))
aVals <- matrix(c(1,1, 33.11,
                  2,2, 54.22), nrow=2, ncol=3, byrow=TRUE)
aUels <- list(c("i3", "i5"), c("j3", "j4"))
aUelsN <- aUels
names(aUelsN) <- c('_compressed','_compressed')
aFull <- matrix(c(33.11, 0,
                      0, 54.22), nrow=2, ncol=2, byrow=TRUE,
                dimnames=aUelsN)
bVals <- matrix(nrow=iCard*jCard, ncol=3)
for (i in 1:iCard) {
  for (j in 1:jCard) {
    k <- (i-1)*jCard + j
    bVals[k,1] <- i
    bVals[k,2] <- j
    bVals[k,3] <- 525
  }
}
bUels <- list(iUels,jUels)
bUelsN <- bUels
names(bUelsN) <- c('_compressed','_compressed')
bFull <- matrix(525,nrow=iCard, ncol=jCard, dimnames=bUelsN)
dVals <- matrix(nrow=iCard,ncol=3)
for (i in 1:iCard) {
  dVals[i,1] <- i
  dVals[i,2] <- 1
  dVals[i,3] <- (10*i + 1) * 1.01
}
dUels <- list(iUels,c('j1'))
dFull <- matrix(nrow=iCard,ncol=1,dimnames=dUels)
for (i in 1:iCard) {
  dFull[i,1] <- (10*i + 1) * 1.01
}
eVals <- matrix(nrow=jCard,ncol=3)
for (j in 1:jCard) {
  eVals[j,1] <- 1
  eVals[j,2] <- j
  eVals[j,3] <- (30 + j) + (10 + j)/100
}
eUels <- list(c('i3'),jUels)
eUelsN <- eUels
names(eUelsN) <- c('_compressed','_compressed')
eFull <- matrix(nrow=1,ncol=jCard,dimnames=eUelsN)
for (j in 1:jCard) {
  eFull[1,j] <- (30 + j) + (10 + j)/100
}

cVals <- matrix(c(1,1.1,
                  2,5.1), nrow=2, ncol=2, byrow=TRUE)
cUels <- list(c("j1", "j5"))
cFull <- matrix(c(1.1, 5.1), nrow=2,ncol=1,dimnames=cUels)


## ---------- reading form=sparse

iwant <- list(name="I", type="set", dim=1L,
              val=iVals,
              form="sparse",
              uels=list(iUels),
              domains=c('_compressed'))
i <- rgdx(fnIn,list(name='i',form='sparse',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (i, iwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(i,sparse) failed",chk$msg))
}

jwant <- list(name="J", type="set", dim=1L,
              val=jVals,
              form="sparse",
              uels=list(jUels),
              domains=c('_compressed'))
j <- rgdx(fnIn,list(name='j',form='sparse',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (j, jwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(j,sparse) failed",chk$msg))
}

awant <- list(name="A", type="parameter", dim=2L,
              val=aVals,
              form="sparse",
              uels=aUels,
              domains=c('_compressed','_compressed'))
a <- rgdx(fnIn,list(name='a',form='sparse',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (a, awant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(a,sparse) failed",chk$msg))
}

bwant <- list(name="B", type="parameter", dim=2L,
              val=bVals,
              form="sparse",
              uels=bUels,
              domains=c('_compressed','_compressed'))
b <- rgdx(fnIn,list(name='b',form='sparse',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (b, bwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(b,sparse) failed",chk$msg))
}

dwant <- list(name="D", type="parameter", dim=2L,
              val=dVals,
              form="sparse",
              uels=dUels,
              domains=c('_compressed','_compressed'))
d <- rgdx(fnIn,list(name='d',form='sparse',compress=TRUE),useDomInfo=useDomInfo)
# floating point values in dVals may be off a bit
chk <- chkRgdxRes (d, dwant, reqIdent=F)
if (!chk$same) {
  stop (paste("test rgdx(d,sparse) failed",chk$msg))
}

ewant <- list(name="E", type="parameter", dim=2L,
              val=eVals,
              form="sparse",
              uels=eUels,
              domains=c('_compressed','_compressed'))
e <- rgdx(fnIn,list(name='e',form='sparse',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (e, ewant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(e,sparse) failed",chk$msg))
}

cwant <- list(name="c", type="parameter", dim=1L,
              val=cVals,
              form="sparse",
              uels=cUels,
              domains=c('_compressed'))
c <- rgdx(fnIn,list(name='c',form='sparse',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (c, cwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(c,sparse) failed",chk$msg))
}


## ---------- reading form=full

iwant <- list(name="I", type="set", dim=1L,
              val=iOnes,
              form="full",
              uels=list(iUels),
              domains=c('_compressed'))
i <- rgdx(fnIn,list(name='i',form='full',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (i, iwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(i,full) failed",chk$msg))
}

jwant <- list(name="J", type="set", dim=1L,
              val=jOnes,
              form="full",
              uels=list(jUels),
              domains=c('_compressed'))
j <- rgdx(fnIn,list(name='j',form='full',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (j, jwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(j,full) failed",chk$msg))
}

awant <- list(name="A", type="parameter", dim=2L,
              val=aFull,
              form="full",
              uels=aUelsN,
              domains=c('_compressed','_compressed'))
a <- rgdx(fnIn,list(name='a',form='full',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (a, awant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(a,full) failed",chk$msg))
}

bwant <- list(name="B", type="parameter", dim=2L,
              val=bFull,
              form="full",
              uels=bUelsN,
              domains=c('_compressed','_compressed'))
b <- rgdx(fnIn,list(name='b',form='full',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (b, bwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(b,full) failed",chk$msg))
}

dwant <- list(name="D", type="parameter", dim=2L,
              val=dFull,
              form="full",
              uels=dUels,
              domains=c('_compressed','_compressed'))
d <- rgdx(fnIn,list(name='d',form='full',compress=TRUE),useDomInfo=useDomInfo)
# floating point values in dVals may be off a bit
chk <- chkRgdxRes (d, dwant, reqIdent=F)
if (!chk$same) {
  stop (paste("test rgdx(d,full) failed",chk$msg))
}

ewant <- list(name="E", type="parameter", dim=2L,
              val=eFull,
              form="full",
              uels=eUelsN,
              domains=c('_compressed','_compressed'))
e <- rgdx(fnIn,list(name='e',form='full',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (e, ewant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(e,full) failed",chk$msg))
}

cwant <- list(name="c", type="parameter", dim=1L,
              val=cFull,
              form="full",
              uels=cUels,
              domains=c('_compressed'))
c <- rgdx(fnIn,list(name='c',form='full',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (c, cwant, reqIdent=reqIdent)
if (!chk$same) {
  stop (paste("test rgdx(c,full) failed",chk$msg))
}

