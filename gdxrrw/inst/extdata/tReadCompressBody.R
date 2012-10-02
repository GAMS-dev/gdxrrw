## body of tests for comparing behavior in two rgdx cases:
##   1. compress=TRUE and useDomInfo=FALSE
##   2. compress=TRUE and useDomInfo=TRUE
## We expect identical behavior so we use a common test file

iUels <- c("i1", "i2", "i3", "i4", "i5", "i6", "i7")
iCard <- length(iUels)
iVals <- matrix(c(1:iCard), nrow=iCard, ncol=1)
iOnes <- matrix(1, nrow=iCard, ncol=1)
jUels <- c("j1", "j2", "j3", "j4", "j5")
jCard <- length(jUels)
jVals <- matrix(c(1:jCard), nrow=jCard, ncol=1)
jOnes <- matrix(1, nrow=jCard, ncol=1)
aVals <- matrix(c(1,1, 33.11,
                  2,2, 54.22), nrow=2, ncol=3, byrow=TRUE)
aFull <- matrix(c(33.11, 0,
                      0, 54.22), nrow=2, ncol=2, byrow=TRUE)
aUels <- list(c("i3", "i5"), c("j3", "j4"))
bVals <- matrix(nrow=iCard*jCard, ncol=3)
for (i in 1:iCard) {
  for (j in 1:jCard) {
    k <- (i-1)*jCard + j
    bVals[k,1] <- i
    bVals[k,2] <- j
    bVals[k,3] <- 525
  }
}
bFull <- matrix(525,nrow=iCard, ncol=jCard)
bUels <- list(iUels,jUels)
dVals <- matrix(nrow=iCard,ncol=3)
for (i in 1:iCard) {
  dVals[i,1] <- i
  dVals[i,2] <- 1
  dVals[i,3] <- (10*i + 1) * 1.01
}
dFull <- matrix(nrow=iCard,ncol=1)
for (i in 1:iCard) {
  dFull[i,1] <- (10*i + 1) * 1.01
}
dUels <- list(iUels,c('j1'))
eVals <- matrix(nrow=jCard,ncol=3)
for (j in 1:jCard) {
  eVals[j,1] <- 1
  eVals[j,2] <- j
  eVals[j,3] <- (30 + j) + (10 + j)/100
}
eFull <- matrix(nrow=1,ncol=jCard)
for (j in 1:jCard) {
  eFull[1,j] <- (30 + j) + (10 + j)/100
}
eUels <- list(c('i3'),jUels)

cVals <- matrix(c(1,1.1,
                  2,5.1), nrow=2, ncol=2, byrow=TRUE)
cFull <- matrix(c(1.1, 5.1), nrow=2,ncol=1)
cUels <- list(c("j1", "j5"))


## ---------- reading form=sparse

iwant <- list(name="I", type="set", dim=1,
              val=iVals,
              form="sparse",
              uels=list(iUels),
              domains=c('_compressed'))
i <- rgdx(fnIn,list(name='i',form='sparse',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (i, iwant)
if (!chk$same) {
  stop (paste("test rgdx(i,sparse) failed",chk$msg))
}

jwant <- list(name="J", type="set", dim=1,
              val=jVals,
              form="sparse",
              uels=list(jUels),
              domains=c('_compressed'))
j <- rgdx(fnIn,list(name='j',form='sparse',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (j, jwant)
if (!chk$same) {
  stop (paste("test rgdx(j,sparse) failed",chk$msg))
}

awant <- list(name="A", type="parameter", dim=2,
              val=aVals,
              form="sparse",
              uels=aUels,
              domains=c('_compressed','_compressed'))
a <- rgdx(fnIn,list(name='a',form='sparse',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (a, awant)
if (!chk$same) {
  stop (paste("test rgdx(a,sparse) failed",chk$msg))
}

bwant <- list(name="B", type="parameter", dim=2,
              val=bVals,
              form="sparse",
              uels=bUels,
              domains=c('_compressed','_compressed'))
b <- rgdx(fnIn,list(name='b',form='sparse',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (b, bwant)
if (!chk$same) {
  stop (paste("test rgdx(b,sparse) failed",chk$msg))
}

dwant <- list(name="D", type="parameter", dim=2,
              val=dVals,
              form="sparse",
              uels=dUels,
              domains=c('_compressed','_compressed'))
d <- rgdx(fnIn,list(name='d',form='sparse',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (d, dwant)
if (!chk$same) {
  stop (paste("test rgdx(d,sparse) failed",chk$msg))
}

ewant <- list(name="E", type="parameter", dim=2,
              val=eVals,
              form="sparse",
              uels=eUels,
              domains=c('_compressed','_compressed'))
e <- rgdx(fnIn,list(name='e',form='sparse',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (e, ewant)
if (!chk$same) {
  stop (paste("test rgdx(e,sparse) failed",chk$msg))
}

cwant <- list(name="c", type="parameter", dim=1,
              val=cVals,
              form="sparse",
              uels=cUels,
              domains=c('_compressed'))
c <- rgdx(fnIn,list(name='c',form='sparse',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (c, cwant)
if (!chk$same) {
  stop (paste("test rgdx(c,sparse) failed",chk$msg))
}


## ---------- reading form=full

iwant <- list(name="I", type="set", dim=1,
              val=iOnes,
              form="full",
              uels=list(iUels),
              domains=c('_compressed'))
i <- rgdx(fnIn,list(name='i',form='full',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (i, iwant)
if (!chk$same) {
  stop (paste("test rgdx(i,full) failed",chk$msg))
}

jwant <- list(name="J", type="set", dim=1,
              val=jOnes,
              form="full",
              uels=list(jUels),
              domains=c('_compressed'))
j <- rgdx(fnIn,list(name='j',form='full',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (j, jwant)
if (!chk$same) {
  stop (paste("test rgdx(j,full) failed",chk$msg))
}

awant <- list(name="A", type="parameter", dim=2,
              val=aFull,
              form="full",
              uels=aUels,
              domains=c('_compressed','_compressed'))
a <- rgdx(fnIn,list(name='a',form='full',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (a, awant)
if (!chk$same) {
  stop (paste("test rgdx(a,full) failed",chk$msg))
}

bwant <- list(name="B", type="parameter", dim=2,
              val=bFull,
              form="full",
              uels=bUels,
              domains=c('_compressed','_compressed'))
b <- rgdx(fnIn,list(name='b',form='full',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (b, bwant)
if (!chk$same) {
  stop (paste("test rgdx(b,full) failed",chk$msg))
}

dwant <- list(name="D", type="parameter", dim=2,
              val=dFull,
              form="full",
              uels=dUels,
              domains=c('_compressed','_compressed'))
d <- rgdx(fnIn,list(name='d',form='full',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (d, dwant)
if (!chk$same) {
  stop (paste("test rgdx(d,full) failed",chk$msg))
}

ewant <- list(name="E", type="parameter", dim=2,
              val=eFull,
              form="full",
              uels=eUels,
              domains=c('_compressed','_compressed'))
e <- rgdx(fnIn,list(name='e',form='full',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (e, ewant)
if (!chk$same) {
  stop (paste("test rgdx(e,full) failed",chk$msg))
}

cwant <- list(name="c", type="parameter", dim=1,
              val=cFull,
              form="full",
              uels=cUels,
              domains=c('_compressed'))
c <- rgdx(fnIn,list(name='c',form='full',compress=TRUE),useDomInfo=useDomInfo)
chk <- chkRgdxRes (c, cwant)
if (!chk$same) {
  stop (paste("test rgdx(c,full) failed",chk$msg))
}

