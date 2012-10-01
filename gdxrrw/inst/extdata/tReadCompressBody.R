## body of tests for comparing behavior in two rgdx cases:
##   1. compress=TRUE and useDomInfo=FALSE
##   2. compress=TRUE and useDomInfo=TRUE
## We expect identical behavior so we use a common test file

## ---------- reading form=sparse

iVals <- matrix(c(1:7), nrow=7, ncol=1)
iUels <- c("i1", "i2", "i3", "i4", "i5", "i6", "i7")
jVals <- matrix(c(1:5), nrow=5, ncol=1)
jUels <- c("j1", "j2", "j3", "j4", "j5")
aVals <- matrix(c(1,1, 33.11,
                  2,2, 54.22), nrow=2, ncol=3, byrow=TRUE)
aUels <- list(c("i3", "i5"), c("j3", "j4"))

iwant <- list(name="I", type="set", dim=1,
              val=iVals,
              form="sparse",
              uels=list(iUels))
i <- rgdx(fnIn,list(name='i',form='sparse',compress=compress),useDomInfo=useDomInfo)
chk <- chkRgdxRes (i, iwant)
if (!chk$same) {
  stop (paste("test rgdx(i,sparse) failed",chk$msg))
}

jwant <- list(name="J", type="set", dim=1,
              val=jVals,
              form="sparse",
              uels=list(jUels))
j <- rgdx(fnIn,list(name='j',form='sparse',compress=compress),useDomInfo=useDomInfo)
chk <- chkRgdxRes (j, jwant)
if (!chk$same) {
  stop (paste("test rgdx(j,sparse) failed",chk$msg))
}

awant <- list(name="A", type="parameter", dim=2,
              val=aVals,
              form="sparse",
              uels=aUels)
a <- rgdx(fnIn,list(name='a',form='sparse',compress=compress),useDomInfo=useDomInfo)
chk <- chkRgdxRes (a, awant)
if (!chk$same) {
  stop (paste("test rgdx(a,sparse) failed",chk$msg))
}

