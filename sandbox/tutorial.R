### R code from vignette source 'tutorial.Rnw'

###################################################
### code chunk number 1: loadPackage
###################################################
library(gdxrrw)
igdx('')
if (! igdx(silent=T)) stop ('Could not load GDX API')


###################################################
### code chunk number 2: genData
###################################################
gms <- system.file('doc','transport.gms',package='gdxrrw', mustWork=T)
isWindows <- ("mingw32" == R.Version()$os)
if (isWindows) gms <- gsub("/","\\",gms,fixed=TRUE)
ingdx <- system.file('doc','inputs.gdx',package='gdxrrw', mustWork=T)
rc <- gams(paste0(gms, " --INPUT=", ingdx))


###################################################
### code chunk number 3: inCase
###################################################
if (! file.exists('outputs.gdx')) {
  ogdx <- system.file('doc','outputs.gdx',package='gdxrrw', mustWork=T)
  file.copy(ogdx,'outputs.gdx')
}


###################################################
### code chunk number 4: origData
###################################################
outgdx <- 'outputs.gdx'
if (! file.exists(outgdx)) stop (paste('File not found:',outgdx))
I <- rgdx.set(outgdx,'I')
J <- rgdx.set(outgdx,'J')
a <- rgdx.param(outgdx,'a')
b <- rgdx.param(outgdx,'b')
c <- rgdx.param(outgdx,'c')


###################################################
### code chunk number 5: identData
###################################################
wgdx.lst('intest',list(I,J,a,b,c))
rc <- system2('gdxdiff', paste('intest.gdx', ingdx), stdout=F)
if (0 != rc) stop ('gdxdiff says intest.gdx and inputs.gdx differ: rc = ',rc)
rc <- gams(paste(gms, '--INPUT intest.gdx --OUTPUT outtest.gdx'))
if (0 != rc) stop ('gams failed: rc = ',rc)
# system2('gams', 'transport.gms --INPUT intest.gdx --OUTPUT outtest.gdx')
zlst <- rgdx('outputs.gdx',list(name='z'))
z <- zlst$val
# we don't need to use the intermediate zlst variable to get the val
zz <- rgdx('outtest.gdx',list(name='z'))$val
if (0.0 != round(z-zz,6)) stop (paste("different objectives!! ", z, zz))


###################################################
### code chunk number 6: doubleData
###################################################
c2 <- c
c2[[3]] <- c[[3]] * 2
wgdx.lst('in2',list(I,J,a,b,c2))
gams(paste(gms,'--INPUT in2.gdx --OUTPUT out2.gdx'))
z2 <- rgdx('out2.gdx',list(name='z'))$val
print(paste('original=', z,'  double=',z2))


###################################################
### code chunk number 7: statesData
###################################################
data(state)
src <- c('California','Washington','New York','Maryland')
dst <- setdiff(state.name,src)
supTotal <- 1001
demTotal <- 1000
srcPop <- state.x77[src,'Population']
srcPopTot <- sum(srcPop)
dstPop <- state.x77[dst,'Population']
dstPopTot <- sum(dstPop)
sup <- (srcPop / srcPopTot) * supTotal
dem <- (dstPop / dstPopTot) * demTotal
x <- state.center$x
names(x) <- state.name
y <- state.center$x
names(y) <- state.name
cost <- matrix(0,nrow=length(src),ncol=length(dst),dimnames=list(src,dst))
for (s in src) {
  for (d in dst) {
    cost[s,d] <- sqrt((x[s]-x[d])^2 + (y[s]-y[d])^2)
  }
}


###################################################
### code chunk number 8: statesGDX
###################################################
ilst <- list(name='I',uels=list(src),ts='supply states')
jlst <- list(name='J',uels=list(dst),ts='demand states')
suplst <- list(name='a',val=as.array(sup),uels=list(src),
               dim=1,form='full',type='parameter',ts='supply limits')
demlst <- list(name='b',val=as.array(dem),uels=list(dst),
               dim=1,form='full',type='parameter',ts='demand quantities')
clst <- list(name='c',val=cost,uels=list(src,dst),
             dim=2,form='full',type='parameter',
             ts='transportation costs')
wgdx.lst('inStates',list(ilst,jlst,suplst,demlst,clst))


###################################################
### code chunk number 9: statesSolve
###################################################
gams(paste(gms,'--INPUT inStates.gdx --OUTPUT outStates.gdx'))
ms <- rgdx.scalar('outStates.gdx','modelStat')
print(paste('Model status:',ms))


###################################################
### code chunk number 10: missingGDX
###################################################
rc <- gams('transport --INPUT notHere')
if (0 == rc) print ("normal gams return") else print ("abnormal gams return")


###################################################
### code chunk number 11: infeas
###################################################
a2 <- a
rows <- a2$i == 'seattle'
a2[rows,2] <- a2[rows,2] - 100
wgdx.lst('inInf',list(I,J,a2,b,c))
gams(paste(gms,'--INPUT inInf.gdx --OUTPUT outInf.gdx'))
ms <- rgdx.scalar('outInf.gdx','modelStat')
if (4 == ms) print ("The model is infeasible")


