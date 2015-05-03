# temporary install a package to test it

binName <- "../gdxrrw_0.5.4_R_x86_64-redhat-linux-gnu.tar.gz"
sysdir <- "/home/sdirkse/leg_alpha/gmstest"

detach("package:gdxrrw",unload=T)

lp <- .libPaths()
print (paste("Before lp:",lp))
sp <- search()
print (paste("Before sp:",sp))

here <- getwd()
print(paste("Current dir =",here))
dir.create("tmplib")
tlib <- paste0(here,"/tmplib")

.libPaths(c(tlib,lp))

lp <- .libPaths()
print (paste("After lp:",lp))
sp <- search()
print (paste("After sp:",sp))

install.packages("../gdxrrw_0.5.4_R_x86_64-redhat-linux-gnu.tar.gz",dependencies=F)
library(gdxrrw)
igdx(sysdir,silent=F)

pp <- path.package("gdxrrw")
pp2 <- paste0(tlib,"/gdxrrw")
if (pp != pp2) {
    stop ("gdxrrw not installed as expected")
}
setwd ("tmplib/gdxrrw/tests")
interact <- F
nErr <- source ("tAll.R")

print (paste("tAll completed: nErr = ",nErr))
