### tGAMS.gms
# test the operation of the gams() function

fnIn <- "eurodist.gdx"
fnOut <- "outTmp.gdx"
if (! file_test ('-f', fnIn)) {
  stop (paste("FAIL: File", fnIn, "does not exist"))
}
suppressWarnings (file.remove(fnOut))

gams (paste("tourLen.gms --gdxIn=",fnIn," --gdxOut=",fnOut,sep=""))
if (! file_test ('-f', fnOut)) {
  stop (paste("FAIL: File", fnOut, "was not created by the GAMS run"))
}

tourLen <- rgdx.scalar (fnOut, "tourLen")
tWant <- 29625
if (tourLen != tWant) {
  stop (paste("FAIL: Computed tour length", tourLen, "was not expected: wanted", tWant))
}
