### Test that the wrappers treat options(gdx.domainNames) properly

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

source ("chkSame.R")
reqIdent <- TRUE
testName <- 'wrapper treatment of options(gdx.domainNames)'
logFile <- 'diffLog.txt'

errFunc <- function(ex) {
  print (paste0("test of gdxrrw on ",testName,": FAILED"))
  print (paste("Check file", logFile, "for possible gdxdiff output"))
  print (ex)
  FALSE
} # errFunc

tryCatch({
  print (paste("testing gdxrrw on", testName))
  fnIn <- "tDomainNames.gdx"

  print ("")
  print ("testing options(gdx.domainNames=F)")
  options(gdx.domainNames=F)

  # read all the data, using various combination of options
  sdfwant <- structure(
      list(
          i = structure(1:4, .Label = c("s1", "s2", "s3", "s4", "t1", "t2", "t3"), class = "factor")
      ),
      .Names = "i",
      row.names = c(NA, -4L),
      class = "data.frame",
      symName = "s",
      domains = "*",
      domInfo = "none",
      ts = ""
  )                                     # end structure
  sdf <- rgdx.set(fnIn,'s',ts=TRUE)
  chk <- chkRgdxDF (sdf, sdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("gdx.domainNames=F test rgdx.set(s,ts=TRUE) failed:",chk$msg))
  }
  tdfwant <- structure(
      list(
          i = structure(1:3, .Label = c("t1", "t2", "t3"), class = "factor")
      ),
      .Names = "i",
      row.names = c(NA, -3L),
      class = "data.frame",
      symName = "t",
      domains = "*",
      domInfo = "compressed"
  )                                     # end structure
  tdf <- rgdx.set(fnIn,'t',compress=TRUE)
  chk <- chkRgdxDF (tdf, tdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("gdx.domainNames=F test rgdx.set(t,compress=TRUE) failed:",chk$msg))
  }
  pdfwant <- structure(
      list(
          i = structure(1:4, .Label = c("s1", "s2", "s3", "s4"), class = "factor"),
          j = structure(1:4, .Label = c("s1", "s2", "s3", "s4"), class = "factor")
      ),
      .Names = c("i","j"),
      row.names = c(NA, -4L),
      class = "data.frame",
      symName = "p",
      domains = c("s","ss"),
      domInfo = "full"
  )                                     # end structure
  pdf <- rgdx.set(fnIn,'p')
  chk <- chkRgdxDF (pdf, pdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("gdx.domainNames=F test rgdx.set(p) failed:",chk$msg))
  }
  adfwant <- structure(
      list(
          i = structure(1L, .Label = c("t1", "t2", "t3"), class = "factor"),
          j = structure(4L, .Label = c("s1", "s2", "s3", "s4"), class = "factor"),
          a = 3.14159
      ),
      .Names = c("i","j","value"),
      row.names = c(NA, -1L),
      class = "data.frame",
      symName = "a",
      domains = c("t","s"),
      domInfo = "full"
  )                                     # end structure
  adf <- rgdx.param(fnIn,'a')
  chk <- chkRgdxDF (adf, adfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("gdx.domainNames=F test rgdx.param(a) failed:",chk$msg))
  }
  bdfwant <- structure(
      list(
          i = structure(1L, .Label = c("s2"), class = "factor"),
          j = structure(1L, .Label = c("s2"), class = "factor"),
          value = 1.5
      ),
      .Names = c("i","j","value"),
      row.names = c(NA, -1L),
      class = "data.frame",
      symName = "b",
      domains = c("ss","u"),
      domInfo = "compressed"
  )                                     # end structure
  bdf <- rgdx.param(fnIn,'b',compress=T)
  chk <- chkRgdxDF (bdf, bdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("gdx.domainNames=F test rgdx.param(b,compress=T) failed:",chk$msg))
  }


  print ("")
  print ("testing options(gdx.domainNames=T)")
  options(gdx.domainNames=T)
  sdf <- rgdx.set(fnIn,'s',ts=TRUE)
  chk <- chkRgdxDF (sdf, sdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("gdx.domainNames=T test rgdx.set(s,ts=TRUE) failed:",chk$msg))
  }
  names(tdfwant) <- ".i"
  tdf <- rgdx.set(fnIn,'t',compress=TRUE)
  chk <- chkRgdxDF (tdf, tdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("gdx.domainNames=T test rgdx.set(t,compress=TRUE) failed:",chk$msg))
  }
  names(pdfwant) <- attr(pdfwant,"domains")
  pdf <- rgdx.set(fnIn,'p')
  chk <- chkRgdxDF (pdf, pdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("gdx.domainNames=T test rgdx.set(p) failed:",chk$msg))
  }
  names(adfwant) <- c(attr(adfwant,"domains"),attr(adfwant,"symName"))
  adf <- rgdx.param(fnIn,'a')
  chk <- chkRgdxDF (adf, adfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("gdx.domainNames=T test rgdx.param(a) failed:",chk$msg))
  }
  names(bdfwant) <- c(attr(bdfwant,"domains"),attr(bdfwant,"symName"))
  bdf <- rgdx.param(fnIn,'b',compress=T)
  chk <- chkRgdxDF (bdf, bdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("gdx.domainNames=T test rgdx.param(b,compress=T) failed:",chk$msg))
  }


  print ("")
  print ("testing options(gdx.domainNames=NULL)")
  options(gdx.domainNames=NULL)         # this should default to TRUE
  sdf <- rgdx.set(fnIn,'s',ts=TRUE)
  chk <- chkRgdxDF (sdf, sdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("gdx.domainNames=NULL test rgdx.set(s,ts=TRUE) failed:",chk$msg))
  }
  tdf <- rgdx.set(fnIn,'t',compress=TRUE)
  chk <- chkRgdxDF (tdf, tdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("gdx.domainNames=NULL test rgdx.set(t,compress=TRUE) failed:",chk$msg))
  }
  pdf <- rgdx.set(fnIn,'p')
  chk <- chkRgdxDF (pdf, pdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("gdx.domainNames=NULL test rgdx.set(p) failed:",chk$msg))
  }
  adf <- rgdx.param(fnIn,'a')
  chk <- chkRgdxDF (adf, adfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("gdx.domainNames=NULL test rgdx.param(a) failed:",chk$msg))
  }
  bdf <- rgdx.param(fnIn,'b',compress=T)
  chk <- chkRgdxDF (bdf, bdfwant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("gdx.domainNames=NULL test rgdx.param(b,compress=T) failed:",chk$msg))
  }


  print (paste0("test of gdxrrw on ",testName,": PASSED"))
  suppressWarnings(file.remove(logFile))
  invisible(TRUE)   ## all tests passed: return TRUE
},

error = errFunc
)
