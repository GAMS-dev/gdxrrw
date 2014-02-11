## ## test rgdx.XXX wrappers and checking of dataframe names

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

testName <- 'valid variable names in data frame'

errFunc <- function(ex) {
  print (paste0("test of rgdx on ",testName,": FAILED"))
  print(ex)
  FALSE
} # errFunc

tryCatch({
  print (paste("testing rgdx on", testName))
  rgdx('?')
  fnIn <- "checkNames.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }

  nm1  <- c('aa bb','s')
  nm1x <- c('aa.bb','s')

  # reserved words not allowed
  nm2  <- c('curr','next' ,'function')
  nm2x <- c('curr','next.','function.')

  # duplicates not allowed
  nm3  <- c('xxx','xxx')
  nm3x <- c('xxx','xxx.1')

  # not allowed: start with digit, any ?, start with .digit
  nm4  <- c('111' ,'???' ,'.1234')
  nm4x <- c('X111','X...','X.1234')

  # duplicates allowed if check.names=F
  nm5  <- c('xxx','xxx','xxx')

  # create desired data frame
  f1 <- factor(c("s1","s1","s2","s2"))
  f2 <- factor(c("s1","s2","s1","s2"))
  ssdf <- data.frame(list("var1"=f1,"var2"=f2))
  attr(ssdf,"symName") <- "ss"
  attr(ssdf,"domains") <- c("s","s")
  ccdf <- data.frame(list("var1"=f1,"var2"=f2,"var3"=c(4,4,4,4)))
  attr(ccdf,"symName") <- "c"
  attr(ccdf,"domains") <- c("s","s")

  ## test with default check.names: should be TRUE
  ss <- rgdx.set ('checkNames.gdx','ss',names=nm1)
  if (!identical(nm1x,names(ss))) {
    stop ("test rgdx.set with default check.names failed: bad names")
  }
  ssx <- ssdf
  names(ssx) <- nm1x
  if (!identical(ssx,ss)) {
    stop ("test rgdx.set with default check.names failed: bad ss")
  }
  cc <- rgdx.param ('checkNames.gdx','c',names=nm2)
  if (!identical(nm2x,names(cc))) {
    stop ("test rgdx.param with default check.names failed: bad names")
  }
  ccx <- ccdf
  names(ccx) <- nm2x
  if (!identical(ccx,cc)) {
    stop ("test rgdx.param with default check.names failed: bad cc")
  }

  ## test with check.names=TRUE
  ss <- rgdx.set ('checkNames.gdx','ss',names=nm3,check.names=TRUE)
  if (!identical(nm3x,names(ss))) {
    stop ("test rgdx.set with check.names=TRUE failed: bad names")
  }
  ssx <- ssdf
  names(ssx) <- nm3x
  if (!identical(ssx,ss)) {
    stop ("test rgdx.set with check.names=TRUE failed: bad ss")
  }
  cc <- rgdx.param ('checkNames.gdx','c',names=nm4,check.names=TRUE)
  if (!identical(nm4x,names(cc))) {
    stop ("test rgdx.param with check.names=TRUE failed: bad names")
  }
  ccx <- ccdf
  names(ccx) <- nm4x
  if (!identical(ccx,cc)) {
    stop ("test rgdx.param with check.names=TRUE failed: bad cc")
  }

  ## test with check.names=FALSE
  ss <- rgdx.set ('checkNames.gdx','ss',names=nm1,check.names=FALSE)
  if (!identical(nm1,names(ss))) {
    stop ("test rgdx.set with check.names=FALSE failed: bad names")
  }
  ssx <- ssdf
  names(ssx) <- nm1
  if (!identical(ssx,ss)) {
    stop ("test rgdx.set with check.names=FALSE failed: bad ss")
  }
  cc <- rgdx.param ('checkNames.gdx','c',names=nm2,check.names=FALSE)
  if (!identical(nm2,names(cc))) {
    stop ("test rgdx.param with check.names=FALSE failed: bad names")
  }
  ccx <- ccdf
  names(ccx) <- nm2
  if (!identical(ccx,cc)) {
    stop ("test rgdx.param with check.names=FALSE failed: bad cc")
  }
  ss <- rgdx.set ('checkNames.gdx','ss',names=nm3,check.names=FALSE)
  if (!identical(nm3,names(ss))) {
    stop ("test rgdx.set(...,'ss',names=nm3,check.names=FALSE) failed: bad names")
  }
  ssx <- ssdf
  names(ssx) <- nm3
  if (!identical(ssx,ss)) {
    stop ("test rgdx.set(...,'ss',names=nm3,check.names=FALSE) failed: bad ss")
  }
  cc <- rgdx.param ('checkNames.gdx','c',names=nm5,check.names=FALSE)
  if (!identical(nm5,names(cc))) {
    stop ("test rgdx.param(...,'c',names=nm5,check.names=FALSE) failed: bad names")
  }
  ccx <- ccdf
  names(ccx) <- nm5
  if (!identical(ccx,cc)) {
    stop ("test rgdx.param(...,'c',names=nm5,check.names=FALSE) failed: bad cc")
  }

  print (paste0("test of rgdx on ", testName, ": PASSED"))
  invisible(TRUE)   ## all tests passed: return TRUE
},

error = errFunc
)
