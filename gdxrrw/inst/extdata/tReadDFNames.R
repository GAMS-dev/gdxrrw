## ## test rgdx.XXX wrappers and checking of dataframe names

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

testName <- 'valid variable names in data frame'

errFunc <- function(ex) {
  print (paste("test of rgdx on",testName,"failed"))
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

  ## test with default check.names: should be TRUE
  ss <- rgdx.set ('checkNames.gdx','ss',names=nm1)
  if (!identical(nm1x,names(ss))) {
    stop ("test rgdx.set with default check.names failed")
  }
  cc <- rgdx.param ('checkNames.gdx','c',names=nm2)
  if (!identical(nm2x,names(cc))) {
    stop ("test rgdx.param with default check.names failed")
  }

  ## test with check.names=TRUE
  ss <- rgdx.set ('checkNames.gdx','ss',names=nm3,check.names=TRUE)
  if (!identical(nm3x,names(ss))) {
    stop ("test rgdx.set with check.names=TRUE failed")
  }
  cc <- rgdx.param ('checkNames.gdx','c',names=nm4,check.names=TRUE)
  if (!identical(nm4x,names(cc))) {
    stop ("test rgdx.param with check.names=TRUE failed")
  }

  ## test with check.names=FALSE
  ss <- rgdx.set ('checkNames.gdx','ss',names=nm1,check.names=FALSE)
  if (!identical(nm1,names(ss))) {
    stop ("test rgdx.set with check.names=FALSE failed")
  }
  cc <- rgdx.param ('checkNames.gdx','c',names=nm2,check.names=FALSE)
  if (!identical(nm2,names(cc))) {
    stop ("test rgdx.param with check.names=TRUE failed")
  }

  print (paste("test of rgdx on", testName, "passed"))
  TRUE   ## all tests passed: return TRUE
},

error = errFunc
)
