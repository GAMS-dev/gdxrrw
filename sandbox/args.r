# how to introduce new syntax for rgdx?

rCurr <- function(gdxName, requestList = NULL, squeeze=TRUE, useDomInfo=TRUE)
{
  if (is.null(requestList) && (gdxName == '?')) {
    invisible(.External(gdxrrw:::rgdxExt, gdxName=gdxName, requestList=NULL,
                        squeeze=squeeze, useDomInfo=useDomInfo))
  }
  else {
    .External(gdxrrw:::rgdxExt, gdxName=gdxName, requestList=requestList,
              squeeze=squeeze, useDomInfo=useDomInfo)
  }
} # rCurr

# rNew: new interface, no attempt at backward compatibility
# possible fields in read specifier list:
#   compress, dim, field, form, name, te, ts, uels
rNew <- function(gdxName,
                 requestList = NULL,
                 symName = NULL,
                 form = 'sparse',
                 uels = NULL,
                 ts = FALSE,
                 te = FALSE,
                 field = NULL,
                 dim = NULL,
                 compress = FALSE,
                 squeeze=TRUE, useDomInfo=TRUE)
{
  if (is.null(symName) && (gdxName == '?')) {
    return(invisible(.External(gdxrrw:::rgdxExt, gdxName=gdxName, requestList=NULL,
                        squeeze=squeeze, useDomInfo=useDomInfo)))
  }
  if (is.null(symName)) {
    return(.External(gdxrrw:::rgdxExt, gdxName=gdxName, requestList=NULL,
                     squeeze=squeeze, useDomInfo=useDomInfo))
  }
  rr <- list()
  rr$name <- symName
  rr$form <- form
  if (! is.null(uels)) {
    rr$uels <- uels
  }
  rr$ts <- ts
  rr$te <- te
  if (! is.null(field)) {
    rr$field <- field
  }
  if (! is.null(dim)) {
    rr$dim <- dim
  }
  if (! is.null(compress)) {
    rr$compress <- compress
  }
  return(.External(gdxrrw:::rgdxExt, gdxName=gdxName, requestList=rr,
                   squeeze=squeeze, useDomInfo=useDomInfo))
} # rNew

r3 <- function(gdxName,
               ...,
#                 requestList = NULL,
#                 symName = NULL,
#                 form = 'sparse',
#                uels = NULL,
#                 ts = FALSE,
#                te = FALSE,
#                field = NULL,
#                dim = NULL,
#                compress = FALSE,
                 squeeze=TRUE, useDomInfo=TRUE)
{
  
}

# r3 ('trns',list(name='hh'))
# r3( 'trns',name='hh')
vv <- list(name='hh',form='sparse')
rNew('trns',someFunc(vv),useDomInfo=FALSE)
>> rNew('trns',name='hh',form='sparse',useDomInfo=FALSE)
  

exercise <- function () {
  cc1 <- rCurr('eurodist.gdx',list(name='cities'))
  cc1
  cc2 <- rNew('eurodist.gdx','cities')
  cc2
  identical(cc1,cc2)
  sub1 <- rCurr('eurodist.gdx',list(name='cities',uels=list(c("athens","hamBurg"))))

} # exercise
