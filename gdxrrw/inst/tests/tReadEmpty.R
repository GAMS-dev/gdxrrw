#### test rgdx reading empty data

#### wanted lists can be produced with    dump("listName",file="")

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

source ("chkSame.R")
reqIdent <- TRUE

testName <- 'empty data'

errFunc <- function(ex) {
  print (paste0("test of rgdx on ",testName,": FAILED"))
  print (ex)
  FALSE
} # errFunc

tryCatch({
  print (paste("testing rgdx on", testName))
  rgdx('?')
  fnIn <- "tReadEmpty.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }

  ### ---------- reading empty set
  esetWant <- structure(
      list(i = structure(integer(0), .Label=character(0), class="factor")),
      .Names="i", row.names=integer(0),
      class="data.frame", symName="bb", domains="*", domInfo="none"
  )
  eset <- rgdx.set(fnIn,'bb')
  chk <- chkRgdxDF (eset, esetWant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set() on empty set bb failed",chk$msg))
  }

  attr(esetWant,'domInfo') <- 'compressed'
  names(esetWant) <- '.i'
  eset <- rgdx.set(fnIn,'bb',compress=T)
  chk <- chkRgdxDF (eset, esetWant, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set(,compress=T) on empty set bb failed",chk$msg))
  }

  ### ---------- reading 3-dim empty set
  es3Want <- structure(
      list(d1 = structure(integer(0), .Label=character(0), class="factor"),
           d2 = structure(integer(0), .Label=character(0), class="factor"),
           d3 = structure(integer(0), .Label=character(0), class="factor")
           ),
      .Names=c("d1","d2","d3"), row.names=integer(0), class="data.frame",
      symName="basket", domains=c("d1","d2","d3"), domInfo="full"
  )
  es3 <- rgdx.set(fnIn,'basket')
  chk <- chkRgdxDF (es3, es3Want, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set() on empty set basket failed",chk$msg))
  }

  attr(es3Want,'domInfo') <- 'compressed'
  # names(es3Want) <- '.i'
  es3 <- rgdx.set(fnIn,'basket',compress=T)
  chk <- chkRgdxDF (es3, es3Want, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.set(,compress=T) on empty set basket failed",chk$msg))
  }

  ### ---------- reading empty 1-d param
  ep1Want <- structure(list(
      d1 = structure(integer(0), .Label=character(0), class="factor"),
      e1 = numeric(0)
    ),
    .Names=c("d1","e1"), row.names=integer(0), class="data.frame",
    symName="e1", domains="d1", domInfo="full"
  )
  ep1 <- rgdx.param(fnIn,'e1')
  chk <- chkRgdxDF (ep1, ep1Want, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.param() on empty param e1 failed",chk$msg))
  }

  attr(ep1Want,'domInfo') <- 'compressed'
  ep1 <- rgdx.param(fnIn,'e1',compress=T)
  chk <- chkRgdxDF (ep1, ep1Want, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.param(,compress=T) on empty param e1 failed",chk$msg))
  }


  ### ---------- reading empty 3-d param
  ep3Want <- structure(list(
      d1 = structure(integer(0), .Label=character(0), class="factor"),
      d2 = structure(integer(0), .Label=character(0), class="factor"),
      d3 = structure(integer(0), .Label=character(0), class="factor"),
      e3 = numeric(0)
    ),
    .Names=c("d1","d2","d3","e3"), row.names=integer(0), class="data.frame",
    symName="e3", domains=c("d1","d2","d3"), domInfo="full"
  )
  ep3 <- rgdx.param(fnIn,'e3')
  chk <- chkRgdxDF (ep3, ep3Want, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.param() on empty param e3 failed",chk$msg))
  }

  attr(ep3Want,'domInfo') <- 'compressed'
  ep3 <- rgdx.param(fnIn,'e3',compress=T)
  chk <- chkRgdxDF (ep3, ep3Want, reqIdent=reqIdent)
  if (!chk$same) {
    stop (paste("test rgdx.param(,compress=T) on empty param e3 failed",chk$msg))
  }


  print (paste("test of rgdx on", testName, "passed"))
  TRUE   ## all tests passed: return TRUE
},

error = errFunc
)
