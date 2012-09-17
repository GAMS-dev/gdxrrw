#### test rgdx and reading set text data, i.e. I.te(I) in gams
####

#### wanted lists produced with    dump("listName",file="")

source ("chkSame.R")

setMsg <- function (msg) {
  print (paste("setMsg: input =",msg))
  return (list(same=TRUE,msg=msg))
}                                       # setMsg

tryCatch({
  print ("testing rgdx handling of set text")
  rgdx('?')
  fnIn <- "teTest.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }

  msg <- 'default'
  rr <- setMsg (msg)
  print (paste("post: same =",rr$same))
  print (paste("post: msg  =",rr$msg))

  iwant <- list(name="I",type="set",dim=1,
                val="notDone",form="sparse",
                uels=list(c('jj')),te=c('i1'))
  i <- rgdx(fnIn,list(name='i',form='sparse',te=TRUE))
  if (i$type != "set")
    stop ("Expected i$type to be 'set', got ", i$type)
  if (i$dim != 1)
    stop ("Expected i$dim to be 1, got ", i$dim)
  chk <- chkRgdxRes (i, iwant)
  if (!chk$same) {
    stop (paste("test A failed",chk$msg))
  }

  i <- rgdx(fnIn,list(name='i',form='sparse',te=TRUE,uels=list(c('i1','i2','i3'))))
  j <- rgdx(fnIn,list(name='j',form='sparse',te=TRUE,uels=list(c('j1','j2','j4'))))

  ## this next still not working
  ## i <- rgdx(fnIn,list(name='i',form='sparse',te=TRUE))


  print ("test of rgdx set text handling passed")
  TRUE   ## all tests passed: return TRUE
},

error = function(ex) { print ("test of rgdx set text handling failed"); print(ex) ; FALSE }
)
