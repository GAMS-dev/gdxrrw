## illustrate subtle points about empty text when writing set text

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the GDX shared library has not been loaded")

## test with inventSetText at default
options(gdx.inventSetText=NULL)

uels <- c("noSetText", "emptySetText", "blankSetText",  "someSetText")
N <- length(uels)
te <- matrix(NA_character_,nrow=N,ncol=1)
te[2,1] <- ""
te[3,1] <- "   "
te[4,1] <- "hello hello"

df <- data.frame(list("s"=factor(1:N,labels=uels),".te"=te),stringsAsFactors=F)
attr(df,"symName") <- "s"
attr(df,"domains") <- c("*")
attr(df,"ts") <- "set with  of set text"

fn <- "tmpset.gdx"
wgdx.lst(fn, df)

s <- rgdx.set(fn,'s',te=T)

options(gdx.inventSetText=NA)
sNA <- rgdx.set(fn,'s',te=T)

options(gdx.inventSetText=T)
sT <- rgdx.set(fn,'s',te=T)

options(gdx.inventSetText=F)
sF <- rgdx.set(fn,'s',te=T)

s

print ("If gdx.inventSetText is not set, it behaves like gdx.inventSetText=NA")
if (! identical(s,sNA)) {
  stop (paste('gdx.inventSetText=NA not identical to gdx.inventSetText=NULL'))
}

print ("With gdx.inventSetText=NA, we get NA_character_ back when no set text exists:")
print(paste(sNA[,2]))

print ("With gdx.inventSetText=T, we get the UEL(s) back when no set text exists:")
print(paste(sT[,2]))

print ("With gdx.inventSetText=F, we get the empty string back when no set text exists:")
print(paste(sF[,2]))
