print ("test reading special values with form='sparse' and no filter")
rgdx('?')

fn <- "svxls.gdx"

print (paste("first read with defaults: input =",fn))
sv <- rgdx (fn,list(name='sv'))

print (paste("second read with squeeze=FALSE: input =",fn))
sv2 <- rgdx (fn,list(name='sv'),squeeze=FALSE)

print (paste("third read as data frame with default squeeze: input =",fn))
svdf0 <- rgdx.param (fn, 'sv', ts=TRUE)

print (paste("fourth read as data frame with squeeze=FALSE: input =",fn))
svdf1 <- rgdx.param (fn, 'sv', ts=TRUE, squeeze=FALSE)
