c <- rgdx.set('agData','c')
expTotal <- rgdx.param('agData','expTotal')
str(c)
str(expTotal)
# the two str() calls above show an issue with UELs and the universe

fnIn <- "agData.gdx"
r <- rgdx(fnIn,list(name='r',compress=TRUE))
c <- rgdx(fnIn,list(name='c',compress=TRUE))
ruels=r$uels
cuels=c$uels

# this gives you a vector with the mlk index dropped
expTotal <- rgdx(fnIn,list(name='expTotal',compress=TRUE,form='full'))
expTotal$val
str(expTotal)

# this gives you the right data but not in sparse form
expTotal <- rgdx(fnIn,list(name='expTotal',compress=TRUE,form='sparse'))

# this gives us a FILTERED read, i.e. a read that returns a parameter
# with the same size as the filter 
expTotal <- rgdx (fnIn, list(name='expTotal',form='full',uels=cuels))
expTotal

exp <- rgdx (fnIn, list(name='exp',form='full'))
exp$val

# this is close to correct: exp is ordered as exp(c,r), so exp(r,c) is zero
exp <- rgdx (fnIn, list(name='exp',form='full',uels=c(ruels,cuels)))
exp$val

# this is what we really want
exp <- rgdx (fnIn, list(name='exp',form='full',uels=c(cuels,ruels)))
exp$val

