#### test runtime of filtered reads

print ("timing test for filtered reads")
gams('?')
fnIn <- "aIJ.gdx"


## first read the universe
u <- rgdx(fnIn)

## read a parameter unfiltered
t1 <- system.time(c1 <- rgdx(fnIn,list(name='A')))

## same read but filtered with the universe
t2 <- system.time(c2 <-
         rgdx(fnIn,list(name='A',
                        uels=list(u$uels,u$uels) )))

print (paste(" card(universe):", length(u$uels)))
print (paste("unfiltered time:", t1[[3]]))
print (paste("  filtered time:", t2[[3]]))
