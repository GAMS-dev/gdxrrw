fnData <- "eurodist.gdx"
fnSol <- "eurosol.gdx"
# suppressWarnings(file.remove(fnData,fnSol))

if (! require(stats))  stop ("stats package is not available")
n <- attr(eurodist,"Size")
cities <- attr(eurodist,"Labels")
uu <- list(c(cities))
dd <- as.matrix(eurodist)

clst <- list(name='cities', type='set', ts='cities from stats::eurodist', uels=uu)
dlst <- list(name='dist', type='parameter', dim=2, form='full',
             ts='distance', val=dd, uels=c(uu,uu))

wgdx (fnData, clst, dlst)
print ("Calling GAMS now - this may take some time.  Be patient.");
flush.console;
rc <- system (paste("gams tspMTZ.gms"))
# rc <- 0
if (0 != rc) {
  stop(paste("Bad return from gams: wanted 0, got",rc))
} else {
  print ("gams call succeeded")
}

lst <- list(name='modelstat',form='full',compress=TRUE)
v <- rgdx(fnSol, lst)
modelstat <- v$val
if (1 != modelstat)     stop(paste("Bad model status: expected 1, got", modelstat))

lst <- list(name='solvestat',form='full',compress=TRUE)
v <- rgdx(fnSol, lst)
solvestat <- v$val
if (1 != solvestat)     stop(paste("Bad solver status: expected 1, got", solvestat))

# tour <- rgdx.set(fnSol, "tour")
lst <- list(name='tour',form='sparse',compress=TRUE)
v <- rgdx(fnSol, lst)
nxt <- v$val[,2]

# now compute the sequence of cities to display, based on the next
# relationship in nxt
solSeq <- NA * c(1:n+1)
k <- 1
for (j in c(1:n)) {
  solSeq[j] <- k
  k <- nxt[k]
}
solSeq[n+1] <- 1
if (k != 1)  stop ("Bogus tour specified")

loc <- cmdscale(eurodist)
rx <- range(x <- loc[,1])
ry <- range(y <- -loc[,2])
tspres <- loc[solSeq,]
s <- seq(n)

plot(x, y, type="n", asp=1, xlab="", ylab="",
     main="optimal tour for traveling salesman problem")
arrows(tspres[s,1], -tspres[s,2], tspres[s+1,1], -tspres[s+1,2],
       angle=10, col="blue")
text(x, y, labels(eurodist), cex=0.8)
