nrnodes <- 2

# define the maximum size of the chart elements
minradius <- 10000
radius <- rep (0, nrnodes^2 - nrnodes)
nr <- 1
radius[nr]
for (i in 1:nrnodes) {
    for (j in 1:nrnodes) {
    	if (i != j) {
	   radius[nr] <- sqrt((nodes$x[i]-nodes$x[j])^2 + (nodes$y[i]-nodes$y[j])^2)
	   radius[nr] <- max(0,radius[nr])
	   minradius <- min(minradius,radius[nr])
	   nr <- nr + 1
    }
}
