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

# plot the pie charts
prec <- 200  # define the number of polygon points
x <- rep(0,prec)
n <- prec - 1
y <- x
colors <- rainbow(rnr)
nr <- 1
rad0 <- max(0.3, 0.4 * minradius)  # maximum radius of pie chart

for (row in 1:gridrows) {
    for (column in 1:gridcols) {
    	rad <- rad0 * resultsscaled[nr,rnr+2]
	alpha <- 0
	beta  <- 0
	x <- rep(nodes[nr,"x"],prec)
	y <- rep(nodes[nr,"y"],prec)
	x[1] <- x[1] + rad*sin(beta)
	y[1] <- y[1] + rad*cos(beta)
	for (rnr in 1:3) {
	    alpha <- (resultsscaled[nr,rnr+1])*2*pi
	    for (i in 2:(prec-1)) {
	    	x[i] <- x[i] + rad*sin(beta+i/n*alpha)
	    	y[i] <- y[i] + rad*cos(beta+i/n*alpha)
	    }
	    coord <- cbind(x,y)
	    coord
	    polygon (x,y,col=colors[rnr])
	    x <- rep(nodes[nr,"x"],prec)
	    y <- rep(nodes[nr,"y"],prec)
	    beta <- beta + alpha
	    x[1] <- x[1] + rad*sin(beta)
	    y[1] <- y[1] + rad*cos(beta)
	}
	nr <- nr + 1
    }
}
