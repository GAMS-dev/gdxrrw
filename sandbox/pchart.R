# read in the 
fnData <- "results.gdx"
kdf <- rgdx.set(fnData,'k')
xdf <- rgdx.param(fnData,'x')
x <- xdf$value
ydf <- rgdx.param(fnData,'y')
y <- ydf$value
nk <- dim(kdf)[1]  # number of nodes
cdf <- rgdx.set(fnData,'c')
nc <- dim(cdf)[1]  # number of commodities: each commodity gets a color
colors <- rainbow(nc)
xmin <- +Inf
xmax <- -Inf
ymin <- +Inf
ymax <- -Inf

# define the maximum size of the chart elements, and the coords for
# the plot
minradius <- +Inf
for (i in 1:nk) {
    xmin <- min(xmin,x[i])
    xmax <- max(xmax,x[i])
    ymin <- min(ymin,y[i])
    ymax <- max(ymax,y[i])
    for (j in 1:nk) {
    	if (i != j) {
	   r <- (x[i]-x[j])^2 + (y[i]-y[j])^2
	   minradius <- min(minradius,r)
        }
    }
}
minradius <- sqrt(minradius)

np <- 200  # define the number of polygon points on the arc
npp <- np + 1
n <- np - 1
xx <- rep(0,npp)
yy <- xx
rad0 <- max(0.4, 0.4 * minradius)  # maximum radius of pie chart

# plot(xmin:xmax, ymin:ymax)
plot(0.5:3.5,0.5:3.5)
# plot(x,y,type="p")
for (k in 1:nk) {
    xx[npp] <- x[k]
    yy[npp] <- y[k]
    r <- rad0
    beta <- 0
    for (c in 1:nc) {
	alpha <- (1/3)*2*pi
	for (i in 1:np) {
	    xx[i] <- xx[npp] + r*sin(beta + alpha *(i-1)/n)
	    yy[i] <- yy[npp] + r*cos(beta + alpha *(i-1)/n)
	}
	beta <- beta + alpha
	coord <- cbind(xx,yy)
	polygon (coord,col=colors[c])
    }
}
