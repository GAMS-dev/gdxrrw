# read in the GAMS data:
#   the states to plot data for
#   the commodities to consider
#   the scaled production for each commodity and state
#   the scale factor w for each state

# library(maps)

fnData <- "stateRes.gdx"
sdf <- rgdx.set(fnData,'s',compress=TRUE)
cdf <- rgdx.set(fnData,'c')
s <- (rgdx(fnData,list(name='sPrd',form='full',compress=TRUE)))$val
w <- (rgdx(fnData,list(name='w',form='full',compress=TRUE)))$val

slist <- as.vector(sdf$i)
ns <- length(slist)  # number of nodes
idx <- match(slist, state.name)
x <- state.center$x[idx]
y <- state.center$y[idx]
nc <- dim(cdf)[1]  # number of commodities: each commodity gets a color
colors <- rainbow(nc)

# define the maximum size of the chart elements
minradius <- +Inf
for (i in 1:ns) {
    for (j in 1:ns) {
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

map('state',region=slist)
map.axes()

for (k in 1:ns) {
    xx[npp] <- x[k]
    yy[npp] <- y[k]
    r <- rad0 * w[k]
    beta <- 0
    for (c in 1:nc) {
	alpha <- s[c,k]*2*pi
	for (i in 1:np) {
	    xx[i] <- xx[npp] + r*sin(beta + alpha *(i-1)/n)
	    yy[i] <- yy[npp] + r*cos(beta + alpha *(i-1)/n)
	}
	beta <- beta + alpha
	coord <- cbind(xx,yy)
	polygon (coord,col=colors[c])
    }
}
