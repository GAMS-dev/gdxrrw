
## Author:  Renger van Nieuwkoop
## Email:   renger@vannieuwkoop.ch
## Projekt: Parking
## Thema:   Plotting Network Results
## Dir:     d:/PhD/Projekte/Parking/R/
## Date:    06 Feb 2012

## $Id: plotnetwork.R 1581 2012-07-18 18:44:30Z renger $

## ===================================================================
# Functions:


# Auxilliary functions for plotting
# - findradius:      find maximium size of chart elements
# - scaleresults:    scale results of dependent variables using sum over variable
# - scaleresultsInd: scale results of independent variables using maximum of variable
# - minxcoord:       calculate x coordinate for lower left corner of network
# - minycoord:       calculate y coordinate for lower left corner of network

# Functions for base network plot
# - plotnet:         plot the network with the arcs
# - plotnodes:       plot the nodes
# - plotconnections: plot the arcs

# Functions for plotting results on the network
# - plotbars:        plot bars of dependent variables at every node
# - plotbarsInd:     plot bars of independent variables at every node
# - plotpie:         plot piecharts at every node

# Functions for reshaping data
# - readresults:     read results from gdx file for the network
# - gsr:             global search replace function
# - gdxReshape:      reshape a dataframe for output with gdx

# Define the maximum size of the chart elements

findradius<-function(coordinates_=coordinates){
    nrnodes   <- 2
#    length(coordinates_[,1])
    minradius <- 100000000   # smallest radius possible initialization
    radius    <- rep(0,nrnodes^2-nrnodes)  # Inititialize the radius vector
    nr        <- 1
    radius[nr]
    for (i in 1:nrnodes){
        for (j in 1:nrnodes){
            if (i != j){
            radius[nr] <- sqrt((coordinates_$x[i]- coordinates_$x[j])^2+ (coordinates_$y[i]- coordinates_$y[j])^2)
            radius[nr] <- max(0,radius[nr])
            minradius  <- min(minradius,radius[nr])
            nr         <- nr+1}
        }
    }
    return(minradius)
}

# Scale results
scaleresults<-function(results=results){
    (nrresults              <- dim(results)[2]-1)
    (results0               <- cbind(results,sum=rowSums(results[,c(2:(nrresults+1))],na.rm=T)))
    (resultsscaled          <- results0)
    (resultsscaled[,c(2:(nrresults+2))] <- resultsscaled[,c(2:(nrresults+2))]/results0[,(nrresults+2)])
    return(resultsscaled)
}

scaleresultsAll<-function(results=results){
    nrresults              <- dim(results)[2]-1
    maxresults0            <- max(results[,2:nrresults],na.rm=T)
    resultsscaled          <- results/maxresults0
    return(resultsscaled)
}

scaleresultsInd<-function(results=results){
    resultsscaledInd<-results[,1:(1+nrresults)]
    for (i in 2:(1+nrresults)){
        resultsscaledInd[,i]<-results[,i]/max(results[,i],na.rm=T)
    }
    return(resultsscaledInd)
}

minxcoord<-function(coordinates_=coordinates){
    minx<-1.1*min(coordinates_$x)
    return(minx)
}

minycoord<-function(coordinates_=coordinates){
    minradius<-findradius(coordinates_)
    miny<-0.5*min(coordinates_$y)-minradius
    return(miny)
}

plotnodes <- function (coordinates_=coordinates,nodes_=nodes,cexsize=0.8,colornode="dark blue", colortext="white",xdisp=0,ydisp=0) {
    for (n in 1:length(coordinates_[,"node"]))
        {print(points(coordinates_[n,"x"]+xdisp,coordinates_[n,"y"]+ydisp, type="p", pch=19,col=colornode,cex=4))}
    for (n in 1:length(coordinates_[,"node"]))
        {print(text(coordinates_[n,"x"]+xdisp,coordinates_[n,"y"]+ydisp,nodes_[n,"nodename"],col=colortext,cex=cexsize))}
}

# Plot a canvas for the network with the connections
plotnet<-function(coordinates_=coordinates,arcs_=arcs){
    minradius <-findradius(coordinates_)
    scaley    <- c(-0.05*minradius+min(coordinates_$y),max(coordinates_$y) + 0.95*minradius)
    scalex    <- c(max(coordinates_$x)+0.05*minradius,min(coordinates_$x)- 0.05*minradius)
    print(plot(scalex,scaley,axes=F,xlab="",ylab="",t="n"))
# axis(1)
# axis(2)
    plotconnections(coordinates_,arcs_)
}

plotconnections<- function(coordinates_=coordinates,arcs_=arcs,colorconnections="black") {
    for (n in 1:length(arcs_[,"start"]))
        {print(lines(c(coordinates_[arcs_[n,"start"] == coordinates_[,"node"],"x"],
            coordinates_[arcs_[n,"end"]   == coordinates_[,"node"],"x"]),
           c(coordinates_[arcs_[n,"start"] == coordinates_[,"node"],"y"],
            coordinates_[arcs_[n,"end"]   == coordinates_[,"node"],"y"]),
           col=colorconnections))
    }
}

# Define colors automatically based on the number of results and define width and
# height based on size of grid
plotbars<-function(coordinates_=coordinates,results_=results){
# Plot bars
    plotnet(coordinates_)
    nrresults     <- dim(results_)[2]-1
    nrnodes       <- length(coordinates_[,1])
    resultsscaled <- scaleresults(results_)
    colors        <- rainbow(nrresults)
    minradius     <- findradius(coordinates_)
    nr            <- 1   # Counter for nodes
    corners       <- 4   # Number of corners in a bar
    width         <- max(0,0.4* minradius/nrresults)   # Maximum width of bar
    height0       <- max(0.5*minradius,0.95*minradius)            # Maximum height of bar

    for (nodenr in 1:nrnodes){
        height <- height0*resultsscaled[nodenr,nrresults+2]
        x      <- rep(coordinates_[nodenr,"x"],corners)-nrresults*width/2
        y      <- rep(coordinates_[nodenr,"y"],corners)
        for (nrresults in 1:nrresults){
            height <- resultsscaled[nodenr,nrresults+1]*height0
            x[2]   <- x[1]+ width
            x[3]   <- x[2]
            x[4]   <- x[1]
            y[2]   <- y[1]
            y[3]   <- y[1]+ height
            y[4]   <- y[3]
            polygon(x,y,col=colors[nrresults])
            y      <- rep(coordinates_[nodenr,"y"],corners)
            x      <- x+width
        }
    }
    plotnodes(coordinates_, cexsize=0.8,colornode="transparent",colortext="black",xdisp=0.2*minradius,ydisp=-0.2*minradius)
    minx<-minxcoord()
    miny<-minycoord()
    legend(minx,miny, colnames(results)[2:(nrresults+1)],colors, fill=colors, ncol = nrresults, cex = 0.8)
}

# Plot bars at the nodes of the network (for several independet results)
plotbarsInd<-function(coordinates_=coordinates,results_=results){
# Plot bars
    plotnet(coordinates_)
    minradius <- findradius(coordinates_)
    nrresults <- dim(results_)[2]-1
    colors    <- rainbow(nrresults)
    nrnodes   <- length(results_[,1])
    nr        <- 1   # Counter for nodes
    corners   <- 4   # Number of corners in a bar
    width     <- max(0.1*minradius,0.4* minradius/nrresults)   # Maximum width of bar
    height0   <- max(0.5*minradius,0.8*minradius)            # Maximum height of bar
    resultsscaledInd<-scaleresultsInd(results_)
    for (nodenr in 1:nrnodes){
        x    <- rep(coordinates_[as.integer(results$Node[nodenr]),"x"],corners)-nrresults*width/2
        y    <- rep(coordinates_[as.integer(results$Node[nodenr]),"y"],corners)
        for (nr in 1:nrresults){
            height <- resultsscaledInd[nodenr,nr+1]*height0
            x[2]   <- x[1]+ width
            x[3]   <- x[2]
            x[4]   <- x[1]
            y[2]   <- y[1]
            y[3]   <- y[1]+ height
            y[4]   <- y[3]
            polygon(x,y,col=colors[nr])
            y      <- rep(coordinates_[as.integer(results_$Node[nodenr]),"y"],corners)
            x      <- x+width
        }
    }
    plotnodes(coordinates_, cexsize=0.8,colornode="transparent",colortext="black",xdisp=0.2*minradius,ydisp=-0.2*minradius)
    minx<-minxcoord()
    miny<-minycoord()
    legend(minx,miny, colnames(results_)[2:(nrresults+1)],colors, fill=colors, ncol = nrresults, cex = 0.8)
}

plotbarsAll<-function(coordinates_=coordinates,results_=results){
# Plot bars
    plotnet(coordinates_)
    minradius <- findradius(coordinates_)
    nrresults <- dim(results_)[2]-1
    colors    <- rainbow(nrresults)
    nrnodes   <- length(results_[,1])
    nr        <- 1   # Counter for nodes
    corners   <- 4   # Number of corners in a bar
    width     <- max(0.1*minradius,0.4* minradius/nrresults)   # Maximum width of bar
    height0   <- max(0.5*minradius,0.8*minradius)            # Maximum height of bar
    resultsscaledAll<-scaleresultsAll(results_)
    for (nodenr in 1:nrnodes){
        x    <- rep(coordinates_[as.integer(results_$Node[nodenr]),"x"],corners)-nrresults*width/2
        y    <- rep(coordinates_[as.integer(results_$Node[nodenr]),"y"],corners)
        for (nr in 1:nrresults){
            height <- resultsscaledAll[nodenr,nr+1]*height0
            x[2]   <- x[1]+ width
            x[3]   <- x[2]
            x[4]   <- x[1]
            y[2]   <- y[1]
            y[3]   <- y[1]+ height
            y[4]   <- y[3]
            print(polygon(x,y,col=colors[nr]))
            y      <- rep(coordinates_[as.integer(results_$Node[nodenr]),"y"],corners)
            x      <- x+width
        }
    }
    plotnodes(coordinates_, cexsize=0.8,colornode="transparent",colortext="black",xdisp=0.2*minradius,ydisp=-0.2*minradius)
    minx<-minxcoord()
    miny<-minycoord()
#    legend(minx,miny, colnames(results_)[2:(nrresults+1)],colors, fill=colors, ncol = nrresults, cex = 0.8)
    legend(0,miny+.29, colnames(results_)[2:(nrresults+1)],colors, fill=colors, ncol = 2, cex = 0.8)
}

# Function for plotting pie charts with results
plotpie<-function(coordinates_=coordinates,results_=results){
    plotnet(coordinates_)
    nrresults                   <- dim(results_)[2]-1
    nrnodes                     <- length(coordinates_[,1])
    colors                      <- rainbow(nrresults)
    minradius                   <- findradius(coordinates_)
    prec                        <- 200   # Define the number of polygon points
    x                           <- rep(0,prec)
    n                           <- prec-1
    y                           <- x
    resultsscaled               <- scaleresults(results_)
    results_                    <- cbind(results_,sum=rowSums(results_[,c(2:(nrresults+1))]))
    resultsscaled[,nrresults+2] <- results_[,nrresults+2]/max(results_[,nrresults+2])
    rad0                        <- max(0.3*minradius, 0.4 * minradius)   # Maximum radius of piechart

    for (nodenr in 1:nrnodes){
        rad   <- rad0*resultsscaled[nodenr,nrresults+2] # Define the radius of the pie chart
        alpha <- 0   # start north
        beta  <- 0   # initialize the starting angle
        x     <- rep(coordinates_[nodenr,"x"],prec)
        y     <- rep(coordinates_[nodenr,"y"],prec)
        y[1]  <- y[1]+rad*cos(alpha) # define the starting coordinate for y

        for (nrresults in 1:nrresults){
            alpha <- (resultsscaled[nodenr,nrresults+1])*2*pi  # define the angle of the pie chart part
            for (i in 2:(prec-1)){                   # define the coordinates for the polygon to be drawn
                x[i] <- x[i]+rad*sin(beta+i/n*alpha)
                y[i] <- y[i]+rad*cos(beta+i/n*alpha)
            }
            coord <- cbind(x,y)
            polygon(x,y,col=colors[nrresults])
            x     <- rep(coordinates_[nodenr,"x"],prec)
            y     <- rep(coordinates_[nodenr,"y"],prec)
            beta  <- beta+alpha     # update the starting angle
            y[1]  <- y[1]+rad*cos(beta) # update the y-coordinate
            x[1]  <- x[1]+rad*sin(beta) # update the x-coordinate
        }
    y[1] <- y[1] + rad*cos(alpha)
    }
    plotnodes(coordinates_, cexsize=0.8,colornode="transparent",colortext="black",xdisp=0.35*minradius,ydisp=-0.35*minradius)
    minx<-minxcoord()
    miny<-minycoord()
    legend(0,miny+.29, colnames(results_)[2:(nrresults+1)],colors, fill=colors, ncol = 2, cex = 0.8)
}

readresults<-function(gdxfile) {
    input                 <-list(rgdx.set(gdxfile,"nodes"),
                                 rgdx.set(gdxfile,"arcs"),
                                 rgdx.param(gdxfile,"coordinates",squeeze=F),
                                 rgdx.param(gdxfile,"results"))
    nodes                 <- input[[1]]
    nodes$nodename        <- as.character(nodes[,1])
    colnames(nodes)[1]    <- "nodename"
    nodes$node            <- seq(1:length(nodes$nodename))
    input[[1]]            <- nodes

    arcs                  <- input[[2]]
    names(arcs)           <- c("start","end")
    arcs[,"start"]        <- gsr(arcs$start,nodes$nodename,nodes$node)
    arcs[,"end"]          <- gsr(arcs$end,nodes$nodename,nodes$node)
    input[[2]]            <- arcs

    coordinates           <- input[[3]]
    zeros                 <- coordinates == 99999
    coordinates[zeros]    <- 0
    coordinates           <- reshape(coordinates, idvar="i",timevar="j",direction="wide")
    colnames              <- unlist(strsplit(names(coordinates[,c(2:dim(coordinates)[2])]),"\\."))
    colnames              <- colnames[seq(2,length(colnames),2)]
    coordinates           <- coordinates[,c(2:dim(coordinates)[2])]
    colnames(coordinates) <- colnames
    input[[3]]            <- coordinates

    results               <- input[[4]]
    results               <- reshape(results,idvar="i",timevar="j",direction="wide")
    zeros                 <- is.na.data.frame(results)
    results[zeros]        <- 0
    colnames              <- unlist(strsplit(names(results[c(2:dim(results)[2])]),"\\."))
    colnames              <- colnames[seq(2,length(colnames),2)]
    names(results)        <- c("node",colnames)
    results[,"node"]      <- gsr(results$node,nodes$nodename,nodes$node)
    input[[4]]            <- results
    return(input)
}

# Global search replace function
gsr <- function(Source, Search, Replace)
{
    if (length(Search) != length(Replace))
        stop("Search and Replace Must Have Equal Number of Items\n")
    Changed <- as.character(Source)
    for (i in 1:length(Search)){
        cat("Replacing: ", Search[i], " With: ", Replace[i], "\n")
        Changed <- replace(Changed, Changed == Search[i], Replace[i])
    }
  cat("\n")
  Changed
}


# given an input dataframe inDF, return a reshaped dataframe prepped
# for output via wgdx.df or wgdx.lst.
# The output parameter parName will have dimension nDims
gdxReshape <- function(inDF, parName, nDims, aggName=NULL) {
    nCols   <- ncol(inDF)
    inNames <- names(inDF)
    idCols  <- 1:(nDims-1)
    dtCols  <- nDims:nCols

    outDF   <- reshape (inDF, idvar=inNames[idCols], varying=list(dtCols),
                    direction="long", times=inNames[dtCols])
    for (i in 1:nDims) {
        outDF[[i]] <- as.factor(outDF[[i]])
    }
    attr(outDF,"symName") <- parName
    if (is.character(aggName)) {
        names(outDF)[nDims] <- aggName
    }
    names(outDF)[nDims+1] <- "value"
    return (outDF)
}

# Function for removing repeating row names
cleanf <- function(x){
   oldx <- c(FALSE, x[-1]==x[-length(x)])
  # is the value equal to the previous?
   res <- x
   res[oldx] <- NA
  return(res)}
