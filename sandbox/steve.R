## Author:  Renger van Nieuwkoop
## Email:   renger@vannieuwkoop.ch
## Projekt: gdxrrw
## Thema:   Plotting Networks
## Dir:     d:/PhD/Projekte/Parking/R/
## Date:    06 Feb 2012

## Version Control Information: $Id: RscriptforGams.R 1581 2012-07-18 18:44:30Z renger $

## ===================================================================

gdxfile<-"example.gdx"

# Read the arcs, nodes and results from the gdx file

source("plotnetwork.R")

input       <-readresults(gdxfile)
nodes       <-input[[1]]
arcs        <-input[[2]]
coordinates <-input[[3]]
results     <-input[[4]]

# plotnet(coordinates)
# plotpie(coordinates,results)
# plotnet(coordinates)
plotbars(coordinates,results)

