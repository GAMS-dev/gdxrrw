
source ("wgdx.reshape.R")

# the data file and read.csv should not change, that is basic R stuff
# and we don't want to force any change there
sample1 <- read.csv ("sample1Mixed.dat", sep="\t", header=T)
# we are really dealing with 3-dimensional data: production(crop, region, year)
symDim <- 3


# write the data, but specify the symbol name in the call
# also specify the name of the aggregated (aka time) index
rc <- wgdx.reshape (sample1,symDim,symName='prod',gdxName="testMix.gdx",tName="year",order=c(0,7,1))


