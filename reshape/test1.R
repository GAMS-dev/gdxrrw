
source ("wgdx.reshape.R")

# the data file and read.csv should not change, that is basic R stuff
# and we don't want to force any change there
sample1 <- read.csv ("sample1.dat", sep="\t", header=T)

rc <- wgdx.reshape ("test1.gdx",sample1,"prd",symDim=3,tName='times')

