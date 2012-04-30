
source ("wgdx.reshape.R")

# the data file and read.csv should not change, that is basic R stuff
# and we don't want to force any change there
sample1 <- read.csv ("sample1.dat", sep="\t", header=T)

rc <- wgdx.reshape (sample1,3,"prd",tName='year',gdxName="test1.gdx")

attr(sample1,"symName") <- "defaultPrd"
attr(sample1,"ts") <- "explicit text"
rc <- wgdx.reshape (sample1,3,tName='year',gdxName="test2.gdx")
