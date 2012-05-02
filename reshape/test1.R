
source ("wgdx.reshape.R")

# the data file and read.csv should not change, that is basic R stuff
# and we don't want to force any change there
sample1 <- read.csv ("sample1.dat", sep="\t", header=T)
# we are really dealing with 3-dimensional data: production(crop, region, year)
symDim <- 3

# the fewest args you can have is two, but you need to specify the symName
# as an attribute in that case, and you get a list you have to send to GDX
# with an additional call
sample1a <- sample1
attr(sample1a,"symName") <- "production"
lst <- wgdx.reshape (sample1a,symDim)
wgdx.lst ('test1.gdx', lst)
str(lst)

# or just write to GDX directly by specifying the GDX name
rc <- wgdx.reshape (sample1a,symDim,gdxName="test2.gdx")

# write the data, but specify the symbol name in the call
# also specify the name of the aggregated (aka time) index
rc <- wgdx.reshape (sample1,symDim,symName='prod',gdxName="test3.gdx",tName="year")


# put the symbol name and text in the input data,
# and write the symbol only - no index sets
sample1b <- sample1
attr(sample1b,"symName") <- "prd4"
attr(sample1b,"ts") <- "text for prd4"
rc <- wgdx.reshape (sample1b,symDim,tName='year',gdxName="test4.gdx",setsToo=FALSE)

rc <- wgdx.reshape (sample1b,symDim,tName='year',gdxName="test5.gdx",order=c(2,1,-1))


# reorder test4 so the output parameter looks like prd(year, crop, region)
# rc <- wgdx.reshape (sample1b,symDim,tName='year',gdxName="test5.gdx",setsToo=FALSE,order=c(-1,2,1))
# rc <- wgdx.reshape (sample1b,symDim,tName='year',gdxName="test6.gdx",setsToo=FALSE,order=c('*','crop','region'))
