
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



# rc <- wgdx.reshape (sample1a,symDim,"prd",tName='year',gdxName="test1.gdx")

# attr(sample1,"symName") <- "defaultPrd"
# attr(sample1,"ts") <- "explicit text"
# rc <- wgdx.reshape (sample1,3,tName='year',gdxName="testJJ.gdx")
