gdxName <- "eurodist"
symName <- "dist"
df <- rgdx.param(gdxName,symName)

n <- attr(eurodist,"Size")
cities <- attr(eurodist,"Labels")
dm <- as.matrix(eurodist)

