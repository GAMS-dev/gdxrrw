# it would be cool to do a German example
# so far, we have downloaded DEU_adm1.RData and poked around
# do it later if time allows

load("DEU_adm1.RData")
str(gadm)
help(SpatialPolygonsDataFrame)
gadm@data
slotNames(gadm)
gadm@polygons
str(gadm@polygons)
str((gadm@polygons)[[1]])
