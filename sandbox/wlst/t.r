ii <- rgdx ('target', list(name='i'))
aa <- rgdx ('target', list(name='a'))
bb <- rgdx ('target', list(name='b'))
idf <- rgdx.set ('target', 'i')
adf <- rgdx.param ('target', 'a')
bdf <- rgdx.param ('target', 'b')
cdf <- rgdx.param ('target', 'c')
ddf <- rgdx.param ('target', 'd')
edf <- rgdx.param ('target', 'e')
fdf <- rgdx.param ('target', 'f')
gdf <- rgdx.param ('target', 'g')

# works with current interface
wgdx.lst('old0', list(idf, adf))
cmd <- 'gdxdiff target old0 id="i a"'
rc <- system(cmd)
if (0 != rc) {
  print (paste("GDXDIFF FAILURE:", cmd));
} else {
  print (paste("gdxdiff ok:", cmd));
}

wgdx.lst('mix', list(ii, adf, bb, cdf))
cmd <- 'gdxdiff target mix id="i a b c"'
rc <- system(cmd)
if (0 != rc) {
  print (paste("GDXDIFF FAILURE:", cmd));
} else {
  print (paste("gdxdiff ok:", cmd));
}

wgdx.lst('dupe', list(ii, adf, bb, cdf, ddf, edf, fdf, gdf))
cmd <- 'gdxdiff target dupe'
rc <- system(cmd)
if (0 != rc) {
  print (paste("GDXDIFF FAILURE:", cmd));
} else {
  print (paste("gdxdiff ok:", cmd));
}


source ("newlist.r")
# -------------------- works with current interface ----------------
wgdx.new('old0', list(idf, adf))
cmd <- 'gdxdiff target old0 id="i a"'
rc <- system(cmd)
if (0 != rc) {
  stop (paste("GDXDIFF FAILURE:", cmd));
} else {
  print (paste("gdxdiff ok:", cmd));
}

wgdx.new('mix', list(ii, adf, bb, cdf))
cmd <- 'gdxdiff target mix id="i a b c"'
rc <- system(cmd)
if (0 != rc) {
  stop (paste("GDXDIFF FAILURE:", cmd));
} else {
  print (paste("gdxdiff ok:", cmd));
}

wgdx.new('dupe', list(ii, adf, bb, cdf, ddf, edf, fdf, gdf))
cmd <- 'gdxdiff target dupe'
rc <- system(cmd)
if (0 != rc) {
  stop (paste("GDXDIFF FAILURE:", cmd));
} else {
  print (paste("gdxdiff ok:", cmd));
}



# ---------------- want to work ---------------
# wgdx.lst('new', idf)
# wgdx.lst('new', idf, adf, bdf, cdf, )
