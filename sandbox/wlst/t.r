ii <- rgdx ('target', list(name='i'))
aa <- rgdx ('target', list(name='a'))
bb <- rgdx ('target', list(name='b'))
uu <- rgdx ('target', list(name='u'))
idf <- rgdx.set ('target', 'i')
adf <- rgdx.param ('target', 'a')
bdf <- rgdx.param ('target', 'b')
cdf <- rgdx.param ('target', 'c')
ddf <- rgdx.param ('target', 'd')
edf <- rgdx.param ('target', 'e')
fdf <- rgdx.param ('target', 'f')
gdf <- rgdx.param ('target', 'g')
usc <- rgdx.scalar ('target', 'u')
vsc <- rgdx.scalar ('target', 'v')

#  ---------- works with older wgdx.lst: 0.2.0 and previous ---------------
wgdx.lst('old0', list(idf, adf))
cmd <- 'gdxdiff target old0 id="i a"'
rc <- system(cmd)
if (0 != rc) {
  print (paste("GDXDIFF FAILURE:", cmd));
} else {
  print (paste("gdxdiff ok:", cmd));
}
wgdx.lst('mix', list(ii, adf, bb, cdf, vsc))
cmd <- 'gdxdiff target mix id="i a b c v"'
rc <- system(cmd)
if (0 != rc) {
  print (paste("GDXDIFF FAILURE:", cmd));
} else {
  print (paste("gdxdiff ok:", cmd));
}
wgdx.lst('dupe', list(ii, adf, bb, cdf, ddf, edf, fdf, gdf, uu, vsc))
cmd <- 'gdxdiff target dupe'
rc <- system(cmd)
if (0 != rc) {
  print (paste("GDXDIFF FAILURE:", cmd));
} else {
  print (paste("gdxdiff ok:", cmd));
}

# ------------ requires newer wgdx.lst: 0.4.0 or later -------------
wgdx.lst('new0', idf, adf)
cmd <- 'gdxdiff target new0 id="i a"'
rc <- system(cmd)
if (0 != rc) {
  stop (paste("GDXDIFF FAILURE:", cmd));
} else {
  print (paste("gdxdiff ok:", cmd));
}
wgdx.lst('new1', list(idf), list(adf))
cmd <- 'gdxdiff target new1 id="i a"'
rc <- system(cmd)
if (0 != rc) {
  stop (paste("GDXDIFF FAILURE:", cmd));
} else {
  print (paste("gdxdiff ok:", cmd));
}
wgdx.lst('new2', list(idf,aa,uu), list(bb,vsc,edf,gdf),cdf)
cmd <- 'gdxdiff target new2 id="i a u b v e g c"'
rc <- system(cmd)
if (0 != rc) {
  stop (paste("GDXDIFF FAILURE:", cmd));
} else {
  print (paste("gdxdiff ok:", cmd));
}
wgdx.lst('new3', fdf, list(idf,aa,uu), ddf, list(bb,vsc,edf,gdf), cdf)
cmd <- 'gdxdiff target new3'
rc <- system(cmd)
if (0 != rc) {
  stop (paste("GDXDIFF FAILURE:", cmd));
} else {
  print (paste("gdxdiff ok:", cmd));
}
