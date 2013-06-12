d <- c(2,2,2,2)
n <- length(d)
aaa <- array(0,d)
for (i1 in 1:d[1]) {
  print (paste("i1 =",i1))
  for (i2 in 1:d[2]) {
    print (paste("  i2 =",i2))
    for (i3 in 1:d[3]) {
      print (paste("    i3 =",i3))
      for (i4 in 1:d[4]) {
        print (paste("      i4 =",i4))
        aaa[i1,i2,i3,i4] <- i1*1000 + i2*100 + i3*10 + i4
      }
    }
  }
}
print(aaa)
m <- length(aaa)
for (kk in 1:m) {
  print (paste("aaa[kk] =",aaa[kk]))
}
uels <- list(c("i1","i2"),c("j1","j2"),c("k1","k2"),c("Lo","m"))

v <- list(name='v',type='variable',form='full',val=aaa,uels=uels,
          typeCode=GMS_VARTYPE$FREE)
wgdx ('dense', v)
