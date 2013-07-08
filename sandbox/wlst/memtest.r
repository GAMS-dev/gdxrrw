adder <- function (a, b, c) {
  d <- dim(a)
  ds <- d[1] + d[2]
  d <- dim(b)
  ds <- ds + d[1] + d[2]
  d <- dim(c)
  ds <- ds + d[1] + d[2]
  print (paste("dimsum =", ds))
  gc()
} # adder

adder2 <- function (a, b, c) {
  a[,] <- a[,] * 2
  d <- dim(a)
  ds <- d[1] + d[2]
  b[,] <- b[,] * 2
  d <- dim(b)
  ds <- ds + d[1] + d[2]
  c[,] <- c[,] * 2
  d <- dim(c)
  ds <- ds + d[1] + d[2]
  print (paste("dimsum2 =", ds))
  gc()
} # adder

gc()
M <- 1000
bigg <- matrix(4, M, M)
print ("0000")
gc()

adder(bigg, bigg, bigg)

print ("1000")
gc()

adder2(bigg, bigg, bigg)
print ("2000")
gc()

symList <- list(name="tester",v=bigg)
print ("3000")
gc()

rm(bigg)
rm(symList)
print ("4000")
gc()
