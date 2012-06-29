# compare the lists f1 and f2, return TRUE if the same, FALSE o/w
## this test assumes a certain ordering of the list elements
## it is not really necessary that they be ordered in this way but it
## makes the test easier to implement
chkLists <- function(s, f1, f2) {
  if (! is.list(f1))   return (FALSE)
  if (! is.list(f2))   return (FALSE)
  n     <- length(f1)
  if (n != length(f2))     return (FALSE)
  if (n < 3)     return (FALSE)

  f1Names <- names(f1)
  f2Names <- names(f2)

  ## element 1: symbol name
  if (! is.character(f1[[1]]))   return (FALSE)
  if (! is.character(f2[[1]]))   return (FALSE)
  if ("name"       != f2Names[[1]])   return (FALSE)
  if (f1Names[[1]] != f2Names[[1]])   return (FALSE)
  if (f1[[1]] != f2[[1]])        return (FALSE)

  ## element 2: symbol type
  if (! is.character(f1[[2]]))   return (FALSE)
  if (! is.character(f2[[2]]))   return (FALSE)
  if ("type"       != f2Names[[2]])   return (FALSE)
  if (f1Names[[2]] != f2Names[[2]])   return (FALSE)
  if (f1[[2]] != f2[[2]])        return (FALSE)

  if ("set" == f2[[2]]) {
    ## element 3: set uels
    if (! is.list(f1[[3]]))   return (FALSE)
    if (! is.list(f2[[3]]))   return (FALSE)
    if ("uels"       != f2Names[[3]])   return (FALSE)
    if (f1Names[[3]] != f2Names[[3]])   return (FALSE)
    uList1 <- f1[[3]]
    uList2 <- f2[[3]]
    if (length(uList1) != length(uList2))  return (FALSE)
    ## loop through the dimensions, comparing the UELs in each
    for (d in 1:length(uList1)) {
      uels1 <- uList1[[d]]
      uels2 <- uList2[[d]]
      nk <- length(uels1)
      if (nk != length(uels2))  return (FALSE)
      for (k in 1:nk) {
        if (uels1[[k]] != uels2[[k]]) {
          return (FALSE)
        }
      }
    }
    if (n > 3) return (FALSE)
    return (TRUE)
  }

  print (paste("checking", s, ": test is not fully implemented"))
  return (FALSE)
  return (TRUE)
} # chkLists



# compare the data frames f1 and f2, return TRUE if the same, FALSE o/w
chkFrames <- function(s, f1, f2) {
  if (! is.data.frame(f1))   return (FALSE)
  if (! is.data.frame(f2))   return (FALSE)
  nc     <- ncol(f1)
  if (nc != ncol(f2))     return (FALSE)
  nr     <- nrow(f1)
  if (nr != nrow(f2))     return (FALSE)
  if (nr == 0)            return (TRUE)
  if (attr(f1,"symName") != attr(f2,"symName"))   return (FALSE)

  cNames1 <- names(f1)
  cNames2 <- names(f2)
  for (j in c(1:nc)) {
    if (cNames1[j] != cNames2[j]) {
      print (paste("checking", s, ": column", j, "names differ"))
      return (FALSE)
    }
    fc1 <- f1[[j]]
    fc2 <- f2[[j]]
    if (is.factor(fc1)) {
      if (! is.factor(fc2))   return (FALSE)
      if (nlevels(fc1) != nlevels(fc2))   return (FALSE)
      lvl1 <- levels(fc1)
      lvl2 <- levels(fc2)
      for (k in c(1:nlevels(fc1))) {
        if (lvl1[[k]] != lvl2[[k]])   return (FALSE)
      }
      if (length(fc1) != length(fc2))   return (FALSE)
      iv1 <- as.integer(fc1)
      iv2 <- as.integer(fc2)
      for (k in c(1:length(fc1))) {
        if (iv1[[k]] != iv2[[k]])   return (FALSE)
      }
    }
    else if (is.vector(fc1)) {
      if (! is.vector(fc2))   return (FALSE)
      if (length(fc1) != length(fc2))   return (FALSE)
      for (k in c(1:length(fc1))) {
        if (fc1[[k]] != fc2[[k]])   return (FALSE)
      }
    }
    else {
      print (paste("checking", s, ": column", j, "type unrecognized"))
      return (FALSE)
    }
  }

  rNames1 <- row.names(f1)
  rNames2 <- row.names(f2)
  for (i in c(1:nr)) {
    if (rNames1[i] != rNames2[i]) {
      print (paste("checking", s, ": row", i, "names differ"))
      return (FALSE)
    }
  }
  return (TRUE)
} # chkFrames
