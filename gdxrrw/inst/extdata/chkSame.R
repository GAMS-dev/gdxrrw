## utility functions for checking results: comparing different structures, etc.

isClose <- function (x, y) {
  dif <- abs(x - y)
  if (0 == dif) return (TRUE)
  m <- abs(x)
  t <- abs(y)
  if (t > m)
    m <- t
  if (dif/m <= 1e-15)  return (TRUE)
  return (FALSE)
}  # isClose

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
    if (n > 3) {
      ## element 4: symbol text
      if (! is.character(f1[[4]]))   return (FALSE)
      if (! is.character(f2[[4]]))   return (FALSE)
      if ("ts"         != f2Names[[4]])   return (FALSE)
      if (f1Names[[4]] != f2Names[[4]])   return (FALSE)
      if (f1[[4]] != f2[[4]])        return (FALSE)
    }
    if (n > 4) {
      return (FALSE)
    }
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

# compare the character vectors v1 and v2, return TRUE if the same, FALSE o/w
chkSameVec <- function(s, v1,v2) {
  if (! is.vector(v1,mode="character"))   return (FALSE)
  if (! is.vector(v2,mode="character"))   return (FALSE)
  n <- length(v1)
  if (n != length(v2))     return (FALSE)
  if (n == 0)              return (TRUE)
  for (k in c(1:n)) {
    if (v1[k] != v2[k]) {
      print (paste("checking", s, ": item", k, "is wrong"))
      return (FALSE)
    }
  }
  return (TRUE)
}  # chkSameVec


# compare the lists f1 and f2, return TRUE if the same, FALSE o/w
## this test is intended to compare the results of rgdx() calls and
## assumes a certain ordering of the list elements
## it is not really necessary that they be ordered in this way but it
## makes the test easier to implement
chkRgdxRes <- function(f1, f2) {
  isSparse <- TRUE

  r <- list(same=FALSE,msg="not lists")
  if (! is.list(f1))   return (r)
  if (! is.list(f2))   return (r)
  r$msg <- "bogus lengths"
  n     <- length(f1)
  if (n != length(f2))     return (r)
  if (n < 3)     return (r)

  f1Names <- names(f1)
  f2Names <- names(f2)

  r$msg <- "invalid/missing 'form' element"
  if (is.null(f1$form)) return (r)
  if (is.null(f2$form)) return (r)
  if (! is.character(f1$form))   return (r)
  if (! is.character(f2$form))   return (r)
  if (f1$form != f2$form)        return (r)
  if ('sparse' == f2$form) {
    isSparse <- TRUE
  }
  else if ('full' == f2$form) {
    isSparse <- FALSE
  }
  else {
    return (r)
  }

  r$msg <- "element 1 (name) error"
  ## element 1: symbol name
  if (! is.character(f1[[1]]))   return (r)
  if (! is.character(f2[[1]]))   return (r)
  if ("name"       != f2Names[[1]])   return (r)
  if (f1Names[[1]] != f2Names[[1]])   return (r)
  if (f1[[1]] != f2[[1]])        return (r)

  r$msg <- "element 2 (type) error"
  ## element 2: symbol type
  if (! is.character(f1[[2]]))   return (r)
  if (! is.character(f2[[2]]))   return (r)
  if ("type"       != f2Names[[2]])   return (r)
  if (f1Names[[2]] != f2Names[[2]])   return (r)
  if (f1[[2]] != f2[[2]])        return (r)

  for (k in 3:n) {
    r$msg <- paste("element",k,"error")
    if (f1Names[[k]] != f2Names[[k]])  return (r)
    if      ("dim" == f2Names[[k]]) {
      if (! is.numeric(f1[[k]]))   return (r)
      if (! is.numeric(f2[[k]]))   return (r)
      if (f1[[k]] != f2[[k]])      return (r)
    }
    else if ("val" == f2Names[[k]]) {
      if (! is.numeric(f1[[k]]))   return (r)
      if (! is.numeric(f2[[k]]))   return (r)
      if (! is.array(f1[[k]]))     return (r)
      if (! is.array(f2[[k]]))     return (r)
      dims1 <- dim(f1[[k]])
      dims2 <- dim(f2[[k]])
      nd <- length(dims2)
      if (length(dims1) != nd)     return (r)
      last <- 1
      for (d in 1:nd) {
        if (dims1[d] != dims2[d])  return (r)
        last <- last * dims1[d]
      }
      for (kk in 1:last) {
        if (f1[[k]][kk] == f2[[k]][kk])  next
        if (! isClose(f1[[k]][kk],f2[[k]][kk])) return (r)
        ## if (f1[[k]][kk] != f2[[k]][kk])  return (r)
      }
    }
    else if ("form" == f2Names[[k]]) {
      if (! is.character(f1[[k]]))   return (r)
      if (! is.character(f2[[k]]))   return (r)
      if (f1[[k]] != f2[[k]])        return (r)
    }
    else if ("uels" == f2Names[[k]]) {
      if (! is.list(f1[[k]]))   return (r)
      if (! is.list(f2[[k]]))   return (r)
      n1 <- length(f1[[k]])
      if (n1 != length(f2[[k]]))  return (r)
      for (i in 1:n1) {
        if (! chkSameVec ("", f1[[k]][[i]], f2[[k]][[i]])) return (r)
      }
    }
    else if ("ts" == f2Names[[k]]) {
      if (! is.character(f1[[k]]))   return (r)
      if (! is.character(f2[[k]]))   return (r)
      if (f1[[k]] != f2[[k]])        return (r)
    }
    else if ("te" == f2Names[[k]]) {
      if (isSparse) {
        if (! chkSameVec ("", f1[[k]], f2[[k]])) return (r)
      }
      else {
        if (! chkSameArray (f1[[k]], f2[[k]])) return (r)
      }
    }
    else {
      return (r)
    }
  }

  r$msg <- ''
  r$same <- TRUE
  return (r)
} # chkRgdxRes

# compare the arrays v1 and v2, return TRUE if the same, FALSE o/w
chkSameArray <- function (v1,v2) {
  if ("character" == mode(v1)) {
    if ("character" != mode(v2)) return (FALSE)
    if (! is.array(v1))   return (FALSE)
    if (! is.array(v2))   return (FALSE)
    dims1 <- dim(v1)
    dims2 <- dim(v2)
    nd <- length(dims1)
    if (nd != length(dims2))    return (FALSE)
    last <- 1
    for (d in 1:nd) {
      if (dims1[d] != dims2[d]) return (FALSE)
      last <- last * dims1[d]
    }
    for (k in 1:last) {
      if (v1[[k]] != v2[[k]])   return (FALSE)
    }
  }
  else if ("numeric" == mode(v1)) {
    if ("numeric" != mode(v2)) return (FALSE)
    if (! is.array(v1))   return (FALSE)
    if (! is.array(v2))   return (FALSE)
    dims1 <- dim(v1)
    dims2 <- dim(v2)
    nd <- length(dims1)
    if (nd != length(dims2))    return (FALSE)
    last <- 1
    for (d in 1:nd) {
      if (dims1[d] != dims2[d]) return (FALSE)
      last <- last * dims1[d]
    }
    for (k in 1:last) {
      if (v1[[k]] != v2[[k]])   return (FALSE)
    }
  }
  else {
    return (FALSE)
  }

  return (TRUE)
}  # chkSameArray

