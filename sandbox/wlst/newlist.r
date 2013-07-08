## processScalar: process a scalar given as input to wgdx
## output a list suitable for raw wgdx
processScalar <- function(s, msg)
{
  symName <- attr(s, "symName", exact=TRUE)
  if (! is.character(symName)) {
    stop ("error processing ", msg, ": missing/bogus symName attribute")
  }
  o <- list (name=symName, type="parameter", dim=0, form="full", val=as.numeric(s))
  symText <- attr(s, "ts", exact=TRUE)
  if (is.character(symText)) {
    o$ts <- symText
  }
  o
} # processScalar

## processDF: process a dataframe given as input to wgdx
## output a list suitable for raw wgdx
processDF <- function(df, msg)
{
  symName <- attr(df, "symName", exact=TRUE)
  if (! is.character(symName)) {
    stop ("error processing ", msg, ": missing/bogus symName attribute")
  }
  nr <- nrow(df)
  nc <- ncol(df)
  isSet <- TRUE
  if (! is.factor(df[[1]])) {
    stop ("(",msg,")[[1]] must be a factor")
  }
  for (j in 1 + seq_len(max(0,nc-2))) {
    if (! is.factor(df[[j]])) {
      stop ("(",msg,")[[", j, "]] must be a factor")
    }
  }
  if (is.factor(df[[nc]])) {
    symType <- "set"
    symDim <- nc
  }
  else if (is.numeric(df[[nc]])) {
    symType <- "parameter"
    isSet <- FALSE
    symDim <- nc-1
  }
  o <- list (name=symName, type=symType, dim=symDim, form="sparse")
  symText <- attr(df, "ts", exact=TRUE)
  if (is.character(symText)) {
    o$ts <- symText
  }
  v <- matrix(0, nrow=nr, ncol=nc)
  uels <- c()
  if (! isSet) {
    v[,symDim+1] <- df[,symDim+1]
  }
  for (j in c(1:symDim)) {
    v[,j] <- as.numeric(df[,j])
    uels <- c(uels,list(levels(df[,j])))
  }
  o$val <- v
  o$uels <- uels
  o
} # processDF

# wgdx.new: write multiple symbols to a GDX file
# The information for a symbol comes encapsulated in a list or a data frame, call it X
# multiple symbols can be sent to this routine
# elements of "..." are either X or list(X1,X2,...,X3)
wgdx.new <- function(gdxName, ..., squeeze='y')
{
  if (! is.character(gdxName)) {
    stop ("bad gdxName: must be a GDX file name")
  }
  dotlist <- list(...)
  nInputs <- length(dotlist)
  print (paste("number of inputs: ", nInputs))
  olst <- list()
  kOut <- 0
  for (k in c(1:nInputs)) {
    item <- dotlist[[k]]
    print (paste("*** Processing item", k))
    if (is.data.frame(item)) {
      print (" *** found a data frame")
      kOut <- kOut + 1
      olst[[kOut]] <- processDF (item, paste("argument",k+1))
    }
    else if (! is.list (item) &&
             is.numeric(item) &&
             is.null(dim(item)) ) {
      # reading a scalar
      print (" *** found a scalar")
      kOut <- kOut + 1
      olst[[kOut]] <- processScalar (item, paste("argument",k+1))
    }
    else if (is.list (item)) {
      if (is.null(names(item))) {
        ## unnamed list: each element must be a dataframe, scalar, or symbol list
        print (" *** found an unnamed list:")
        nList <- length(item)
        for (kk in c(1:nList)) {
          item2 <- item[[kk]]
          print (paste("  *** Processing item2", kk))
          if (is.data.frame(item2)) {
            print ("   *** found a data frame")
            kOut <- kOut + 1
            olst[[kOut]] <- processDF (item2, paste0("arg",k+1,"[[",kk,"]]"))
          }
          else if (! is.list (item2) &&
                   is.numeric(item2) &&
                   is.null(dim(item2)) ) {
                                        # reading a scalar
            print ("   *** found a scalar")
            kOut <- kOut + 1
            olst[[kOut]] <- processScalar (item2, paste0("arg",k+1,"[[",kk,"]]"))
          }
          else if (is.list (item2)) {
            if (! is.null(names(item2))) {
              ## named list: add to the outputs
              kOut <- kOut + 1
              olst[[kOut]] <- item2
            }
            else {
              stop (paste0("invalid input found: arg",k+1,"[[",kk,"]] is a list but does not contain symbol info"))
            }
          }
          else {
            stop (paste0("unrecognized input found: arg",k+1,"[[",kk,"]] not valid"))
          }
        } # loop over kk: item2 members of item list
      }
      else {
        print (" *** found a named list:")
        kOut <- kOut + 1
        olst[[kOut]] <- item
      }
    }
    else {
      stop (paste("unrecognized input found: argument",k+1,"not valid"))
    }
  }

  wgdx (gdxName, olst, squeeze=squeeze)
} # wgdx.new
