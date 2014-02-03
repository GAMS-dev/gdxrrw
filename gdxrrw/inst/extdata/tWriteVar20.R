## ## test wgdx with form='sparse', 20-d variable: see if we can go big

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

testName <- '20-dim var with 2**20 ~= 1M entries'

errFunc <- function(ex) {
  print (paste("test of wgdx on",testName,"failed"))
  print(ex)
  FALSE
} # errFunc

tryCatch({
  print (paste("testing wgdx on", testName))
  print ("-----------------------------------------------")
  print ("--- N.B.: This test may take a minute or so ---")
  print ("-----------------------------------------------")
  wgdx('?')
  fnOut <- "tmp.gdx"
  fnWant <- "tVar20.gdx"
  if (! file_test ('-f', fnWant)) {
    stop (paste("FAIL: File-to-duplicate", fnWant, "does not exist"))
  }

  u1 <- c("d1_1","d1_2")
  u2 <- c("d2_1","d2_2")
  u3 <- c("d3_1","d3_2")
  u4 <- c("d4_1","d4_2")
  u5 <- c("d5_1","d5_2")
  u6 <- c("d6_1","d6_2")
  u7 <- c("d7_1","d7_2")
  u8 <- c("d8_1","d8_2")
  u9 <- c("d9_1","d9_2")
  ua <- c("da_1","da_2")
  ub <- c("db_1","db_2")
  uc <- c("dc_1","dc_2")
  ud <- c("dd_1","dd_2")
  ue <- c("de_1","de_2")
  uf <- c("df_1","df_2")
  ug <- c("dg_1","dg_2")
  uh <- c("dh_1","dh_2")
  ui <- c("di_1","di_2")
  uj <- c("dj_1","dj_2")
  uk <- c("dk_1","dk_2")
  fUels <- c("l", "m", "lo", "up", "s")
  uels <- list(u1,u2,u3,u4,u5,u6,u7,u8,u9,ua,
               ub,uc,ud,ue,uf,ug,uh,ui,uj,uk,fUels)
#  uels <- list(u1,u2,u3,u4,fUels)

  nd <- 20                              # number of dimensions
  ncol <- nd + 2
  nrow <- 2**nd
  step <- nrow / 2
  v <- matrix(0,nrow=nrow,ncol=ncol)
  for (d in 1:nd) {
    print(paste("d = ",d))
    b <- 0
    while (b < nrow) {
      # print(paste("  assigning 1 to",b+(1:step)))
      v[b+1:step,d] <- 1
      b <- b + step
      # print(paste("  assigning 2 to",b+(1:step)))
      v[b+1:step,d] <- 2
      b <- b + step
    }
    step <- step / 2
  }
  v[1:nrow,nd+1] <- 1                  # field=level
  v[1:nrow,ncol] <- 0.75

  ## patch in a marginal: idx = (1,2,1,1,1,***)
  b <- nrow / 4
  irow <- b + 1
  v[irow,nd+1] <- 2                     # field=marginal
  v[irow,ncol] <- 0.5

  ## patch in a lower: idx = (1,1,2,1,1,1,***)
  b <- b / 2
  irow <- b + 1
  v[irow,nd+1] <- 3                     # field=lower
  v[irow,ncol] <- 0

  ## patch in an upper: idx = (1,1,1,2,1,1,1,***)
  b <- b / 2
  irow <- b + 1
  v[irow,nd+1] <- 4                     # field=upper
  v[irow,ncol] <- 0

  ## patch in a scale: idx = (1,1,1,1,2,1,1,1,***)
  b <- b / 2
  irow <- b + 1
  v[irow,nd+1] <- 5                     # field=scale
  v[irow,ncol] <- 100

  vList <- list(name='big',type='variable',val=v,uels=uels,
                  typeCode=GMS_VARTYPE$FREE,ts='20-dim var')
  wgdx (fnOut, vList)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  rc <- system (paste("gdxdiff", fnWant, fnOut, "id=big"))
  if (0 != rc) {
    stop(paste("Bad return from gdxdiff: wanted 0, got",rc))
  } else {
    print ("gdxdiff call succeeded")
  }


  ## full test
  dims <- sapply(uels,length)
  symDim <- length(dims)-1
  step <- 2**symDim
  vf <- array(0, dim=dims, dimnames=uels)
  for (k in 0:(step-1)) {
    vf[k+1   ] <- 0.75
    vf[k+1 + step] <- 0
    vf[k+1 + 2*step] <- -Inf
    vf[k+1 + 3*step] <-  Inf
    vf[k+1 + 4*step] <-  1
  }
  k <- 3
  vf[k] <- 0
  vf[k+step] <- 0.5

  k <- 5
  vf[k] <- 0
  vf[k+2*step] <- 0

  k <- 9
  vf[k] <- 0
  vf[k+3*step] <- 0

  k <- 17
  vf[k] <- 0
  vf[k+4*step] <- 100

  vList <- list(name='big',type='variable',val=vf,form='full',uels=uels,
                typeCode=GMS_VARTYPE$FREE,ts='20-dim var')
  wgdx (fnOut, vList)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  rc <- system (paste("gdxdiff", fnWant, fnOut, "id=big"))
  if (0 != rc) {
    stop(paste("Bad return from gdxdiff: wanted 0, got",rc))
  } else {
    print ("gdxdiff call succeeded")
  }



  print (paste("test of wgdx on", testName, "passed"))
  TRUE   ## all tests passed: return TRUE
},

error = errFunc
)
