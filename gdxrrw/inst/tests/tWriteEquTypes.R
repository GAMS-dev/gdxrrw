## ## test wgdx with form='sparse', 1-d equations, write different types
## gdxdump or gdxdiff do not really do what we want here, so just have
## to read from the generated GDX and the target GDX and compare the results

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

testName <- 'all types of equation writes form=sparse'

errFunc <- function(ex) {
  print (paste0("test of wgdx on ",testName,": FAILED"))
  print(ex)
  FALSE
} # errFunc

tryCatch({
  print (paste("testing wgdx on", testName))
  wgdx('?')
  fnOut <- "tmp.gdx"
  fnWant <- "tEquTypes.gdx"
  if (! file_test ('-f', fnWant)) {
    stop (paste("FAIL: File-to-duplicate", fnWant, "does not exist"))
  }

  ## all writes should use the same uels
  iUels <- paste0("i","1":"10")
  fUels <- c("l", "m", "lo", "up", "s")
  uels <- list(iUels,fUels)

  ## write =E= equation to GDX, compare with fnWant version
  val0 <- matrix(0,nrow=5,ncol=3)
  for (i in 1:5) {
    val0[i,1] <- i
    val0[i,2] <- 2                      # only marginals are set non-default
  }
  val0[c(1,3,5),3] <- -1
  val0[c(2,4),3] <- 0
  vE <- list(name='equ_e_0',type='equation',val=val0,uels=uels,
             typeCode=GMS_EQUTYPE$E,ts='=e= equation')
  wgdx (fnOut, vE)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  eWant <- rgdx(fnWant,list(name='equ_e_0',form='sparse',field='all',ts=TRUE))
  eWrot <- rgdx(fnOut, list(name='equ_e_0',form='sparse',field='all',ts=TRUE))
  if (identical(eWrot$domains[1],"*"))
    eWrot$domains[1] <- eWant$domains[1]
  if (! identical(eWant,eWrot)) {
    print(all.equal(eWant,eWrot))
    stop ("FAIL: for equ_e_0, eWant and eWrot do not agree")
  }

  ## write =G= equation to GDX, compare with fnWant version
  val1 <- matrix(0,nrow=12,ncol=3)
  b <- 0                                # set base
  for (i in 1:5) {
    val1[b+i,1] <- i
    val1[b+i,2] <- 3
    val1[b+i,3] <- 2                    # equ_g_1.lo = 2
  }
  b <- b + 5
  for (i in 1:5) {
    val1[b+i,1] <- i
    val1[b+i,2] <- 1
    val1[b+i,3] <- 8                    # equ_g_1.l = 8
  }
  val1[b+c(2,4),3] <- 2                 # equ_g_1.l = 2 for some rows
  b <- b + 5
  val1[b+1,1] <- 2
  val1[b+2,1] <- 4
  val1[b+(1:2),2] <- 2
  val1[b+(1:2),3] <- 1                  # equ_g.m('i2','i4') = 1
  vE <- list(name='equ_g_1',type='equation',val=val1,uels=uels,
                   typeCode=GMS_EQUTYPE$G,ts='=g= equation')
  wgdx (fnOut, vE)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  eWant <- rgdx(fnWant,list(name='equ_g_1',form='sparse',field='all',ts=TRUE))
  eWrot <- rgdx(fnOut, list(name='equ_g_1',form='sparse',field='all',ts=TRUE))
  if (identical(eWrot$domains[1],"*"))
    eWrot$domains[1] <- eWant$domains[1]
  if (! identical(eWant,eWrot)) {
    print(all.equal(eWant,eWrot))
    stop ("FAIL: for equ_g_1, eWant and eWrot do not agree")
  }

  ## write =L= equation to GDX, compare with fnWant version
  val2 <- matrix(0,nrow=13,ncol=3)
  b <- 0                                # set base
  val2[b+(1:5),1] <- (1:5)
  val2[b+(1:5),2] <- 4
  val2[b+(1:5),3] <- 8                  # equ_g_1(i1..i5).up = 8
  b <- b + 5
  val2[b+(1:5),1] <- (1:5)
  val2[b+(1:5),2] <- 1
  val2[b+c(1,3,5),3] <- 8               # equ_g_1(i1,i3,i5).L = 8
  val2[b+c(2,4)  ,3] <- 2               # equ_g_1(i2,i4).L = 8
  b <- b + 5
  val2[b+(1:3),1] <- c(1,3,5)
  val2[b+(1:3),2] <- 2
  val2[b+(1:3),3] <- -1                 # equ_g_1(i1,i3,i5).m = -1
  vL <- list(name='equ_l_2',type='equation',val=val2,uels=uels,
                    typeCode=GMS_EQUTYPE$L,ts='=l= equation')
  wgdx (fnOut, vL)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  eWant <- rgdx(fnWant,list(name='equ_l_2',form='sparse',field='all',ts=TRUE))
  eWrot <- rgdx(fnOut, list(name='equ_l_2',form='sparse',field='all',ts=TRUE))
  if (identical(eWrot$domains[1],"*"))
    eWrot$domains[1] <- eWant$domains[1]
  if (! identical(eWant,eWrot)) {
    print(all.equal(eWant,eWrot))
    stop ("FAIL: for equ_l_2, eWant and eWrot do not agree")
  }

  ## write =N= equation to GDX, compare with fnWant version
  val3 <- matrix(0,nrow=5,ncol=3)
  val3[(1:5),1] <- (1:5)
  val3[(1:5),2] <- 1
  val3[(1:5),3] <- c(56,2,56,2,56)
  vN <- list(name='equ_n_3',type='equation',val=val3,uels=uels,
                    typeCode=GMS_EQUTYPE$N,ts='=n= equation')
  wgdx (fnOut, vN)
  if (file_test ('-f', fnOut) == TRUE) {
    print (paste("File", fnOut, "was created"))
  } else {
    stop (paste("FAIL: File", fnOut, "is not readable"))
  }
  eWant <- rgdx(fnWant,list(name='equ_n_3',form='sparse',field='all',ts=TRUE))
  eWrot <- rgdx(fnOut, list(name='equ_n_3',form='sparse',field='all',ts=TRUE))
  if (identical(eWrot$domains[1],"*"))
    eWrot$domains[1] <- eWant$domains[1]
  if (! identical(eWant,eWrot)) {
    print(all.equal(eWant,eWrot))
    stop ("FAIL: for equ_n_3, eWant and eWrot do not agree")
  }


  print (paste0("test of wgdx on ", testName, ": PASSED"))
  invisible(TRUE)   ## all tests passed: return TRUE
},

error = errFunc
)
