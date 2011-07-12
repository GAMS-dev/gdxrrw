# In this example we show how a GAMS user can use R to solve a simple
# least-squares model and retrieve the results.
# For a test problem we use the data from the GAMS test model ls01,
# originally taken from the Norris test problem on the NIST web site.
# Here we also test that the results are the same as those given by
# the GAMS LS solver and posted on the NIST site.

# options(list("digits"=10))

fnIn <- "ls01.gdx"
fnSol <- "lsSolu.gdx"

if (! file_test ('-f', fnIn)) {
  stop (paste("FAIL: File", fnIn, "does not exist"))
}
lst <- list(name='p',form='full',compress=TRUE)
data <- rgdx(fnIn,lst)
p <- data$uels
lst <- list(name='i',form='full',compress=TRUE)
data <- rgdx(fnIn,lst)
i <- data$uels
lst <- list(name='data',form='full',compress=TRUE)
data <- rgdx(fnIn,lst)
d <- data.frame(y=data$val[,1],x=data$val[,2])

f <- y ~ x
res <- lm(f, data = d)
est <- as.array(coef(res))
smry <- summary(res)
se <- as.array(smry$coefficients[,'Std. Error'])
df <- res$df.residual
residuals <- res$residuals

estlst <- list(name='estimate', type='parameter', form='full',
               ts='Estimated coefficients', val=est, uels=p)

selst <- list(name='se', type='parameter', form='full',
            ts='Standard errors',
            val=se, uels=p)

tlst <- list(name='tval', type='parameter', form='full',
             ts='t values',
             val=as.array(smry$coefficients[,'t value']), uels=p)

plst <- list(name='pval', type='parameter', form='full',
            ts='p values',
            val=as.array(smry$coefficients[,'Pr(>|t|)']), uels=p)

dflst <- list(name='df', type='parameter', form='full', dim=0,
              ts='Degrees of freedom', val=df)

sigmalst <- list(name='sigma', type='parameter', form='full', dim=0,
                 ts='Standard error',
                 val=smry$sigma)

r2lst <- list(name='r2', type='parameter', form='full', dim=0,
              ts='R Squared',
              val=smry$r.squared)

resvarlst <- list(name='resvar', type='parameter', form='full', dim=0,
                  ts='Residual variance',
                  val=(smry$sigma)^2)

rsslst <- list(name='rss', type='parameter', form='full', dim=0,
               ts='Residual sum of squares',
               val=sum(residuals^2))

residualslst <- list(name='resid', type='parameter', form='full',
                ts='residuals',
                val=as.array(residuals), uels=i)

fittedlst <- list(name='fitted', type='parameter', form='full',
             ts='fitted values for dependent variable',
             val=as.array(res$fitted.values), uels=i)

# set of confidence intervals to write
cnf <- c("90%"=.9, "95%"=.95, "97.5%"=.975, "99%"=.99)
confs <- list(attr(cnf,"names"))
ncnf  <- length(cnf)
bnds  <- list(c("LO", "UP"))
nbnds <- length(bnds[[1]])
confint <- array(0.5,dim=c(ncnf,length(p[[1]]),nbnds))
for (k in 1:ncnf) {
  tstar <- qt((1+cnf[k])/2,df=df)
  confint[k,,1] <- est - tstar * se
  confint[k,,2] <- est + tstar * se
}
conflst <- list(name='confint', type='parameter', form='full',
             ts='confidence intervals',
             val=confint, uels=c(confs,p,bnds))

covlst <- list(name='covar', type='parameter', form='full',
             ts='variance-covariance matrix',
             val=vcov(res), uels=c(p,p))

wgdx (fnSol, estlst, selst, tlst, plst, dflst, sigmalst, r2lst,
      resvarlst, rsslst, residualslst, fittedlst, conflst, covlst)

if (file_test ('-f', fnSol) == TRUE) {
  print (paste("File", fnSol, "was created"))
} else {
  stop (paste("FAIL: File", fnSol, "is not readable"))
}
rc <- system (paste("gdxdiff ls.gdx",fnSol,"releps=5e-12 eps=1e-30 id=estimate,se,df,sigma,r2,resid,fitted,resvar,rss"))
if (0 != rc) {
  stop(paste("Bad return from tight gdxdiff: wanted 0, got",rc))
} else {
  print ("tight gdxdiff call succeeded")
}

rc <- system (paste("gdxdiff ls.gdx",fnSol,"releps=1e-5 eps=1e-30 id=tval,pval,confint,covar"))
if (0 != rc) {
  stop(paste("Bad return from loose gdxdiff: wanted 0, got",rc))
} else {
  print ("loose gdxdiff call succeeded")
}
