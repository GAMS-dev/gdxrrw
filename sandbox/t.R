fnIn <- "ls01.gdx"
fnSol <- "lsSolu.gdx"

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

ev <- as.array(coef(res))
est <- list(name='estimate', type='parameter', form='full',
            ts='Estimated coefficients', val=ev, uels=p)

selst <- list(name='se', type='parameter', form='full',
            ts='Standard errors',
            val=as.array(summary(res)$coefficients[,'Std. Error']), uels=p)

tlst <- list(name='tval', type='parameter', form='full',
             ts='t values',
             val=as.array(summary(res)$coefficients[,'t value']), uels=p)

plst <- list(name='pval', type='parameter', form='full',
            ts='p values',
            val=as.array(summary(res)$coefficients[,'Pr(>|t|)']), uels=p)

dflst <- list(name='df', type='parameter', form='full', dim=0,
              ts='Degrees of freedom',
              val=summary(res)$df[2])

sigmalst <- list(name='sigma', type='parameter', form='full', dim=0,
                 ts='Standard error',
                 val=summary(res)$sigma)

r2lst <- list(name='r2', type='parameter', form='full', dim=0,
              ts='R Squared',
              val=summary(res)$r.squared)

residualslst <- list(name='resid', type='parameter', form='full',
                ts='residuals',
                val=as.array(res$residuals), uels=i)

fittedlst <- list(name='fitted', type='parameter', form='full',
             ts='fitted values for dependent variable',
             val=as.array(res$fitted.values), uels=i)

wgdx (fnSol, est, selst, tlst, plst, dflst, sigmalst, r2lst,
      residualslst, fittedlst)
# still to do:
#  confint: must compute??
#  covar: ???
#  resvar
#  rss

if (file_test ('-f', fnSol) == TRUE) {
  print (paste("File", fnSol, "was created"))
} else {
  stop (paste("FAIL: File", fnSol, "is not readable"))
}
rc <- system (paste("gdxdiff ls.gdx",fnSol,"releps=5e-12 eps=1e-30 id=estimate,se,df,sigma,r2,resid,fitted"))
if (0 != rc) {
  stop(paste("Bad return from tight gdxdiff: wanted 0, got",rc))
} else {
  print ("tight gdxdiff call succeeded")
}

rc <- system (paste("gdxdiff ls.gdx",fnSol,"releps=1e-5 eps=1e-30 id=tval,pval"))
if (0 != rc) {
  stop(paste("Bad return from loose gdxdiff: wanted 0, got",rc))
} else {
  print ("loose gdxdiff call succeeded")
}
