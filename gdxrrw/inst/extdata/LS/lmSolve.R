fnData <- "norrisData.gdx"
fnSol <- "norrisSol.gdx"

if (! file_test ('-f', fnData)) {
  stop (paste("FAIL: File", fnData, "does not exist"))
}
lst <- list(name='p',form='full',compress=TRUE)
out <- rgdx(fnData,lst)
p <- out$uels
lst <- list(name='data',form='full',compress=TRUE)
out <- rgdx(fnData,lst)
d <- data.frame(y=out$val[,1],x=out$val[,2])

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


wgdx (fnSol, estlst, selst, tlst, plst, dflst, sigmalst, r2lst,
      resvarlst, rsslst)
