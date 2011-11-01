print ("test writing special values with form='sparse' and no filter")
wgdx ('?')

uels <- list(c('dummy',"R-PInf","R-MInf","R-NaN","R-NA","R-Zero","R-denorm"));
sv <- matrix(c(1,2, 1,
              2,2, Inf,
              1,3, 1,
              3,3, -Inf,
              1,4, 1,
              4,4, NaN,
              1,5, 1,
              5,5, NA,
              1,6, 1,
              6,6, 0,
              1,7, 1,
              7,7, 1e-320),
              nrow=12,ncol=3,byrow=TRUE);


svList <- list (name='sv', type='parameter', form='sparse', dim=2,
                ts='special values with default squeeze="yes"',
                val=sv, uels=c(uels,uels))

print ("first write with defaults");

wgdx ("wSV0.gdx", svList)

wgdx ("wSV1.gdx", svList, squeeze=FALSE)

wgdx ("wSV2.gdx", svList, squeeze='Eps')
