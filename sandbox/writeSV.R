print ("test writing special values with form='sparse' and no filter")
wgdx ('?')

uels <- list(c('dummy',"R-PInf","R-MInf","R-NaN","R-NA","R-Zero","R-denorm"));
sv <- matrix(c(1,2, 1,
              2,2, Inf,
              1,3, 1,
              3,3, 525,
              1,4, 1,
              4,4, 525,
              1,5, 1,
              5,5, 525,
              1,6, 1,
              6,6, 525,
              1,7, 1,
              7,7, 525),
              nrow=12,ncol=3,byrow=TRUE);


print ("first write with defaults");

svList <- list (name='sv', type='parameter', form='sparse', dim=2,
                ts='special values with default squeeze="yes"',
                val=sv, uels=c(uels,uels))

wgdx ("wSV0.gdx", svList)
