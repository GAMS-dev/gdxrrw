print ("test reading special values with form='sparse' and no filter")
rgdx('?')

print ("first read with defaults")
sv <- rgdx ('svGAMS',list(name='sv'))

print ("second read with squeeze=FALSE")
sv2 <- rgdx ('svGAMS',list(name='sv'),squeeze=FALSE)

print ("third read as data frame with default squeeze")
svdf0 <- rgdx.param ('svGAMS', 'sv', ts=TRUE)

print ("fourth read as data frame with squeeze=FALSE")
svdf1 <- rgdx.param ('svGAMS', 'sv', ts=TRUE, squeeze=FALSE)
