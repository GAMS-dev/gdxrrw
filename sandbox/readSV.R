print ("test reading special values with form='sparse' and no filter")
rgdx('?')

print ("first read with defaults")
sv <- rgdx ('svGAMS',list(name='sv'))

print ("then read with squeeze=FALSE")
sv2 <- rgdx ('svGAMS',list(name='sv'),squeeze=FALSE)
