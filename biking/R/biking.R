
biking_function <- function(love=TRUE) {
  if (love) {
    print('Time to roll!!')
  }
  else {
    print('You can drive the sag wagon.')
  }
}

# typeCode constants for variables
GMS_VARTYPE <- list(UNKNOWN=0L,
                    BINARY=1L,
                    INTEGER=2L,
                    POSITIVE=3L,
                    NEGATIVE=4L,
                    FREE=5L,
                    SOS1=6L,
                    SOS2=7L,
                    SEMICONT=8L,
                    SEMIINT=9L,
                    MAX=10L)
