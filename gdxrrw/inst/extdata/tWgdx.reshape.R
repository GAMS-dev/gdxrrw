### Test wgdx.reshape

tryCatch({

  print ("test wgdx.reshape")
  wgdx ('?')

  print ("all tests for wgdx.reshape passed")
  return (TRUE)
}

, error = function(ex) { print(ex) ; return (FALSE) }
)
