### run through all the tests

tests <- c("tReadSparse1", "tReadFull1",
           "tRead.set", "tRead.param",
           "tReadSV",
           "tWriteSparse1", "tWriteSparse2", "tWriteFull1", "tWriteFull2",
           "tWrap",
           "tLS")
for (t in tests) {
  print (paste("Starting test", t))
  rc <- source (paste(t,".R",sep=""))
  if (rc$value) {
    print (paste("Test", t, "result: PASS"))
  } else {
    print (paste("Test", t, "result: FAIL"))
  }
  ans <- readline("Hit enter to continue ")
  print ("")
}
