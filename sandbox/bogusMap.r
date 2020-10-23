wgdx('bogus.gdx',
     list(name = "bogusData", type = "parameter", dim = 2L,
	  val = structure(c(1, 1, 2, 2, 1, 2, 1, 3, 2.5, -1.5, 32.5, -1.75), .Dim = 4:3),
	  form = "sparse",
	  uels = list(c("Seattle", "San-Diego"), c("lat", "lng")),
	  domains = c("i", "locHdr"),
	  domInfo = "full")
     )
bb <- rgdx("bogus.gdx", list(name = "bogusData"))
