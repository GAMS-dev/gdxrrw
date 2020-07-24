wgdx('duplicate.gdx',
     list(name = "ilocData", type = "parameter", dim = 2L,
	  val = structure(c(1, 1, 2, 2, 1, 2, 1, 1, 47.608013, -122.335167, 32.715736, -117.161087), .Dim = 4:3),
	  form = "sparse",
	  uels = list(c("Seattle", "San-Diego"), c("lat", "lng")),
	  domains = c("i", "locHdr"),
	  domInfo = "full")
     )
dd <- rgdx("duplicate.gdx", list(name = "ilocData"))$val
