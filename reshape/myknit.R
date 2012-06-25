tryCatch({

  print ("knit the reshape doc and rebuild the PDF")
  library(knitr)
  knit("reshape.Rnw")
  system("pdflatex reshape.tex")
  system("pdflatex reshape.tex")

##  return (TRUE)
}

, error = function(ex) { print(ex) }
)
