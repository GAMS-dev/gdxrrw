To reproduce the .pdf output from Renger using sweave, start R and do

Sweave("gdxrrw.rnw")

This will generate a file gdxrrw.tex.  On Windows, hit this one with
MiKTeX to generate the PDF:

pdflatex gdxrrw.tex


To generate the knitr version of the PDF, do:

library(knitr)
knit("gdxDataFrame.Rnw")

This will generate a file gdxDataFrame.tex.  On Windows, hit this one with
MiKTeX to generate the PDF:

pdflatex gdxDataFrame.tex
