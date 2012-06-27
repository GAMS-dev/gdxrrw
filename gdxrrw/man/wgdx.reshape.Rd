\name{wgdx.reshape}
\title{Reshape R data to prepare it for writing to GDX}
\description{
  Reshape the input data frame (assumed to be in in "wide" form) into a "long" form
  suitable for passing to \code{wgdx.lst} or \code{wgdx.df}.
}
\usage{
  wgdx.reshape(inDF, symDim, symName=NULL, tName="time",
               gdxName=NULL, setToo=TRUE, order-NULL,
               setNames=NULL)
}
\arguments{
  \item{inDF}{data frame in "wide" form}
  \item{symDim}{dimension of the output GDX symbol}
  \item{symName}{name of the output GDX symbol}
  \item{tName}{index set name for the new index position created by reshaping}
  \item{gdxName}{name of the GDX file to write}
  \item{setsToo}{if TRUE, extract the index sets defined by \code{inDF}}
}
