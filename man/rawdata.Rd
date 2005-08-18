\name{rawdata}
\alias{rawdata}
\docType{data}
\title{Micro array dataset}
\description{
Microarray dataset consisting of 7 chips.3 control and 4
experiment with 2382 genes.The first column consists of
gene names, hence the dataset has 8 columns in total.
}
\usage{data(rawdata)}

\format{
  A data frame with 2382 observations on the following 11 variables.
  \describe{
\item{GeneID}{Gene IDS or Gene names}
\item{c1}{a numeric vector of gene intensities for control chip 1}
\item{c2}{a numeric vector of gene intensities for control chip 2}
\item{c3}{a numeric vectorof gene intensities for control chip 3}
\item{e1}{a numeric vector of gene intensities for experiment chip 1}
\item{e2}{a numeric vector of gene intensities for experiment chip 2}
\item{e3}{a numeric vector of gene intensities for experiment chip 3}
\item{e4}{a numeric vector of gene intensities for experiment chip 4}
  }
}

\source{
Dozmorov I,Centola,M. An associative analysis of gene expression array data.
Bioinformatics.2003 Jan22;19(2):204-11

Knowlton N,Dozmorov I, Centola M. Microarray data Analysis Tool box(MDAT):
for normalization,adjustment and analysis of gene expression data.
Bioonformatics.2004 Dec 12;20(18):3687-90
}

\examples{
data(rawdata)
}
\keyword{internal}
