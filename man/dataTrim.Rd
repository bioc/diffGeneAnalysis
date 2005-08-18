\name{dataTrim}
\alias{dataTrim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ DataTrim using pseudo winsorization algorithm}
\description{
  DataTrim cleans the data through a pseudo winsorization algorithm.
  First, the mean and SD are calculated.  Then, any values above 2SD
  are trimmed.The mean is recalculated and any values less then 2 SD
  are trimmed.This process of cutting 2 SD above the mean and then
 2 SD below the mean is repeated until no further cuts are possible.
}
\usage{
dataTrim(chipdata)
}

\arguments{
  \item{chipdata}{chipdata is a vector of chipdata from microarray
    chips.}
}

\value{
  Returns a vector of trimmed chipdata representing background
  based on pseudo winsorization algorithm.
  
}
\references{  
Dozmorov I,Centola,M. An associative analysis of gene expression array data.
Bioinformatics.2003 Jan22;19(2):204-11

Knowlton N,Dozmorov I, Centola M. Microarray data Analysis Tool box(MDAT):
for normalization,adjustment and analysis of gene expression data.
Bioonformatics.2004 Dec 12;20(18):3687-90 }

\author{ Choudary L Jagarlamudi }


\examples{
 #see normalize for details
}
\keyword{internal}