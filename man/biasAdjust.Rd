\name{biasAdjust}
\alias{biasAdjust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ baisAdjust performs bias adjustment of normalized data}
\description{
  biasAdjust takes a normalized dataset and applies a multiplicative
  scalar derived from the data to help account for expression biases.
  These expression biases can come from many sources including dye
  bias,hybridization efficacy,changes in personnel,etc.After bias
 adjustment the data is ready for differential gene analysis.
}
\usage{
biasAdjust(normalized, numSlides)
}

\arguments{
  \item{normalized}{normalized is a matrix of normalized microarray
  data.The first row consists of headers ans the first column
  consists of gene names.}
  \item{numSlides}{numSlides is the total number of chips of the
   microarray dataset.}
}



\value{
  Returns a matrix of bias adjusted normalized data.
}
\references{  
Dozmorov I,Centola,M. An associative analysis of gene expression array data.
Bioinformatics.2003 Jan22;19(2):204-11

Knowlton N,Dozmorov I, Centola M. Microarray data Analysis Tool box(MDAT):
for normalization,adjustment and analysis of gene expression data.
Bioonformatics.2004 Dec 12;20(18):3687-90 }

\note{ 
 biasAdjust takes 2-3 seconds to execute under optimal conditions of
 size of datasets and speed of the machine..Tested on a Pentium IV 1.6Ghz,
 256Mb RAM with 22464 genes and 10 chips it took 12 seconds to execute.
 }

\author{Choudary L Jagarlamudi}


\examples{
 
 #biasAdjust(normalized,7)

}
\keyword{internal}