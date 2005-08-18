\name{curveFit}
\alias{curveFit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{CurveFit data to a Gaussian distribution}
\description{
  CurveFit takes a vector of chipdata from microarray slides and
  fits the data to a Gaussian distribution through a non-linear
  least-squares optimization algorithm.The results are graphically
  depicted in a series of histograms. Each histogram represents a
  different initial seed (left to right: 2 bins, 3 bins, 4 bins,
   4.5 bins, 5 bins, and 5.5 bins) that is passed to the curve
   fitting algorithm. The resulting fit for each histogram is
   superimposed with a solid blue line.The user is then able
   to visually select the 'best' fit.
}
\usage{
curveFit(chipdata, plot)
}

\arguments{
  \item{chipdata}{ a vector of chipdata from microarray slides.}
  \item{plot}{plot can take values of 1 or 0. If plot is 1 then 
   the histogram with the curve fit will be shown graphically.}
}

\value{
  an object res which is a list containing the following components.
  res1]: mean of the computed background.
  res[2]: standard deviation of the computed background.
}

\references{  
Dozmorov I,Centola,M. An associative analysis of gene expression array data.
Bioinformatics.2003 Jan22;19(2):204-11

Knowlton N,Dozmorov I, Centola M. Microarray data Analysis Tool box(MDAT):
for normalization,adjustment and analysis of gene expression data.
Bioonformatics.2004 Dec 12;20(18):3687-90 }
\author{Choudary L Jagarlamudi}

\examples{
#see normalize for details.
}
\keyword{internal}
