\name{normalize}
\alias{normalize}

\title{Normalization of microarray data}
\description{
Normalization of data utilizing information obtained from
background fluoresence.Background fluoresce intensity values
are used to determine a Gaussian distribution of lowly
expressed genes,yielding the background
estimates(mean and standard deviation).
}
\usage{
normalize(rawdata, numSlides,ctrl,expm,ctrlbg,expmbg)
}

\arguments{
\item{rawdata}{rawdata is matrix of microarray data.The first column consists
        of gene Names and the first row consists of headers.}
\item{numSlides}{numSlides represents the total number of chips/slides in the
        microarray dataset including control and experiment.Control slides are
        always followed by experiment slides from left to right
       in the matrix.}
\item{ctrl}{ctrl represents the total number of control chips in the
             microarray dataset.}
\item{expm}{expm represents the total number of experiment chips in
              the microarray dataset.}
\item{ctrlbg}{ctrlbg represents the percent of data to pick for background
                computation of the control chips.30 percent is the default.}
\item{expmbg}{expmbg represents the percent of data to pick for background
              computation of the experiment chips.30 percent is the default.}
}
\details{
     The normalization algorithm trims the data based on innitial emperical
     estimates of the mean and standard deviation.All data beyond +/-2SD of
     the mean are cut iteratively.This procedure is repeated until no more
     cuts can be made.The trimmed data is then subjected to a non linear
     curve fitting procedure. The user is presented with six different
     pictures obtained using bars 2,3,4,4.5,5,and5.5 as mean. The user is
     given the freedom to select the best visual estimate of background.
     The user selected parameters are used to perform a z-Trnasformation
     on the data.The percent of data selected to compute background depends
     on the data obtained.The default is 30 percent.A normal distributed
     histogram should confirm that, else the user is allowed to pick a 
     percent and make changes until the user sees a normal distributed
     histogram.Upon running normalize the user is presented with a set
     of 6 histograms. If the user is not happy with the default 30 percent,
     the user should go ahead and select a mean and confirm curvefit,then
     select 'no' to confirm histogram distribution.The user will be presented
     with a new set of 6 histograms. This process is repeated until the user
     selects the best Histogram distribution.This process is repeated for
     each individual chip. 
}
\value{
  A matrix of normalized values of rawdata
  
}

\references{  
Dozmorov I,Centola,M. An associative analysis of gene expression array data.
Bioinformatics.2003 Jan22;19(2):204-11

Knowlton N,Dozmorov I, Centola M. Microarray data Analysis Tool box(MDAT):
for normalization,adjustment and analysis of gene expression data.
Bioonformatics.2004 Dec 12;20(18):3687-90 }
\author{ Choudary L Jagarlamudi}

\examples{

#rawdata is loaded in the package. Run example as follows:
#Read the description file for best results.
#data(rawdata)
#normalize(rawdata,7,3,4,0.15,0.60)

}

\keyword{internal}
