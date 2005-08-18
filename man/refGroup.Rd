\name{refGroup}
\alias{refGroup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Reference Group computes a robust estimate of inter-assay
         variability (Standard Error)} 
\description{
     refGroup takes a normalized, bias adjusted dataset and
     computes a robust estimate of inter-assay variability
    (Standard Error).This value is used here to perform T-tests.
     It can also be used in sample size calculations.The
     associative analysis method is then applied to the
     computed refernce group.
}
\usage{
refGroup(biasAdjusted, numSlides, ctrl, exp, pval)
}

\arguments{
  \item{biasAdjusted}{biasAdjusted is a matrix of bias adjusted
        normalized microarray data.The first row consists of
        headers and the first column must consist of Gene Bank
        Id's.}
  \item{numSlides}{numSlides represents the total number of
        chips in a microarray dataset
        including control and experiment.}
  \item{ctrl}{ctrl represents the total number of ctrl chips
        in the microarray.}
  \item{exp}{exp represents the total number of experiment
            chips in the microarray.}
  \item{pval}{pval is the strigency value for computing the
    reference group. 0.05 is the normally suggested value.}
}
\details{
  AssociativeAnalysis performs the Associative-T method of
  differential gene analysis.The user is asked to enter
  values for E and R.E stand for the increase in fold over 
  background and R stands for the ratio of experimental
  chips average over control chips average. The higher
  these values the higher will be the stringency. Example
  dataset used here was run with an E values of 1 and R
  value of 1.5.
 
  The results are displayed in a 10 column matrix as follows

  Column       Representation.
  
  1     Gene Bank ID

  2     Average Signal of the Control Chips/Channels.

  3     SD of Control Chips/Channels.

  4     Probability that a given gene in the Control Chips/Channels
        belongs or does not belong to background.
 
  5     Average Signal of the Experimental Chips/Channels.

  6     SD of Experimental Chips/Channels.

  7     Probability that a given gene in the Experimental
        Chips/Channels belongs or does not belong to background.

  8     P-value from a Student T-test.

  9     P-value from an Associative T-test.

  10    Ratio of mean expression values (Control/Experimental).

  11    Group Number.

Group Numbers are defined as follows:.

A1      Expressed above background in both sample types, but
        over-expressed on the Experimental Chips/Channels.

A2      Expressed above background in both sample types, but
        over-expressed on the Control Chips/Channels.

A3      Expressed above background only on the Experimental
        Chips/Channel.

A4      Expressed above background only in the Control
        Chips/Channel.

0       None of the above.}

\value{
  A matrix of 11 columns as described in the details and n
  rows where n stands for the number of genes in the microarray
  dataset.
  }
\references{  Dozmorov I,Centola,M. An associative analysis of
 gene expression array data.Bioinformatics.
2003 Jan22;19(2):204-11

Knowlton N,Dozmorov I, Centola M. Microarray data Analysis
Tool box(MDAT): for normalization,adjustment and analysis 
of gene expression data.Bioonformatics.
2004 Dec 12;20(18):3687-90}

\note{
    refGroup takes 4-5 seconds to execute under optimal
    conditions of size of datasets and speed of the
    machine.Tested on Pentium IV 1.6Ghz, 256 MB RAM with
    22464 genes and 10 chips it took 25 seconds to execute.
}

\author{Choudary L Jagarlamudi}


\examples{

#refGroup(bAdjusted,7,3,4,0.05)

}
\keyword{internal}