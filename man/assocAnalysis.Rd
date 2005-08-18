\name{assocAnalysis}
\alias{assocAnalysis}
\title{ Associative Analysis}
\description{
  assocAnalysis performs the Associative-T method of
  differential gene analysis.
}
\usage{
assocAnalysis(bAdjusted, numSlides, ctrl, exp, sdrefgrp,
pctrl, pexpm,ctrlavjs, expmavjs, ctrlsds, expmsds, rgrplen)
}

\arguments{
\item{bAdjusted}{bAdjusted is a matrix of bias adjusted
 normalized microarray data.}
\item{numSlides}{numSlides represents the total number
 of chips in the microarray experiment.}
\item{ctrl}{ctrl is the number of control chips in the
 microarray experiment.}
\item{exp}{exp is the number of control chips in the
 microarray experiment.}
\item{sdrefgrp}{standard deviation of the computed
 refernce group.}
\item{pctrl}{p values of the control chips on
 performin ttests.}
\item{pexpm}{p values of the experiment chips on
 performin ttests.}
\item{ctrlavjs}{averages of genes across all
 control chips.}
\item{expmavjs}{averages of genes across all
 experiment chips.}
\item{ctrlsds}{standard deviations of genes across all 
control chips.}
\item{expmsds}{standard deviations of genes across all
 experiment chips.}
\item{rgrplen}{total number of genes of the computed
 refernce group.}
}
\details{
AssociativeAnalysis performs the Associative-T method
of differential gene analysis.The results are displayed
in a 10 column matrix as follows

  Column        Representation.

1     Gene Bank Id

2     Average Signal of the Control Chips/Channels.

3     SD of Control Chips/Channels.

4     Probability that a given gene in the Control 
      Chips/Channels belongs or does not belong to
      background.
 
5     Average Signal of the Experimental Chips/Channels.

6     SD of Experimental Chips/Channels.

7     Probability that a given gene in the Experimental
      Chips/Channels belongs or does not belong to background.

8     P-value from a Student T-test.

9     P-value from an Associative T-test.

10    Ratio of mean expression values (Control/Experimental).

11    Group Number.

Group Numbers are defined as follows:.

A1      Expressed above background in both sample types,
        but over-expressed on the Experimental Chips/Channels.

A2      Expressed above background in both sample types, but
        over-expressed on the Control Chips/Channels.

A3      Expressed above background only on the Experimental
        Chips/Channel.

A4      Expressed above background only in the Control
        Chips/Channel.

0       None of the above.}

\value{
  A matrix of 11 columns as described in the details and n 
  rows where n stands for the number of genes in the
   microarray dataset.
  }
\references{
Dozmorov I,Centola,M. An associative analysis of gene
expression array data.Bioinformatics.2003 Jan22;19(2):204-11
 
Knowlton N,Dozmorov I, Centola M. Microarray data Analysis
Tool box(MDAT): for normalization,adjustment and analysis
of gene expression data.Bioonformatics.
2004 Dec 12;20(18):3687-90}

\author{Choudary L Jagarlamudi}


\examples{
#see refGroup for more details
}
\keyword{internal}