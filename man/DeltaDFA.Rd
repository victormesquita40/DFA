\name{DeltaDFA}
\alias{DeltaDFA}
\title{
log-amplitude Detrended Fluctuation Analysis (DeltaDFA)
}
\description{
Applies the log-amplitude Detrended Fluctuation Analysis (DFA) to nonstationary time series.
}
\usage{
DeltaDFA(file,file2,scale = 2^(1/8),box_size = 4,m=1)
}

\arguments{
  \item{file}{
Univariate time series (must be a vector or data frame)}
  \item{file2}{
Univariate time series (must be a vector or data frame)}
  \item{scale}{
Specifies the ratio between successive box sizes (by default \code{scale = 2^(1/8)})}
  \item{box_size}{
Vector of box sizes (must be used in conjunction with \code{scale = "F"})
}
\item{m}{
An integer of the polynomial order for the detrending (by default \code{m=1}).}
}

\details{
The DFA log-amplitude fluctuation can be computed in a geometric scale or for different choices of boxes sizes.
}
\value{
\item{boxe}{Size \eqn{n} of the overlapping boxes. }
\item{DeltaDFA}{log-amplitude Detrended Fluctuation function defined as the difference between the DFA decimal logarithmic of the first time series (\code{file}) and the DFA decimal logarithmic of the second time series (\code{file2})}
}
\note{
The time series \code{file} and \code{file2} must have the same sample size.
}

\author{
Victor Barreto Mesquita
}

\references{
 G.  F.  Zebende,  F.  M.  Oliveira  Filho,  J.  A.  L.  Cruz,  Auto-correlationin  the  motor/imaginary  human  eeg  signals:   A  vision  about  the  fdfafluctuations, PloS one 12 (9) (2017).

  F. Oliveira Filho, J. L. Cruz, G. Zebende, Analysis of the eeg bio-signalsduring the reading task by dfa method, Physica A: Statistical Mechanicsand its Applications 525 (2019) 664-671.

S. R. Hirekhan, R. R. Manthalkar, The detrended fluctuation and cross-correlation  analysis  of  eeg  signals,  International  Journal  of  IntelligentSystems Design and Computing 2 (2) (2018) .

}
\examples{
#The following examples using the database of financial time series
#collected during the United States bear market of 2007-2009.
\donttest{
library(DFA)
data("NYA2008")
data("IXIC2008")
file = NYA2008
file2= IXIC2008

DeltaDFA(file,file2,scale = 2^(1/8),box_size = c(4,8,16),m=1)
}
\donttest{
# Example with different polynomial fit order.

library(DFA)
data("NYA2008")
data("LSE.L2008")
file = NYA2008
file2= LSE.L2008

DeltaDFA(file,file2,scale = 2^(1/8),box_size = c(4,8,16),m=2)
}
\donttest{
# Example using differente choice of overlapping boxes sizes.

library(DFA)
data("NYA2008")
data("IXIC2008")
file = NYA2008
file2= IXIC2008

DeltaDFA(file,file2,scale = "F",box_size = c(4,8,16),m=1)
}
}
