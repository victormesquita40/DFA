\name{Deltarho}
\alias{Deltarho}
\title{
Delta Amplitude Detrended Cross-Correlation Coefficient (DeltarhoDCCA)
}
\description{
Applies the Detrended Cross-Correlation Coefficient Difference (Deltarho) to nonstationary time series.
}
\usage{
Deltarho(file,file2,file3,file4,scale = 2^(1/8),box_size = 4,m=1)
}

\arguments{
  \item{file}{
Univariate time series (must be a vector or data frame)}
  \item{file2}{
Univariate time series (must be a vector or data frame)}
  \item{file3}{
Univariate time series (must be a vector or data frame)}
  \item{file4}{
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
The Deltarho can be computed in a geometric scale or for different choices of boxes sizes.
}
\value{
\item{boxe}{Size \eqn{n} of the overlapping boxes. }
\item{DFA1}{DFA of the first time series (\code{file}).}
\item{DFA2}{DFA of the second time series  (\code{file2}).}
\item{DFA3}{DFA of the third time series  (\code{file3}).}
\item{DFA4}{DFA of the fourth time series  (\code{file4}).}
\item{DCCA}{Detrended Cross-Correlation function between the first time series (\code{file}) and the second time series (\code{file2}).}
\item{DCCA2}{Detrended Cross-Correlation function between the third time series (\code{file3}) and the fourth time series (\code{file4}).}
\item{rhoDCCA}{Detrended Cross-Correlation Coefficient function, defined as the ratio between the \code{DCCA} and two DFA (\code{DFA1,DFA2}).}
\item{rhoDCCA2}{Detrended Cross-Correlation Coefficient function, defined as the ratio between the \code{DCCA2} and two DFA (\code{DFA3,DFA4}).}
}
\note{
The time series \code{file},\code{file2},\code{file3} and \code{file4} must have the same sample size.
}
\author{
Victor Barreto Mesquita
}

\references{

SILVA, Marcus Fernandes da et al. Quantifying cross-correlation between ibovespa
and brazilian blue-chips: The dcca approach. Physica A: Statistical Mechanics and its
Applications, v. 424,2015.
}
\examples{
#The following examples using the database of financial time series
#collected during the United States bear market of 2007-2009.
\donttest{
library(DFA)
data("NYA2008")
data("IXIC2008")
data("LSE.L2008")
data("SSEC2008")

file = NYA2008
file2= IXIC2008
file3 = LSE.L2008
file4 = SSEC2008

Deltarho(file,file2,file3,file4,scale = 2^(1/8),box_size = c(4,8,16),m=1)
}
\donttest{
# Example with different polynomial fit order.

library(DFA)
data("NYA2008")
data("IXIC2008")
data("LSE.L2008")
data("SSEC2008")

file = NYA2008
file2 = LSE.L2008
file3= IXIC2008
file4 = SSEC2008

Deltarho(file,file2,file3,file4,scale = 2^(1/8),box_size = c(4,8,16),m=2)

}
\donttest{
# Example using different choice of overlapping boxes sizes.

library(DFA)
data("NYA2008")
data("IXIC2008")
data("LSE.L2008")
data("SSEC2008")

file = NYA2008
file2= IXIC2008
file3 = LSE.L2008
file4 = SSEC2008

Deltarho(file,file2,file3,file4,scale = "F",box_size = c(4,8,16),m=1)

}

}
