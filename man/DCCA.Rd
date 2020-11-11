\name{DCCA}
\alias{DCCA}
\title{
Detrended Cross-Correlation Analysis (DCCA)
}
\description{
Applies the Detrended Cross-Correlation Analysis (DCCA) to nonstationary time series.
}
\usage{
DCCA(file,file2,scale = 2^(1/8),box_size = 4,m=1)
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
The Detrended Cross-Correlation Analysis method (DCCA) can be computed in a geometric scale or for different choices of boxes sizes.}
\value{
\item{boxe}{Size \eqn{n} of the overlapping boxes. }
\item{DFA1}{DFA of the first time series (\code{file}).}
\item{DFA2}{DFA of the second time series  (\code{file2}).}
\item{DCCA}{Detrended Cross-Correlation function.}
}
\note{
The time series \code{file} and \code{file2} must have the same sample size.
}
\author{
Victor Barreto Mesquita
}

\references{
N. Xu, P. Shang, S. Kamae
Modeling traffic flow correlation using DFA and DCCA
Nonlinear Dynam., 61 (2010), pp. 207-216

B. Podobnik, D. Horvatic, A. Petersen, H.E. Stanley
Cross-correlations between volume change and price change
PNAS, 106 (52) (2009), pp. 22079-22084

R. Ursilean, A.-M. Lazar
Detrended cross-correlation analysis of biometric signals used in a new authentication method
Electr. Electron. Eng., 1 (2009), pp. 55-58

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

DCCA(file,file2,scale = 2^(1/8),box_size = c(4,8,16),m=1)
}
\donttest{
# Example with different polynomial fit order.

library(DFA)
data("NYA2008")
data("LSE.L2008")
file = NYA2008
file2= LSE.L2008

DCCA(file,file2,scale = 2^(1/8),box_size = c(4,8,16),m=2)
}
\donttest{
# Example using different choice of overlapping boxes sizes.

library(DFA)
data("NYA2008")
data("IXIC2008")
file = NYA2008
file2= IXIC2008

DCCA(file,file2,scale = "F",box_size = c(4,8,16),m=1)
}
}
