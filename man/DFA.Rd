\name{DFA}
\alias{DFA}
\title{
Detrended Fluctuation Analysis (DFA)
}
\description{
Applies the Detrended Fluctuation Analysis (DFA) to nonstationary time series.
}
\usage{
DFA(file,scale = 2^(1/8),box_size = 4,m=1)
}

\arguments{
  \item{file}{
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
The DFA fluctuation can be computed in a geometric scale or for different choices of boxes sizes.
}
\value{
\item{boxe}{Size \eqn{n} of the overlapping boxes. }
\item{DFA}{Detrended Fluctuation function.}
}
\note{
The time series \code{file} and \code{file2} must have the same sample size.
}
\author{
Victor Barreto Mesquita
}

\references{
C.-K. Peng, S.V. Buldyrev, S. Havlin, M. Simons, H.E. Stanley, A.L. Goldberger
Phys. Rev. E, 49 (1994), p. 1685

H.E. Stanley, L.A.N. Amaral, A.L. Goldberger, S. Havlin, P.Ch. Ivanov, C.-K. Peng
Physica A, 270 (1999), p. 309

P.C. Ivanov, A. Bunde, L.A.N. Amaral, S. Havlin, J. Fritsch-Yelle, R.M. Baevsky, H.E. Stanley, A.L. Goldberger
Europhys. Lett., 48 (1999), p. 594

P. Talkner, R.O. Weber
Phys. Rev. E, 62 (2000), p. 150

M. Ausloos, K. Ivanova
Physica A, 286 (2000), p. 353

H.E. Hurst, R.P. Black, Y.M. Simaika
Long-Term Storage, An Experimental Study, Constable, London (1965)
}

\examples{
#The following examples using the database of financial time series
#collected during the United States bear market of 2007-2009.
\donttest{
library(DFA)
data("NYA2008")
file = NYA2008

DFA(file,scale = 2^(1/8),box_size = c(4,8,16),m=1)
}
\donttest{
# Example with different polynomial fit order.

library(DFA)
data("LSE.L2008")
file = LSE.L2008

DFA(file,scale = 2^(1/8),box_size = c(4,8,16),m=2)
}
\donttest{
# Example using different choice of overlapping boxes sizes.

library(DFA)
data("NYA2008")
file = NYA2008

DFA(file,scale = "F",box_size = c(4,8,16),m=1)
}
}
