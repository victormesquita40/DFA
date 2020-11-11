\name{rhoDCCA}
\alias{rhoDCCA}
\title{
Detrended Cross-Correlation Coefficient (rhoDCCA)
}
\description{
Applies the Detrended Cross-Correlation Coefficient (rhoDCCA) to nonstationary time series.
}
\usage{
rhoDCCA(file,file2,scale = 2^(1/8),box_size = 4,m=1)
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
The Detrended Cross-Correlation Coefficient (rhoDCCA) can be computed in a geometric scale or for different choices of boxes sizes.
}
\value{
\item{boxe}{Size \eqn{n} of the overlapping boxes. }
\item{DFA1}{DFA of the first time series (\code{file}).}
\item{DFA2}{DFA of the second time series  (\code{file2}).}
\item{DCCA}{Detrended Cross-Correlation function.}
\item{rhoDCCA}{Detrended Cross-Correlation Coefficient function, defined as the ratio between the \code{DCCA} and two DFA (\code{DFA1,DFA2}).}
}
\note{
The time series \code{file} and \code{file2} must have the same sample size.
}
\author{
Victor Barreto Mesquita
}
\references{

Zebende G.F.
DCCA cross-correlation coefficient: Quantifying level of cross-correlation
Physica A, 390 (4) (2011), pp. 614-618

Vassoler R.T., Zebende G.F.
DCCA cross-correlation coefficient apply in time series of air temperature and air relative humidity
Physica A, 391 (7) (2012), pp. 2438-2443

Guedes E.F., Zebende G.F., da Cunha Lima I.C.
Quantificacao dos Efeitos do Cambio na Producao da Industria de Transformacao Baiana: uma abordagem via coeficiente de correlacao cruzada rho dcca
Conjuntura & Planejamento, 1 (192) (2017), pp. 75-89

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

rhoDCCA(file,file2,scale = 2^(1/8),box_size = c(4,8,16),m=1)
}
\donttest{
# Example with different polynomial fit order.

library(DFA)
data("NYA2008")
data("LSE.L2008")
file = NYA2008
file2= LSE.L2008

rhoDCCA(file,file2,scale = 2^(1/8),box_size = c(4,8,16),m=2)
}
\donttest{
# Example using different choice of overlapping boxes sizes.

library(DFA)
data("NYA2008")
data("IXIC2008")
file = NYA2008
file2= IXIC2008

rhoDCCA(file,file2,scale = "F",box_size = c(4,8,16),m=1)

}

}

