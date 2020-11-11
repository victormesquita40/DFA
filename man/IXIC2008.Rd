\name{IXIC2008}
\alias{IXIC2008}
\docType{data}
\title{
Time series referring to the adjusted closing price of the NASDAQ Composite (^IXIC) during the United States bear market of 2007\eqn{-}2009
}
\description{
Univariate vector of time series referring to the adjusted closing price of the NASDAQ Composite (^IXIC)
during the United States bear market of 2007\eqn{-}2009, considered the worst bear market this side of the Great Depression. The crash, which unfolded from Oct. 9, 2007 to March 9, 2009, obliterated more than half of the total value of the U.S. stock market. During this period, the S&P 500 lost approximately a half of its value and threatened the very existence of iconic companies from General Motors to Merrill Lynch.

}
\usage{data("IXIC2008")}
\format{
  The format is:
 num [1:332] 2811 2772 2805 2780 2763 ...
}

\source{
Yahoo Finance
}
\references{
\url{https://money.com/bear-market-anniversary/}
}
\examples{
library(DFA)
data("IXIC2008")
}
\keyword{datasets}
