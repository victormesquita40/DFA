\name{SSEC2008}
\alias{SSEC2008}
\docType{data}
\title{
Time series referring to the adjusted closing price of the SSE Composite Index (^SSEC) during the period which the United States faced the bear market of 2007\eqn{-}2009.
}
\description{
Univariate vector of time series referring to the adjusted closing price of the SSE Composite Index (^SSEC)
during the period which the United States faced the bear market of 2007\eqn{-}2009, considered the worst bear market this side of the Great Depression. The crash, which unfolded from Oct. 9, 2007 to March 9, 2009, obliterated more than half of the total value of the U.S. stock market. During this period, the S&P 500 lost approximately a half of its value and threatened the very existence of iconic companies from General Motors to Merrill Lynch.

}
\usage{data("SSEC2008")}
\format{
  The format is:
 num [1:332] 5771 5913 5903 6030 6092 ...
}

\source{
Yahoo Finance
}
\references{
\url{https://money.com/bear-market-anniversary/}
}
\examples{
library(DFA)
data("SSEC2008")
}
\keyword{datasets}
