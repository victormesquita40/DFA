\name{lrcorrelation}
\alias{lrcorrelation}
\docType{data}
\title{
data frame with log fluctuation channel curve simulated following an ARFIMA process}
\description{
The data contains the data frame with log fluctuation channel curve simulated following an ARFIMA process with different DFA exponents ranging from short 0.1 to long 0.9 .}
\usage{data("lrcorrelation")}
\format{
  A data frame with 40 observations on the following 10 variables.
  \describe{
    \item{\samp{log10(boxes)}}{a numeric vector referring to the decimal logarithm of the boxes sizes.}
    \item{\samp{log10(DFA(alpha = 0.1))}}{a numeric vector referring to the decimal logarithm of the Detrended Fluctuation Analysis (DFA) with DFA exponent equal 0.1 and calculated in each boxe.}
    \item{\samp{log10(DFA(alpha = 0.2))}}{a numeric vector referring to the decimal logarithm of the Detrended Fluctuation Analysis (DFA) with DFA exponent equal 0.2 and calculated in each boxe.}
    \item{\samp{log10(DFA(alpha = 0.3))}}{a numeric vector referring to the decimal logarithm of the Detrended Fluctuation Analysis (DFA) with DFA exponent equal 0.3 and calculated in each boxe.}
    \item{\samp{log10(DFA(alpha = 0.4))}}{a numeric vector referring to the decimal logarithm of the Detrended Fluctuation Analysis (DFA) with DFA exponent equal 0.4 and calculated in each boxe.}
    \item{\samp{log10(DFA(alpha = 0.5))}}{a numeric vector referring to the decimal logarithm of the Detrended Fluctuation Analysis (DFA) with DFA exponent equal 0.5 and calculated in each boxe.}
    \item{\samp{log10(DFA(alpha = 0.6))}}{a numeric vector referring to the decimal logarithm of the Detrended Fluctuation Analysis (DFA) with DFA exponent equal 0.6 and calculated in each boxe.}
    \item{\samp{log10(DFA(alpha = 0.7))}}{a numeric vector referring to the decimal logarithm of the Detrended Fluctuation Analysis (DFA) with DFA exponent equal 0.7 and calculated in each boxe.}
    \item{\samp{log10(DFA(alpha = 0.8))}}{a numeric vector referring to the decimal logarithm of the Detrended Fluctuation Analysis (DFA) with DFA exponent equal 0.8 and calculated in each boxe.}
    \item{\samp{log10(DFA(alpha = 0.9))}}{a numeric vector referring to the decimal logarithm of the Detrended Fluctuation Analysis (DFA) with DFA exponent equal 0.9 and calculated in each boxe.}
  }
}

\examples{
library(DFA)
#library(latex2exp) # it is necessary for legend of the plot function
data(lrcorrelation)
plot(lrcorrelation$`log10(boxes)`,lrcorrelation$`log10(DFA(alpha = 0.9))`
     ,xlab="log10(n)",ylab="log10FDFA(n)",col="black"
     ,pch=19, ylim= c(-0.8,1.2))
lines(lrcorrelation$`log10(boxes)`,lrcorrelation$`log10(DFA(alpha = 0.8))`,type="p"
      ,col="blue", pch=19)
lines(lrcorrelation$`log10(boxes)`,lrcorrelation$`log10(DFA(alpha = 0.7))`,type="p"
      ,col="red", pch=19)
lines(lrcorrelation$`log10(boxes)`,lrcorrelation$`log10(DFA(alpha = 0.6))`,type="p"
      ,col="green", pch=19)
lines(lrcorrelation$`log10(boxes)`,lrcorrelation$`log10(DFA(alpha = 0.5))`,type="p"
      ,col="brown", pch=19)
lines(lrcorrelation$`log10(boxes)`,lrcorrelation$`log10(DFA(alpha = 0.4))`,type="p"
      ,col="yellow", pch=19)
lines(lrcorrelation$`log10(boxes)`,lrcorrelation$`log10(DFA(alpha = 0.3))`,type="p"
      ,col="orange", pch=19)
lines(lrcorrelation$`log10(boxes)`,lrcorrelation$`log10(DFA(alpha = 0.2))`,type="p"
      ,col="pink", pch=19)
lines(lrcorrelation$`log10(boxes)`,lrcorrelation$`log10(DFA(alpha = 0.1))`,type="p"
      ,col="magenta", pch=19)

#legend("bottom", legend=c(TeX("$\\alpha_{DFA} = 0.9$"),TeX("$\\alpha_{DFA} = 0.8$")
#                          ,TeX("$\\alpha_{DFA} = 0.7$"),TeX("$\\alpha_{DFA} = 0.6$")
#                          ,TeX("$\\alpha_{DFA} = 0.5$"),TeX("$\\alpha_{DFA} = 0.4$")
#                          ,TeX("$\\alpha_{DFA} = 0.3$"),TeX("$\\alpha_{DFA} = 0.2$")
#                          ,TeX("$\\alpha_{DFA} = 0.1$"))
#       , col=c("black","blue","red","green","brown","yellow","orange","pink","magenta")
#       , pch=c(19,19,19,19,19,19,19,19,19)
#       , cex = 0.55
#       , ncol = 5
#)

}
\keyword{datasets}
