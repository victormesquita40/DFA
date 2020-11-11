\name{AUC}
\alias{AUC}
\title{
Area Under the Curve}
\description{
Applies the Area Under the Curve on the log-log curve.
}
\usage{
AUC(x,data)
}
\arguments{
  \item{x}{Vector of the decimal logarithm of the boxes sizes.}
  \item{data}{A data frame of different decimal logarithm of the DFA calculated in each boxe.}

}
\details{
Compute the Area Under the Curve to a data frame. The method returns the curve with higher AUC.
}
\value{

\item{position}{Position of the DFA curve with higher Area Under the Curve (AUC).}
\item{Area}{ Respective Area Under the Curve (AUC) computed by trapezoidal rule for the channel with higher AUC.}

}
\note{
All of log-log curve contained in the data frame must have the same sample size.
}

\author{
Victor Barreto Mesquita
}

\references{
\url{https://www.khanacademy.org/math/ap-calculus-ab/ab-integration-new/ab-6-2/a/understanding-the-trapezoid-rule}

\url{https://en.wikipedia.org/wiki/Trapezoidal_rule}
}

\examples{

# Example with a data frame with different DFA exponents ranging from short 0.1 to long 0.9.
# The functions returns the channel with higher AUC and its respective area.

library(DFA)
#library(latex2exp) # it is necessary for legend of the plot function

data("lrcorrelation")

#plot(lrcorrelation$`log10(boxes)`,lrcorrelation$`log10(DFA(alpha = 0.9))`
#     ,xlab=TeX("$log_{10}(n)$"),ylab=TeX("$log_{10}F_{DFA}(n)$"),col="black"
#     ,pch=19, ylim= c(-0.8,1.2))
#lines(lrcorrelation$`log10(boxes)`,lrcorrelation$`log10(DFA(alpha = 0.8))`,type="p"
#      ,col="blue", pch=19)
#lines(lrcorrelation$`log10(boxes)`,lrcorrelation$`log10(DFA(alpha = 0.7))`,type="p"
#      ,col="red", pch=19)
#lines(lrcorrelation$`log10(boxes)`,lrcorrelation$`log10(DFA(alpha = 0.6))`,type="p"
#      ,col="green", pch=19)
#lines(lrcorrelation$`log10(boxes)`,lrcorrelation$`log10(DFA(alpha = 0.5))`,type="p"
#      ,col="brown", pch=19)
#lines(lrcorrelation$`log10(boxes)`,lrcorrelation$`log10(DFA(alpha = 0.4))`,type="p"
#      ,col="yellow", pch=19)
#lines(lrcorrelation$`log10(boxes)`,lrcorrelation$`log10(DFA(alpha = 0.3))`,type="p"
#      ,col="orange", pch=19)
#lines(lrcorrelation$`log10(boxes)`,lrcorrelation$`log10(DFA(alpha = 0.2))`,type="p"
#      ,col="pink", pch=19)
#lines(lrcorrelation$`log10(boxes)`,lrcorrelation$`log10(DFA(alpha = 0.1))`,type="p"
#      ,col="magenta", pch=19)

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

x = lrcorrelation$`log10(boxes)`

data = lrcorrelation

AUC(x,data)

}
