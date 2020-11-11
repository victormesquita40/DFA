\name{secant}
\alias{secant}
\title{
secant method for detection of crossover points
}
\description{
Applies the secant method for detection of crossover points on the log-log curve.
}
\usage{
secant(x,y,npoint,size_fit)
}

\arguments{
  \item{x}{Vector of the decimal logarithm of the boxes sizes.}
  \item{y}{Vector of the decimal logarithm of the DFA calculated in each boxe.}
  \item{npoint}{Number of crossover points calculated on the log-log curve.}
  \item{size_fit}{Number of points of the two semi-curved fitted in the extremes of the log-log curve. }

}

\value{
\item{position}{Position of the crossover point identified by the secant method.}

}
\author{
Victor Barreto Mesquita
}
\examples{
# Example with the data referring to the log fluctuation
#channel curve data calculated for an epileptic subject
#extracted in the Physionet platform.

library(DFA)
data("EEGsignal")
x<-EEGsignal$`log10(boxes)`
y<-EEGsignal$`log10(DFA)`
plot(x,y,xlab="log10(boxes)",ylab="log10(DFA)")

secant(x,y,npoint=2,size_fit=8)

# Example with crossover point fixed in position=20.

library(DFA)
part1 <- seq(1,20)
part2 <- seq(20,1)
y = c(part1,part2)
x<-seq(1,40)
plot(x,y)
secant(x,y,npoint=1,size_fit=8)

}
