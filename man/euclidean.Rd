\name{euclidean}
\alias{euclidean}
\title{
euclidean method for detection of crossover points

}
\description{
Applies the euclidean method for detection of crossover points on the log-log curve.
}
\usage{
euclidean(x,y,npoint)
}
\arguments{
  \item{x}{Vector of the decimal logarithm of the boxes sizes.}
  \item{y}{Vector of the decimal logarithm of the DFA calculated in each boxe.}
  \item{npoint}{Number of crossover points calculated on the log-log curve.}
}
\value{

\item{position}{Position of the crossover point identified by the euclidean method.}
\item{sugestion_before}{Sugestion for the position of the second crossover point identified by the euclidean method and calculated in the area before the first crossover point.}
\item{sugestion_after}{Sugestion for the position of the second crossover point identified by the euclidean method and calculated in the area after the first crossover point.}
}
\author{
Victor Barreto Mesquita
}

\references{
https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
}

\examples{
# Example with crossover point fixed in position=20.

library(DFA)
data(lrcorrelation)
x<-lrcorrelation$`log10(boxes)`
y<-c(lrcorrelation$`log10(DFA(alpha = 0.1))`[1:20],lrcorrelation$`log10(DFA(alpha = 0.3))`[21:40])
plot(x,y,xlab="log10(boxes)",ylab="log10(DFA)",pch=19)
fit<- lm(y[1:20] ~ x[1:20])
fit2<-lm(y[21:40] ~ x[21:40])
abline(fit,col="blue")
abline(fit2,col="red")
euclidean(x,y,npoint=1)

# Example with crossover point fixed in position=13 and 26.
library(DFA)
data(lrcorrelation)
x<-lrcorrelation$`log10(boxes)`
y<-c(lrcorrelation$`log10(DFA(alpha = 0.2))`[1:13],lrcorrelation$`log10(DFA(alpha = 0.6))`[14:26]
  ,lrcorrelation$`log10(DFA(alpha = 0.9))`[27:40])
plot(x,y,xlab="log10(boxes)",ylab="log10(DFA)",pch=19)
fit<- lm(y[1:13] ~ x[1:13])
fit2<-lm(y[14:26] ~ x[14:26])
fit3<-lm(y[27:40] ~ x[27:40])
abline(fit,col="blue")
abline(fit2,col="red")
abline(fit3,col="brown")
euclidean(x,y,npoint=2)

}
