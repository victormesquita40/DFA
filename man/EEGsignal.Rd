\name{EEGsignal}
\alias{EEGsignal}
\docType{data}
\title{
A single DFA dataframe with the decimal log fluctuation curve.
}
\description{
The data contains the log fluctuation channel curve calculated for an epileptic subject extracted in the Physionet platform.
}
\usage{data("EEGsignal")}
\format{
  A data frame with 91 observations on the following 2 variables.
  \describe{
    \item{\samp{log10(boxes)}}{a numeric vector referring to the decimal logarithm of the boxes sizes.}
    \item{\samp{log10(DFA)}}{a numeric vector referring to the decimal logarithm of the Detrended Fluctuation Analysis (DFA) calculated in each boxe.}
  }
}

\references{
\url{https://physionet.org/content/chbmit/1.0.0/chb01/#files-panel}
}
\examples{
data(EEGsignal)
data("EEGsignal")
x<-EEGsignal$`log10(boxes)`
y<-EEGsignal$`log10(DFA)`
plot(x,y)
}
\keyword{datasets}
