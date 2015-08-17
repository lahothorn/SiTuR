\name{th}
\alias{th}
\docType{data}
\title{Cell viabilities}
\description{Multi-centre inhibition bioassay running on replicated micro-titer plates: cell viability}
\usage{data("th")}
\format{
  A data frame with 428 observations on the following 4 variables.
  \describe{
    \item{\code{dose}}{a numeric vector}
    \item{\code{response}}{a numeric vector}
    \item{\code{experiment}}{a factor with levels \code{1} \code{2} \code{3} \code{4}}
    \item{\code{center}}{a factor with levels \code{C1} \code{C2} \code{C3} \code{C4}}
  }
}
\source{A. K. Krug, et al. Human embryonic stem cell-derived test systems for developmental neuro-toxicity: a transcriptomics approach. Archives of Toxicology, 87(1):123–143, January 2013}
\references{ESNATS project}
\examples{
data(th)
}
\keyword{datasets}
