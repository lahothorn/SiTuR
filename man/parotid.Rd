\name{parotid}
\alias{parotid}
\docType{data}
\title{Severity findings of hyperplasia in parotid gland (salivary glands)}
\description{Severity findings}
\usage{data("parotid")}
\format{
  A data frame with 90 observations on the following 5 variables.
  \describe{
    \item{\code{X}}{a numeric vector}
    \item{\code{Anino}}{a numeric vector}
    \item{\code{Finding}}{a factor with levels \code{Marked} \code{Mild} \code{Minimal} \code{Moderate} \code{none}}
    \item{\code{Score}}{a numeric vector}
    \item{\code{Group}}{a factor with levels \code{control} \code{low} \code{medium}}
  }
}
\source{National Cancer Institute. Carcinogenesis technical report series no. 142 1979 bioassay
of p-cresidine for possible carcinogenicity nci-cg-tr-142. Technical report, 1979.
}

\examples{
data(parotid)
}
\keyword{datasets}
