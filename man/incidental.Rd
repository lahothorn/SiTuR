\name{incidental}
\alias{incidental}
\docType{data}
\title{Incidental tumors in lung alveolar cell adenoma }
\description{Incidental tumor data}
\usage{data("incidental")}
\format{
  A data frame with 278 observations on the following 3 variables.
  \describe{
    \item{\code{Treatment}}{a factor with levels \code{Control} \code{High} \code{Low} \code{Medium}}
    \item{\code{Tumor}}{a factor with levels \code{no} \code{yes}}
    \item{\code{Time}}{a factor with levels \code{0-52} \code{53-78} \code{79-91} \code{92-104} \code{Final Kill 105-105}}
  }
}
\source{Lung alveolar cell adenoma in male mice. Tumor data provided by Dr. Atiar Rahman
of Division of Biometrics 6, Office of Biostatistics, CDER, FDA. 2007}
\examples{
data(incidental)
}
\keyword{datasets}
