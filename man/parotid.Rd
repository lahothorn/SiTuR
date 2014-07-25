\name{parotid}
\alias{parotid}
\docType{data}
\title{Severity findings of hyperplasia in parotid gland} 
\description{
Non-neoplastic lesions in the NTP P-Cresidine
carcinogenicity study  on each 30 male mice the severity findings of hyperplasia
in parotid gland (salivary glands)}
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
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(parotid)
## maybe str(parotid) ; plot(parotid) ...
}
\keyword{datasets}
