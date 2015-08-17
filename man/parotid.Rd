\name{parotid}
\alias{parotid}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
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
