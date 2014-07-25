\name{mn}
\alias{mn}
\docType{data}
\title{Micronucleus data}
\description{
In vivo micronucleus assay in mice with 1-phenylethanol
at 24 hours sampling time  \cite{Engelhardt2006}}
\usage{data("mn")}
\format{
  A data frame with 25 observations on the following 3 variables.
  \describe{
    \item{\code{group}}{a factor with levels \code{Control} \code{D188} \code{D375} \code{D750} \code{Positive}}
    \item{\code{animal}}{a numeric vector}
    \item{\code{MN}}{a numeric vector}
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
data(mn)
## maybe str(mn) ; plot(mn) ...
}
\keyword{datasets}
