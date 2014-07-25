\name{th}
\alias{th}
\docType{data}
\title{Cell viability data}
\description{
Cell viability data of multi-centre inhibition bioassay of a selected test compounds
on prelicated micro-titer plates (experiment) (from the ESNATS project \cite{Krug2013}).
}
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
data(th)
## maybe str(th) ; plot(th) ...
}
\keyword{datasets}
