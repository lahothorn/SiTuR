\name{vor}
\alias{vor}
\docType{data}
\title{LLNA cellularity and lymph node weights}
\description{
Two endpoints cellularity (cell) and lymph node weight (lnw) 
were measured in a control and three doses in BALB mice \cite{Vohr2007}}
\usage{data("vor")}
\format{
  A data frame with 24 observations on the following 5 variables.
  \describe{
    \item{\code{group}}{a factor with levels \code{Control} \code{Dlow} \code{Dmed} \code{Dxtr}}
    \item{\code{cell_NRMI}}{a numeric vector}
    \item{\code{lnw_NRMI}}{a numeric vector}
    \item{\code{cell_BALB}}{a numeric vector}
    \item{\code{lnw_BALB}}{a numeric vector}
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
data(vor)
## maybe str(vor) ; plot(vor) ...
}
\keyword{datasets}
