\name{np}
\alias{np}
\docType{data}
\title{
NPPD-microneucleus assay
%%   ~~ data name/kind ... ~~
}
\description{
NPPD-microneucleus assay with 5-(4-Nitrophenyl)-2,4-pentadien-1-al on the peripherical blood of B6C3F1 mice using the standard NTP protocol (Study ID: A63788)
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("np")}
\format{
  A data frame with 60 observations on the following 5 variables.
  \describe{
    \item{\code{Dose}}{a factor with levels \code{Control} \code{D0.03} \code{D0.1} \code{D0.3} \code{D1} \code{D3}}
    \item{\code{Animal}}{a numeric vector}
    \item{\code{PCE}}{a numeric vector}
    \item{\code{MN}}{a numeric vector}
    \item{\code{MNP}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
http://ntp.niehs.nih.gov
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(np)
## maybe str(np) ; plot(np) ...
}
\keyword{datasets}
