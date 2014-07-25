\name{ntp137}
\alias{ntp137}
\docType{data}
\title{Crude tumor rates of fibroadenoma}
\description{NTP137 report on 2 years females mice treated with Diazinon: 
crude tumor rates for fibroadenoma in mammary gland}
\usage{data("ntp137")}
\format{
  A data frame with 125 observations on the following 6 variables.
  \describe{
    \item{\code{AnimalNo}}{a numeric vector}
    \item{\code{Days}}{a numeric vector}
    \item{\code{Treatment}}{a factor with levels \code{Control} \code{Dose100} \code{Dose200}}
    \item{\code{RemovalReason}}{a factor with levels \code{Accidently Killed} \code{Moribund Sacrifice} \code{Natural Death} \code{Scheduled Sacrifice}}
    \item{\code{Weeks}}{a numeric vector}
    \item{\code{Fibroadenoma}}{a numeric vector}
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
data(ntp137)
## maybe str(ntp137) ; plot(ntp137) ...
}
\keyword{datasets}
