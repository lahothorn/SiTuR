\name{ntp558}
\alias{ntp558}
\docType{data}
\title{Two-years carcinogenicity study NTP588}
\description{Two-years carcinogenicity study NTP588}
\usage{data("ntp558")}
\format{
  A data frame with 200 observations on the following 8 variables.
  \describe{
    \item{\samp{Animal No#}}{a numeric vector}
    \item{\code{Days}}{a numeric vector}
    \item{\code{Treatment}}{a factor with levels \code{Control} \code{High} \code{Low} \code{Medium}}
    \item{\code{RemovalReason}}{a factor with levels \code{Accidently Killed} \code{Dosing Accident} \code{Moribund Sacrifice} \code{Natural Death} \code{Terminal Sacrifice}}
    \item{\code{Weeks}}{a numeric vector}
    \item{\code{Tumor}}{a numeric vector}
    \item{\code{TumorFinding}}{a factor with levels \code{Cholangiocarcinoma}}
    \item{\code{Censoring}}{a logical vector}
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
data(ntp558)
## maybe str(ntp558) ; plot(ntp558) ...
}
\keyword{datasets}
