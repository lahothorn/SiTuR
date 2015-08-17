\name{ntp558}
\alias{ntp558}
\docType{data}
\title{Fatal tumors in liver cholangiocarcinoma}
\description{Fatal tumor}
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
\source{National Toxicology Program. Toxicology and Carcinogenesis Studies of 3,3,4,4-
Tetrachloroazobenzene. TR-558 in Sprague-Dawley rats and B6C3F1 mice, 2010}
\examples{data(ntp558)}
\keyword{datasets}
