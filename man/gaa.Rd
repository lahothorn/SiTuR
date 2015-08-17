\name{gaa}
\alias{gaa}
\docType{data}
\title{Dry matter weights}
\description{Dry matter weights (mg/pot)  for small plants of Galium aparine}
\usage{data("gaa")}
\format{
  A data frame with 100 observations on the following 6 variables.
  \describe{
    \item{\code{dose}}{a numeric vector}
    \item{\code{drymatter}}{a numeric vector}
    \item{\code{treatment}}{a numeric vector}
    \item{\code{Dose}}{a factor with levels \code{0} \code{12} \code{100} \code{200}}
    \item{\code{Treatment}}{a factor with levels \code{C} \code{P} \code{M}}
    \item{\code{IAfac}}{a factor with levels \code{0:C} \code{12:P} \code{12:M} \code{100:P} \code{100:M} \code{200:P} \code{200:M}}
  }
}
\source{Available in package(drc) citing:  Cabanne, F., Gaudry, J. C. and Streibig, J. C. (1999) Influence of alkyl oleates on efficacy of phenmedipham applied as an acetone:water solution on Galium aparine, Weed Research, 39, 57–67}
\examples{
data(gaa)
}
\keyword{datasets}
