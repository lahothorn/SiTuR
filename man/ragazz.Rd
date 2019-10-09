\name{ragazz}
\alias{ragazz}
\docType{data}
\title{Behavioral test data}
\description{Behavioral tests on selected pups in a neurotoxicity bioassay on 64 pregnant female rats}
\usage{data("ragazz")}
\format{
  A data frame with 256 observations on the following 5 variables.
  \describe{
    \item{\code{Sex}}{a factor with levels \code{females} \code{males}}
    \item{\code{Litter}}{a numeric vector}
    \item{\code{Dose}}{a factor with levels \code{Control} \code{High} \code{Low} \code{Medium}}
    \item{\code{Pup}}{a numeric vector}
    \item{\code{Olfactory}}{a numeric vector}
  }
}
\source{M. Razzaghi. A hierarchical model for the skew-normal distribution with applica-
tion in developmental neurotoxicology. Communications in Statistics- Theory and
Methods, 43(8):1859–1872, April 2014}
\examples{
data(ragazz)
}
\keyword{datasets}
