\name{he}
\alias{he}
\docType{data}
\title{Cell numbers carrying one, two, and three micronuclei}
\description{Multinomial data}
\usage{data("he")}
\format{
  A data frame with 16 observations on the following 8 variables.
  \describe{
    \item{\code{group}}{a factor with levels \code{control} \code{d0.02} \code{d0.05} \code{d0.10} \code{d0.25} \code{d0.50} \code{d1.00} \code{d2.00}}
    \item{\code{donor}}{a factor with levels \code{a} \code{b}}
    \item{\code{NC}}{a numeric vector}
    \item{\code{one}}{a numeric vector}
    \item{\code{two}}{a numeric vector}
    \item{\code{three}}{a numeric vector}
    \item{\code{total}}{a numeric vector}
    \item{\code{ntotal}}{a numeric vector}
  }
}
\source{J. L. He, W. L. Chen, L. F. Jin, and H. Y. Jin. Comparative evaluation of the in vitro
micronucleus test and the comet assay for the detection of genotoxic effects of x-ray
radiation. Mutation Research-Genetic Toxicology And Environmental Mutagenesis,
469(2):223–231, September 2000}
\examples{
data(he)
}
\keyword{datasets}
