\name{np}
\alias{np}
\docType{data}
\title{No. of micronuclei and polychromatic erythrocytes per animal in vivo assay}
\description{No. of MN/ PCE}
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
\source{
National Toxicology Program. Micronucleus Assay (A63788) of 5-(4-Nitrophenyl)-2,4-
pentadien-1-al (NPPD) on the peripheral blood of B6C3F1 mice - 2608482.}
\examples{
data(np)
}
\keyword{datasets}
