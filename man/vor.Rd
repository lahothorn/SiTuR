\name{vor}
\alias{vor}
\docType{data}
\title{Local lymph node assay data}
\description{Cellularity  and lymph node weight in two mice strains}
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
\source{
Statistical evaluation of the Local Lymph Node Assay
By: Hothorn, Ludwig A.; Vohr, Hans-Werner
Reg. Toxicol. Pharmacol.  Volume: 56   Issue: 3   Pages: 352-356   Published: APR 2010}
\examples{
data(vor)
}
\keyword{datasets}
