\name{FMn}
\alias{FMn}
\docType{data}
\title{Mortality  data from a long-term in vivo bioassay}
\description{Mortailty data}
\usage{data("FMn")}
\format{
  A data frame with 120 observations on the following 8 variables.
  \describe{
    \item{\code{chemical_name}}{a factor with levels \code{Piperonyl butoxide}}
    \item{\code{study_no}}{a factor with levels \code{C02813}}
    \item{\code{Days}}{a numeric vector}
    \item{\code{DOSE_GROUP}}{a factor with levels \code{2500 PPM} \code{5000 PPM} \code{Untreated Control}}
    \item{\code{Treatment}}{a factor with levels \code{Control} \code{D1} \code{D2}}
    \item{\code{Event}}{a logical vector}
    \item{\code{REMOVAL_REASON}}{a factor with levels \code{Missing} \code{Moribund Sacrifice} \code{Natural Death} \code{Scheduled Sacrifice}}
    \item{\code{AnimalSex}}{a factor with levels \code{FemaleMice}}
  }
}

\source{National Toxicology Program. Bioassay of Piperonyl Butoxide For Possible Carcino
genicity (CAS No. 51-06-6 / NCI-CG-TR-120), 1979}
\examples{
data(FMn)
}
\keyword{datasets}
