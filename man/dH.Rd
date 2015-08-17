\name{dH}
\alias{dH}
\docType{data}
\title{Historical controls  for HET-MN assay data}
\description{Historical controls data}
\usage{data("dH")}
\format{
  A data frame with 160 observations on the following 5 variables.
  \describe{
    \item{\code{lab}}{a factor with levels \code{B}}
    \item{\code{run}}{a numeric vector}
    \item{\code{labrun}}{a factor with levels \code{B:1} \code{B:2} \code{B:3} \code{B:4} \code{B:5} \code{B:6} \code{B:7} \code{B:8} \code{B:9} \code{B:10} \code{B:11} \code{B:12} \code{B:13} \code{B:14} \code{B:15} \code{B:16} \code{B:17} \code{B:18} \code{B:19} \code{B:20} \code{B:21} \code{B:22} \code{B:23} \code{B:24}}
    \item{\code{MN}}{a numeric vector}
    \item{\code{ft}}{a numeric vector}
  }
}
\source{A. Kitsche, L. A. Hothorn, and F. Schaarschmidt. The use of historical controls
in estimation simultaneous confidence intervals for comparisons against a concurrent
control. Computational Statistics and Data Analysis, 56(12):3865–3875, 2012}
\examples{
data(dH)
}
\keyword{datasets}
