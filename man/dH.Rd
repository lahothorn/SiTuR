\name{dH}
\alias{dH}
\docType{data}
\title{Historic.data HET-MN assay
%%   ~~ data name/kind ... ~~
}
\description{
HET-MN assay historical control data of run B in Hothorn et al. Mut Res. 2013
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
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
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
Hothorn LA., Reisinger, K., Wolf, Th.,  Poth, A. Fieblinger, D., Liebsch, M. Pirow, R. Statistical analysis of the hens egg test for micronucleus induction (HET-MN Assay). Mutation Res (2013) 757:68-78 
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(dH)
## maybe str(dH) ; plot(dH) ...
}
\keyword{datasets}
