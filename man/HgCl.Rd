\name{HgCl}
\alias{HgCl}
\docType{data}
\title{NTP raw data: Mercuric Chloride Bioassay}
\description{
National Toxicology Program. Toxicology and Carcinogenesis Studies of Mercuric Chloride
(CAS No. 7487-94-7) in F344 Rats and B6C3F1 Mice (Gavage Studies). Technical
Report 408, U.S. Department of Health and Human Services, Research Triangle Park,
NC, February 1993. URL http://ntp.niehs.nih.gov/ntp/htdocs/lt_rpts/tr408.
pdf}
\usage{data("HgCl")}
\format{
  A data frame with 180 observations on the following 5 variables.
  \describe{
    \item{\code{Animal}}{a numeric vector}
    \item{\code{Cage}}{a numeric vector}
    \item{\code{Dose}}{a factor with levels \code{2.5 mg/kg} \code{5 mg/kg} \code{Vehicle Control}}
    \item{\code{Week.65}}{a numeric vector}
    \item{\code{Week.105}}{a numeric vector}
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
data(HgCl)
## maybe str(HgCl) ; plot(HgCl) ...
}
\keyword{datasets}
