\name{woo}
\alias{woo}
\docType{data}
\title{Graded histopathological findings- basophilic tubules}
\description{Histopathological findings as an example the findings of basophilic tubules in the kidneys of male rats (Table 6 in \cite{Woo2007} of a 28-days oral dose toxicity study of nonylphenol}
\usage{data("woo")}
\format{
  A data frame with 40 observations on the following 3 variables.
  \describe{
    \item{\code{dose}}{a factor with levels \code{10mgkg} \code{250mgkg} \code{50mgkg} \code{control}}
    \item{\code{finding}}{a numeric vector}
    \item{\code{Dose}}{a factor with levels \code{control} \code{10mgkg} \code{50mgkg} \code{250mgkg}}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
G. H. Woo, M. Shibutani, T. Ichiki, M. Hamamura, K. Y. Lee, K. Inoue, and M. Hirose.
A repeated 28-day oral dose toxicity study of nonylphenol in rats, based on the
'enhanced oecd test guideline 407' for screening of endocrine-disrupting chemicals.
Archives of Toxicology, 81(2):77{88, February 2007.}

\examples{
data(woo)
## maybe str(woo) ; plot(woo) ...
}
\keyword{datasets}
