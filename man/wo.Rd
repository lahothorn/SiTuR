\name{wo}
\alias{wo}
\docType{data}
\title{Graded histopathological findings for basophilic tubules}
\description{
Graded histopathological findings for basophilic tubules in the kidneys of male rats Table 6 in \cite{Woo2007}
of the 28-days oral dose toxicity study of nonylphenol}
\usage{data("wo")}
\format{
  A data frame with 80 observations on the following 4 variables.
  \describe{
    \item{\code{sex}}{a factor with levels \code{female} \code{male}}
    \item{\code{dose}}{a factor with levels \code{10mgkg} \code{250mgkg} \code{50mgkg} \code{control}}
    \item{\code{animal}}{a numeric vector}
    \item{\code{finding}}{a numeric vector}
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
data(wo)
## maybe str(wo) ; plot(wo) ...
}
\keyword{datasets}
