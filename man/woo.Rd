\name{woo}
\alias{woo}
\docType{data}
\title{Graded histopathological findings- basophilic tubules}
\description{Graded histopathological findingsof basophilic tubules in the kidneys of male rats (Table 6 in \cite{Woo2007} of the 28-days oral dose toxicity study of nonylphenol}
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
}

\references{
@Article{Woo2007,
  Title                    = {A repeated 28-day oral dose toxicity study of nonylphenol in rats, based on the 'Enhanced OECD Test Guideline 407' for screening of endocrine-disrupting chemicals},
  Author                   = {Woo, G. H. and Shibutani, M. and Ichiki, T. and Hamamura, M. and Lee, K. Y. and Inoue, K. and Hirose, M.},
  Journal                  = {Archives of Toxicology},
  Year                     = {2007},

  Month                    = feb,
  Number                   = {2},
  Pages                    = {77--88},
  Volume                   = {81},
}
\examples{
data(woo)
## maybe str(woo) ; plot(woo) ...
}
\keyword{datasets}
