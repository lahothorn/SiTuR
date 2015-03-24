\name{IsoBHPlot1}
\alias{IsoBHPlot1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Modified IsoBHPlot}
\description{IsoBHPlot without title printing}
\usage{
IsoBHPlot1(rp, FDR, stat = c("E2", "Williams", "Marcus", "M", "ModifM"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rp}{
%%     ~~Describe \code{rp} here~~
}
  \item{FDR}{
%%     ~~Describe \code{FDR} here~~
}
  \item{stat}{
%%     ~~Describe \code{stat} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (rp, FDR, stat = c("E2", "Williams", "Marcus", "M", 
    "ModifM")) 
{
    stat <- match.arg(stat)
    Probe.ID <- rp[, 1]
    rpraw <- switch(stat, E2 = rp[, 2], Williams = rp[, 3], Marcus = rp[, 
        4], M = rp[, 5], ModifM = rp[, 6])
    adjp <- cbind(rpraw, p.adjust(rpraw, "BH"), p.adjust(rpraw, 
        "BY"))
    plot(1:nrow(rp), sort(adjp[, 1]), col = 4, pch = ".", lty = 1, 
        xlab = "index", ylab = "Adjusted P values")
    lines(1:nrow(rp), sort(adjp[, 1]), lty = 1, col = 1)
    lines(1:nrow(rp), sort(adjp[, 2]), lty = 4, col = 2)
    lines(1:nrow(rp), sort(adjp[, 3]), lty = 5, col = 3)
    abline(FDR, 0, lty = 6)
    legend(nrow(rp)/2, 0.3, col = c(1, 2, 3), c("Raw P", "BH(FDR)", 
        "BY(FDR)"), lty = c(1, 4:5))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
