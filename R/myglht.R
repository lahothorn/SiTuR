myglht <- function (x, digits = max(3, getOption("digits") - 3), ...) 
{
  #    cat("\n\t", "Simultaneous Tests for General Linear Hypotheses\n\n")
  #    if (!is.null(x$type)) 
  # cat("Multiple Comparisons of Means:", x$type, "Contrasts\n\n\n")
  #    call <- if (isS4(x$model)) 
  #        x$model@call
  #   else x$model$call
  #  if (!is.null(call)) {
  #      cat("Fit: ")
  #      print(call)
  #      cat("\n")
  #  }
  pq <- x$test
  mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
  error <- attr(pq$pvalues, "error")
  pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df == 
                                                              0, "z", "t"), ")", sep = ""), greater = paste("Pr(>", 
                                                                                                            ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|", 
                                                                                                                                                                           ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
  colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df == 
                                                           0, "z value", "t value"), pname)
  type <- pq$type
  if (!is.null(error) && error > .Machine$double.eps) {
    sig <- which.min(abs(1/error - (10^(1:10))))
    sig <- 1/(10^sig)
  }
  else {
    sig <- .Machine$double.eps
  }
  #   cat("Linear Hypotheses:\n")
  alt <- switch(x$alternative, two.sided = "==", less = ">=", 
                greater = "<=")
  rownames(mtests) <- paste(rownames(mtests), alt, x$rhs)
  # printCoefmat(mtests, digits = digits, has.Pvalue = TRUE, 
  #     P.values = TRUE, eps.Pvalue = sig)
  switch(type, univariate = cat("(Univariate p values reported)"), 
         `single-step` = cat("(Adjusted p values reported -- single-step method)"), 
         Shaffer = cat("(Adjusted p values reported -- Shaffer method)"), 
         Westfall = cat("(Adjusted p values reported -- Westfall method)"), 
         cat("(Adjusted p values reported --", type, "method)"))
  #    cat("\n\n")
  mtests
}
