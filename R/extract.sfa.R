# Add extract class to texreg for sfa-forntier package
#' Title
#'
#' @param model model object
#' @param include.loglik GOF log-likelihood
#' @param include.eff GOF Mean efficiency
#' @param include.nobs GOF number of observations
#' @param ... extra setting for sfa
#'
#' @return list of extract
#'
#' @importFrom methods setMethod
#'
extract.sfa <- function(model,
                        include.loglik = TRUE,
                        include.eff = TRUE,
                        include.nobs = TRUE, ...) {
  s <- summary(model, ...)
  names <- rownames(s$mleParam)
  co <- s$mleParam[, 1]
  se <- s$mleParam[, 2]
  pval <- s$mleParam[, 4]

  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.loglik == TRUE) {
    loglike <- s$mleLogl
    gof <- c(gof, loglike)
    gof.names <- c(gof.names, "Log Likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.eff == TRUE) {
    eff <- s$efficMean
    gof <- c(gof, eff)
    gof.names <- c(gof.names, "Mean efficiency")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    n <- as.integer(s$nob)
    gof <- c(gof, n)
    gof.names <- c(gof.names, "Num.\\ obs.")
    gof.decimal <- c(gof.decimal, FALSE)
  }

  tr <- texreg::createTexreg(
    coef.names = names,
    coef = co,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

setMethod("extract", signature = className("frontier", "frontier"), definition = extract.sfa)
