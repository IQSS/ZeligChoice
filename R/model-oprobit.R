#' @include model-obinchoice.R
zoprobit <- setRefClass("Zelig-oprobit",
                       contains = "Zelig-obinchoice")

zoprobit$methods(
  initialize = function() {
    callSuper()
    .self$name <- "oprobit"
    .self$description <- "Ordinal Probit Regression for Ordered Categorical Dependent Variables"
    .self$method <- "probit"
    .self$linkinv <- function(eta, zeta) {
      tmp1 <- matrix(1, nrow = length(eta), ncol = length(zeta) + 1)
      tmp1[, 1:length(zeta)] <- pnorm(zeta - eta)
      return(tmp1)
    }
    .self$wrapper <- "oprobit"
    .self$vignette.url <- "http://docs.zeligproject.org/en/latest/zeligchoice-oprobit.html"
  }
)
