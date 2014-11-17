#' @include model-bbinchoice.R
zbprobit <- setRefClass("Zelig-bprobit",
                        contains = "Zelig-bbinchoice")

zbprobit$methods(
  initialize = function() {
    callSuper()
    .self$name <- "bprobit"
    .self$description <- "Bivariate Probit Regression for Dichotomous Dependent Variables"
    .self$family <- quote(binom2.rho(zero = 3))
    .self$linkinv <- binom2.rho()@linkinv
  }
)

