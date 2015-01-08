#' @include model-obinchoice.R
zologit <- setRefClass("Zelig-ologit",
                       contains = "Zelig-obinchoice")

zologit$methods(
  initialize = function() {
    callSuper()
    .self$name <- "ologit"
    .self$packageauthors <- "William N. Venables, and Brian D. Ripley"
    .self$description <- "Ordinal Logit Regression for Ordered Categorical Dependent Variables"
    .self$method <- "logistic"
    .self$linkinv <- function(eta, zeta) {
      tmp1 <- matrix(1, nrow = length(eta), ncol = length(zeta) + 1)
      tmp1[, 1:length(zeta)] <- exp(zeta - eta) / (1 + exp(zeta - eta))
      return(tmp1)
    }
    .self$wrapper <- "ologit"
    .self$vignette.url <- "http://docs.zeligproject.org/en/latest/zeligchoice-ologit.html"
  }
)
