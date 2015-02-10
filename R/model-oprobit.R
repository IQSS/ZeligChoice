#' @include model-obinchoice.R
zoprobit <- setRefClass("Zelig-oprobit",
                       contains = "Zelig-obinchoice")

zoprobit$methods(
  initialize = function() {
    callSuper()
    .self$name <- "oprobit"
    .self$packageauthors <- "William N. Venables, and Brian D. Ripley"
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

zoprobit$methods(
  test = function(b0 = -1, b1 = 1, nsim = 1000, minx = -1, maxx = 1) {
    
    x.init <- mcunit.init(nsim, minx, maxx)
    xb <- b0 + b1 * x.init[,1]
    
    y.star <- rnorm(nsim, xb, 1)
    t1 <- qnorm(.25, mean=mean(xb), sd = 1)
    t2 <- qnorm(.5, mean=mean(xb), sd = 1)
    t3 <- qnorm(.75, mean=mean(xb), sd = 1)
    
    y.sim = rep(NA, nsim)
    y.sim[y.star < t1] <- 1
    y.sim[y.star >= t1 & y.star < t2] <- 2
    y.sim[y.star >= t2] <- 3
      
    y.true <- rep(NA, nsim)
    data <- data.frame(cbind(x.init, y.sim, y.true))
#     return(data)
# 
    z <- zoprobit$new()
    callSuper(z, data)
    
  }
)
