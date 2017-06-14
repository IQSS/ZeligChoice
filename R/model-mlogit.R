#' Multinomial Logistic Regression for Dependent Variables with Unordered Categorical Values
#'
#' @param formula a symbolic representation of the model to be
#'   estimated, in the form \code{as.factor(y) \~\, x1 + x2}, where \code{y} is the
#'   dependent variable and \code{x1} and \code{x2} are the explanatory
#'   variables, and \code{y}, \code{x1}, and \code{x2} are contained in the
#'   same dataset. (You may include more than two explanatory variables,
#'   of course.) The \code{+} symbol means ``inclusion'' not
#'   ``addition.'' You may also include interaction terms and main
#'   effects in the form \code{x1*x2} without computing them in prior
#'   steps; \code{I(x1*x2)} to include only the interaction term and
#'   exclude the main effects; and quadratic terms in the form
#'   \code{I(x1^2)}.
#' @param model the name of a statistical model to estimate.
#'   For a list of supported models and their documentation see:
#'   \url{http://docs.zeligproject.org/articles/}.
#' @param data the name of a data frame containing the variables
#'   referenced in the formula or a list of multiply imputed data frames
#'   each having the same variable names and row numbers (created by
#'   \code{Amelia} or \code{\link{to_zelig_mi}}).
#' @param ... additional arguments passed to \code{zelig},
#'   relevant for the model to be estimated.
#' @param by a factor variable contained in \code{data}. If supplied,
#'   \code{zelig} will subset
#'   the data frame based on the levels in the \code{by} variable, and
#'   estimate a model for each subset. This can save a considerable amount of
#'   effort. For example, to run the same model on all fifty states, you could
#'   use: \code{z.out <- zelig(y ~ x1 + x2, data = mydata, model = 'ls',
#'   by = 'state')} You may also use \code{by} to run models using MatchIt
#'   subclasses.
#' @param cite If is set to 'TRUE' (default), the model citation will be printed
#'   to the console.
#'
#' @examples
#' library(zeligverse)
#' data(mexico)
#' z.out1 <- zelig(as.factor(vote88) ~ pristr + othcok + othsocok, 
#'                 model = "mlogit", data = mexico, cite = FALSE) 
#'
#' @details
#' The dependent variable (Y in the parameters) should be a categorical varible.
#' By default the last level and omitted. (You cannot specify a different base level at this time.) For J equations,
#' there must be J+1 levels.
#' 
#' Additional parameters avaialable to many models include:
#' \itemize{
#'   \item weights: vector of weight values or a name of a variable in the dataset
#'   by which to weight the model. For more information see:
#'   \url{http://docs.zeligproject.org/articles/weights.html}.
#'   \item bootstrap: logical or numeric. If \code{FALSE} don't use bootstraps to
#'   robustly estimate uncertainty around model parameters due to sampling error.
#'   If an integer is supplied, the number of boostraps to run.
#'   For more information see:
#'   \url{http://docs.zeligproject.org/articles/bootstraps.html}.
#' }

#'
#' @return Depending on the class of model selected, \code{zelig} will return
#'   an object with elements including \code{coefficients}, \code{residuals},
#'   and \code{formula} which may be summarized using
#'   \code{summary(z.out)} or individually extracted using, for example,
#'   \code{coef(z.out)}. See
#'   \url{http://docs.zeligproject.org/articles/getters.html} for a list of
#'   functions to extract model components. You can also extract whole fitted
#'   model objects using \code{\link{from_zelig_model}}.
#'
#'
#' @seealso Vignette: \url{http://docs.zeligproject.org/articles/zeligchoice_mlogit.html}
#' @import methods
#' @export Zelig-bprobit
#' @exportClass Zelig-bprobit

zmlogit <- setRefClass("Zelig-mlogit",
                          contains = "Zelig",
                          field = list(family = "ANY",
                                       linkinv = "function"
                          ))

zmlogit$methods(
  initialize = function() {
    callSuper()
    .self$name <- "mlogit"
    .self$description <- "Multinomial Logistic Regression for Dependent Variables with Unordered Categorical Values"
    .self$fn <- quote(VGAM::vglm)
    .self$authors <- "Matthew Owen, Olivia Lau, Kosuke Imai, Gary King"
    .self$packageauthors <- "Thomas W. Yee"
    .self$year <- 2007
    .self$category <- "multinomial"
    .self$family <- "multinomial"
    .self$wrapper <- "mlogit"
    .self$vignette.url <- "http://docs.zeligproject.org/articles/zeligchoice_mlogit.html"
  }
)

zmlogit$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL, bootstrap = FALSE) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    .self$model.call$family <- .self$family
    callSuper(formula = formula, data = data, ..., weights = NULL, by = by, bootstrap = bootstrap)
  }
)

zmlogit$methods(
  param = function(z.out, method="mvn") {
    if(identical(method,"mvn")){
      return(mvrnorm(.self$num, coef(z.out), vcov(z.out))) 
    } else if(identical(method,"point")){
      return(t(as.matrix(coef(z.out))))
    } else {
      stop("param called with method argument of undefined type.")
    }
  }
)

zmlogit$methods(
  # From ZeligChoice 4
  qi = function(simparam, mm) {
    fitted <- .self$zelig.out$z.out[[1]]
    # get constraints from fitted model
    constraints <- fitted@constraints
    coef <- simparam
    ndim <- ncol(fitted@y) - 1
    all.coef <- NULL
    v <- construct.v(constraints, ndim)
    # put all indexed lists in the appropriate section
    for (i in 1:ndim)
      all.coef <- c(all.coef, list(coef[, v[[i]]]))
#     cnames <- ynames <-  if (is.null(colnames(fitted@y))) {1:(ndim + 1)} else colnames(fitted@y)
    if (is.null(colnames(fitted@y))) {
      cnames <- 1:(ndim + 1)
    } else
        cnames <- colnames(fitted@y)
    ynames <- cnames
    cnames <- paste("Pr(Y=", cnames, ")", sep = "")
    ev <- ev.mlogit(fitted, constraints, all.coef, mm, ndim, cnames)
    pv <- pv.mlogit(fitted, ev) #, ynames)
    return(list(ev = ev, pv = pv))
  }
)


#' Split Names of Vectors into N-vectors
#' This function is used to organize how variables are spread
#' across the list of formulas
#' @usage construct.v(constraints, ndim)
#' @param constraints a constraints object
#' @param ndim an integer specifying the number of dimensions
#' @return a list of character-vectors
construct.v <- function(constraints, ndim) {
  v <- rep(list(NULL), ndim)
  names <- names(constraints)
  for (i in 1:length(constraints)) {
    cm <- constraints[[i]]
    for (j in 1:ndim) {
      if (sum(cm[j, ]) == 1) {
        v[[j]] <- if (ncol(cm) == 1)
          c(v[[j]], names[i])
        else
          c(v[[j]], paste(names[i], ':', j, sep=""))
      }
    }
  }
  return(v)
}


#' Simulate Expected Value for Multinomial Logit
#' @usage ev.mlogit(fitted, constraints, all.coef, x, ndim, cnames)
#' @param fitted a fitted model object
#' @param constraints a constraints object
#' @param all.coef all the coeficients
#' @param x a setx object
#' @param ndim an integer specifying the number of dimensions
#' @param cnames a character-vector specifying the names of the columns
#' @return a matrix of simulated values
ev.mlogit <- function (fitted, constraints, all.coef, x, ndim, cnames) {
  if (is.null(x))
    return(NA)
  linkinv <- fitted@family@linkinv
  xm <- rep(list(NULL), ndim)
  sim.eta <- NULL
  x <- as.matrix(x)
  for (i in 1:length(constraints)) {
    for (j in 1:ndim)
      if (sum(constraints[[i]][j,] ) == 1)
        xm[[j]] <- c(xm[[j]], x[, names(constraints)[i]])
  }
  for (i in 1:ndim)
    sim.eta <- cbind(sim.eta, all.coef[[i]] %*% as.matrix(xm[[i]]))
  ev <- linkinv(sim.eta, extra = fitted@extra)
  colnames(ev) <- cnames
  return(ev)
}

#' Simulate Predicted Values
#' @usage pv.mlogit(fitted, ev)
#' @param fitted a fitted model object
#' @param ev the simulated expected values
#' @return a vector of simulated values
pv.mlogit <- function (fitted, ev){ #, ynames) {
  if (all(is.na(ev)))
    return(NA)
  # initialize predicted values and a matrix
  pv <- NULL
  Ipv <- sim.cut <- matrix(NA, nrow = nrow(ev), ncol(ev))
  k <- ncol(ev)
  colnames(Ipv) <- colnames(sim.cut) <- colnames(ev)
  sim.cut[, 1] <- ev[, 1]
  for (j in 2:k)
    sim.cut[, j] <- sim.cut[ , j - 1] + ev[, j]
  tmp <- runif(nrow(ev), min = 0, max = 1)
  for (j in 1:k)
    Ipv[, j] <- tmp > sim.cut[, j]
  for (j in 1:nrow(Ipv))
    pv[j] <- 1 + sum(Ipv[j, ])
  pv <- factor(pv, ordered = FALSE)
  pv.matrix <- matrix(pv, nrow = dim(ev)[1])
  levels(pv.matrix) <- levels(pv)
  return(pv.matrix)
}

zmlogit$methods(
  mcfun = function(x, b0=-0.5, b1=0.5, b2=-1, b3=1, ..., sim=TRUE){
    mu1 <- b0 + b1 * x
    mu2 <- b2 + b3 * x

    if(sim){
      n.sim = length(x)
      y.star.1 <- exp( rlogis(n = n.sim, location = mu1, scale = 1) ) # latent continuous y
      y.star.2 <- exp( rlogis(n = n.sim, location = mu2, scale = 1) ) # latent continuous y
      pi1 <- y.star.1/(1 + y.star.1 + y.star.2)
      pi2 <- y.star.2/(1 + y.star.1 + y.star.2)
      pi3 <- 1 - pi1 - pi2

      y.draw <- runif(n=n.sim)
      y.obs <- 1 + as.numeric(y.draw>pi1) + as.numeric(y.draw>(pi1 + pi2))
      return(as.factor(y.obs))
    }else{
      pi1.hat <- exp(mu1)/(1 + exp(mu1) + exp(mu2))
      pi2.hat <- exp(mu2)/(1 + exp(mu1) + exp(mu2))
      pi3.hat <- 1 - pi1.hat - pi2.hat
      
      y.obs.hat <- pi1.hat*1 + pi2.hat*2 + pi3.hat*3    # This is the expectation the MC test will check, although it is not substantively meaningful for factor dep. var.
      return(y.obs.hat)
    }
  }
)
