#' Bivariate Binary Choice object for inheritance across models in ZeligChoice.
#' 
#' This class is the overarching class for the binary logit and binary probit models.
#' Load all required packages using \code{library(zeligverse)}.
#'
#' @param formula a symbolic representation of the model to be
#'   estimated, in the form \code{cbind(y1,y2) \~\, x1 + x2}, where \code{y1} and \code{y2} are the
#'   dependent variables and \code{x1} and \code{x2} are the explanatory
#'   variables, and \code{y}, \code{x1}, and \code{x2} are contained in the
#'   same dataset. (You may include more than two explanatory variables,
#'   of course.) The \code{+} symbol means ``inclusion'' not
#'   ``addition.'' You may also include interaction terms and main
#'   effects in the form \code{x1*x2} without computing them in prior
#'   steps; \code{I(x1*x2)} to include only the interaction term and
#'   exclude the main effects; and quadratic terms in the form
#'   \code{I(x1^2)}. See details section for further information.
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
#' @details
#' Formula Details: In every bivariate probit specification, there are three equations
#'  which correspond to each dependent variable (Y1Y1, Y2Y2), and the 
#'  correlation parameter ρρ. Since the correlation parameter does not correspond 
#'  to one of the dependent variables, the model estimates ρρ as a constant by default. 
#'  Hence, only two formulas (for μ1μ1 and μ2μ2) are required. If the explanatory variables 
#'  for μ1μ1 and μ2μ2 are the same and effects are estimated separately for each parameter,
#'  you may use the following short hand: \code{fml <- list(cbind(Y1,Y2) ~ X1 + X2)} which
#'  has the same meaning as: \code{fml <- list(mu1 = Y1 ~ X1 + X2, + mu2 = Y2 ~ X1 + X2)}.
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
#' @import methods
#' @export Zelig-bbinchoice
#' @exportClass Zelig-bbinchoice



zbbinchoice <- setRefClass("Zelig-bbinchoice",
                          contains = "Zelig",
                          field = list(family = "ANY",
                                       linkinv = "function"
                          ))

zbbinchoice$methods(
  initialize = function() {
    callSuper()
    .self$fn <- quote(VGAM::vglm)
    .self$authors <- "Kosuke Imai, Gary King, Olivia Lau"
    .self$packageauthors <- "Thomas W. Yee"
    .self$year <- 2007
    .self$category <- "dichotomous"
  }
)

zbbinchoice$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL, bootstrap = FALSE) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    .self$model.call$family <- .self$family
    if (!is.null(weights)) 
        message('Note: Zelig weight results may differ from those in VGAM::vglm.')
    callSuper(formula = formula, data = data, ..., weights = weights, by = by, 
              bootstrap = bootstrap)
  }
)

zbbinchoice$methods(
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

zbbinchoice$methods(
  # From Zelig 4
  qi = function(simparam, mm) {
    .pp <- function(object, constr, all.coef, x) {
      xm <- list()
      xm <- rep(list(NULL), 3)
      sim.eta <- NULL
      for (i in 1:length(constr))
        for (j in 1:3)
          if (sum(constr[[i]][j,]) == 1)
            xm[[j]] <- c(xm[[j]], x[,names(constr)[i]])
      sim.eta <- cbind(
        all.coef[[1]] %*% as.matrix( xm[[1]] ),
        all.coef[[2]] %*% as.matrix( xm[[2]] ),
        all.coef[[3]] %*% as.matrix( xm[[3]] )
      )
      # compute inverse (theta)
      ev <- .self$linkinv(sim.eta)
      # assign correct column names
      colnames(ev) <- c("Pr(Y1=0, Y2=0)",
                        "Pr(Y1=0, Y2=1)",
                        "Pr(Y1=1, Y2=0)",
                        "Pr(Y1=1, Y2=1)"
      )
      return(ev)
    }
    
    .pr <- function(ev) {
      mpr <- cbind(ev[, 3] + ev[, 4], ev[, 2] + ev[, 4])
      index <- matrix(NA, ncol=2, nrow=nrow(mpr))
      index[, 1] <- rbinom(n=nrow(ev), size=1, prob=mpr[, 1])
      index[, 2] <- rbinom(n=nrow(ev), size=1, prob=mpr[, 2])
      pr <- matrix(NA, nrow=nrow(ev), ncol=4)
      pr[, 1] <- as.integer(index[, 1] == 0 & index[, 2] == 0)
      pr[, 2] <- as.integer(index[, 1] == 0 & index[, 2] == 1)
      pr[, 3] <- as.integer(index[, 1] == 1 & index[, 2] == 0)
      pr[, 4] <- as.integer(index[, 1] == 1 & index[, 2] == 1)
      colnames(pr) <- c("(Y1=0, Y2=0)",
                        "(Y1=0, Y2=1)",
                        "(Y1=1, Y2=0)",
                        "(Y1=1, Y2=1)")
      return(pr)
    }
    .make.match.table <- function(index, cols=NULL) {
      pr <- matrix(0, nrow=nrow(index), ncol=4)
      # assigns values by the rule:
      #   pr[j,1] = 1 iff index[j,1] == 0 && index[j,2] == 0
      #   pr[j,2] = 1 iff index[j,1] == 0 && index[j,2] == 1
      #   pr[j,3] = 1 iff index[j,1] == 1 && index[j,2] == 0
      #   pr[j,4] = 1 iff index[j,1] == 1 && index[j,2] == 1
      # NOTE: only one column can be true at a time, so as a result
      #       we can do a much more elegant one liner, that I'll code
      #       later.  In this current form, I don't think this actually
      #       explains what is going on.
      pr[, 1] <- as.integer(index[, 1] == 0 & index[, 2] == 0)
      pr[, 2] <- as.integer(index[, 1] == 0 & index[, 2] == 1)
      pr[, 3] <- as.integer(index[, 1] == 1 & index[, 2] == 0)
      pr[, 4] <- as.integer(index[, 1] == 1 & index[, 2] == 1)
      # assign column names
      colnames(pr) <- if (is.character(cols) && length(cols)==4)
        cols
      else
        c("(Y1=0, Y2=0)",
          "(Y1=0, Y2=1)",
          "(Y1=1, Y2=0)",
          "(Y1=1, Y2=1)")
      return(pr)
    }
    all.coef <- NULL
    coefs <- simparam
    cm <- constraints(.self$zelig.out$z.out[[1]])
    v <- vector("list", 3)
    for (i in 1:length(cm)) {
      if (ncol(cm[[i]]) == 1){
        for (j in 1:3)
          if (sum(cm[[i]][j, ]) == 1)
            v[[j]] <- c(v[[j]], names(cm)[i])
      }
      else {
        for (j in 1:3)
          if (sum(cm[[i]][j,]) == 1)
            v[[j]] <- c(v[[j]], paste(names(cm)[i], ":", j, sep=""))
      }
    }
    for(i in 1:3)
      all.coef[[i]] <- coefs[ , unlist(v[i]) ]
    col.names <- c("Pr(Y1=0, Y2=0)",
                   "Pr(Y1=0, Y2=1)",
                   "Pr(Y1=1, Y2=0)",
                   "Pr(Y1=1, Y2=1)"
    )
    ev <- .pp(.self$zelig.out$z.out[[1]], cm, all.coef, as.matrix(mm))
    pv <- .pr(ev)
    levels(pv) <- c(0, 1)
#     return(list("Predicted Probabilities: Pr(Y1=k|X)" = ev,
#                 "Predicted Values: Y=k|X" = pv))
    return(list(ev = ev, pv = pv))
  }
)

# zbinchoice$methods(
#   show = function() {
#     lapply(.self$zelig.out, function(x) print(VGAM::summary(x)))
#   }
# )

