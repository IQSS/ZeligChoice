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
    .self$year <- 2007
    .self$category <- "multinomial"
    # .self$family <- quote(VGAM::multinomial)
    .self$family <- "multinomial"
  }
)

zmlogit$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    .self$model.call$family <- .self$family
    callSuper(formula = formula, data = data, ..., weights = NULL, by = by)
  }
)

zmlogit$methods(
  param = function(z.out) {
    return(mvrnorm(.self$num, coef(z.out), vcov(z.out)))
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
    pv <- pv.mlogit(fitted, ev, ynames)
    return(list(ev = ev, pv = pv))
  }
)

# qi.mlogit <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {
#   # get fitted model object
#   fitted <- GetObject(obj)
#   # get constraints from fitted model
#   constraints <- fitted@constraints
#   coef <- coef(param)
#   ndim <- ncol(fitted@y) - 1
#   all.coef <- NULL
#   v <- construct.v(constraints, ndim)
#   # put all indexed lists in the appropriate section
#   for (i in 1:ndim)
#     all.coef <- c(all.coef, list(coef[, v[[i]]]))
#   cnames <- ynames <- if (is.null(colnames(fitted@y)))
#     1:(ndim+1)
#   else
#     colnames(fitted@y)
#   cnames <- paste('Pr(Y=', cnames, ')', sep='')
#   ev1 <- ev.mlogit(fitted, constraints, all.coef, x, ndim, cnames)
#   ev2 <- ev.mlogit(fitted, constraints, all.coef, x1, ndim, cnames)
#   pv1 <- pv.mlogit(fitted, ev1, ynames)
#   pv2 <- pv.mlogit(fitted, ev2, ynames)
#   
#   return(list(
#     "Expected Values: E(Y|X)" = ev1,
#     "Expected Values: E(Y|X1)" = ev2,
#     "Predicted Values: Y|X" = pv1,
#     "Predicted Values: Y|X1" = pv2,
#     "First Differences: E(Y|X1) - E(Y|X)" = ev2 - ev1,
#     "Risk Ratios: E(Y|X1)/E(Y|X)" = ev2/ev1))
# }

# Split Names of Vectors into N-vectors
# This function is used to organize how variables are spread
# across the list of formulas
# @param constraints a constraints object
# @param ndim 
# @return a list of character-vectors
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
  ev <- linkinv(sim.eta)
  colnames(ev) <- cnames
  return(ev)
}

pv.mlogit <- function (fitted, ev, ynames) {
  if (all(is.na(ev)))
    return(NA)
  # initialize predicted values and a matrix
  pr <- NULL
  Ipr <- sim.cut <- matrix(NA, nrow = nrow(ev), ncol(ev))
  k <- ncol(ev)
  colnames(Ipr) <- colnames(sim.cut) <- colnames(ev)
  sim.cut[, 1] <- ev[, 1]
  for (j in 2:k)
    sim.cut[, j] <- sim.cut[ , j - 1] + ev[, j]
  tmp <- runif(nrow(ev), min = 0, max = 1)
  for (j in 1:k)
    Ipr[, j] <- tmp > sim.cut[, j]
  for (j in 1:nrow(Ipr))
    pr[j] <- 1 + sum(Ipr[j, ])
#   pr <- factor(pr, levels = sort(unique(pr)), labels = ynames)
  pr <- factor(pr, ordered = FALSE)
  pr.matrix <- matrix(pr, nrow = dim(ev)[1])
  levels(pr.matrix) <- levels(pr)
  return(pr.matrix)
}

# zbinchoice$methods(
#   show = function() {
#     lapply(.self$zelig.out, function(x) print(VGAM::summary(x)))
#   }
# )
