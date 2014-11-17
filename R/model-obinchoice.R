zobinchoice <- setRefClass("Zelig-obinchoice",
                          contains = "Zelig",
                          field = list(method = "character",
                                       linkinv = "function"
                          ))

zobinchoice$methods(
  initialize = function() {
    callSuper()
    .self$name <- "obinchoice"
    .self$fn <- quote(MASS::polr)
    .self$authors <- "Matthew Owen, Olivia Lau, Kosuke Imai, Gary King"
    .self$year <- 2011
    .self$category <- "multinomial"
  }
)

zobinchoice$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    .self$model.call$method <- .self$method
    .self$model.call$Hess <- TRUE
    formula <- update(formula, as.factor(.) ~ .)
    callSuper(formula = formula, data = data, ..., weights = NULL, by = by)
  }
)

zobinchoice$methods(
  param = function(z.out) {
    coef <- coef(z.out)
    zeta <- z.out$zeta
    theta <- zeta[1]
    for (k in 2:length(zeta))
      theta[k] <- log(zeta[k] - zeta[k - 1])
    simalpha <- zeta
    simparam <- mvrnorm(.self$num, c(coef, theta), vcov(z.out))
    simparam <- list(simparam = simparam, simalpha = simalpha)
    return(simparam)
  }
)

zobinchoice$methods(
  # From ZeligChoice 4
  qi = function(simparam, mm) {
    z <- .self$zelig.out$z.out[[1]]
    # startup work
    simulations <- simparam$simparam
    coef <- coef(z)
    # simulations on coefficients
    sim.coef <- simulations[, 1:length(coef), drop = FALSE]
    # remove (Intercept), make sure matrix is numeric
    mat1 <- as.numeric(as.matrix(mm)[, -1])
    # compute eta
    eta1 <- t(mat1 %*% t(sim.coef))
    # simulations on zeta, and define theta
    sim.zeta <- sim.theta <- simulations[, (length(coef) + 1):ncol(simulations), drop = FALSE]
    sim.zeta[, -1] <- exp(sim.theta[, -1])
    sim.zeta <- t(apply(sim.zeta, 1, cumsum))
    
    ev <- .compute.ev(z, mm, .self$num, simulations, eta1, sim.zeta)
    pv <- .compute.pr(z, mm, .self$num, simulations, eta1, sim.zeta)

    return(list(ev = ev, pv = pv))
  }
)

.compute.ev <- function(z, x, num, param, eta, sim.zeta) {
  if (is.null(x))
    return(NA)
  simulations <- param
  coef <- coef(z)
  # simulations on coefficients
  sim.coef <- simulations[, 1:length(coef), drop = FALSE]
  #
  k <- length(z$zeta) + 1
  # remove (Intercept), make sure matrix is numeric
  mat <- as.numeric(as.matrix(x)[, -1])
  # compute eta
  eta <- t(mat %*% t(sim.coef))
  lev <- z$lev
  rows <- as.matrix(x)
  Ipr <- cuts <- tmp0 <- {
    array(
      0, dim = c(num, k, nrow(rows)),
      dimnames = list(1:num, lev, rownames(rows))
    )
  }
  for (i in 1:num) {
    cuts[i, , ] <- t(.self$linkinv(eta[i, ], sim.zeta[i, ]))
  }
  # NOTE:
  #  2:k => 2, 3, 4, ..., k
  #  2:k-1 => 1, 2, 3, 4, ..., k-1
  tmp0[, 2:k, ] <- cuts[, 2:k - 1, ]
  ev <- cuts - tmp0
  dimnames(ev) <- list(1:num, z$result$lev, rownames(x))
  # remove unnecessary dimensions
  if (dim(ev)[3] == 1)
    ev <- ev[, , 1]
  colnames(ev) <- z$lev
  return(ev)
}

.compute.pr <- function(z, x, num, param, eta, sim.zeta) {
  if (is.null(x))
    return(NA)
  x <- as.matrix(x)
  rows <- x
  k <- length(z$zeta) + 1
  lev <- z$lev
  
  Ipr <- cuts <- tmp0 <- {
    array(
      0, dim = c(num, k, nrow(rows)),
      dimnames = list(1:num, lev, rownames(rows))
    )
  }
  for (i in 1:num) 
    cuts[i, , ] <- t(.self$linkinv(eta[i, ], sim.zeta[i, ]))
  pr <- matrix(NA, nrow = num, ncol = nrow(as.matrix(x)))
  tmp <- matrix(
    runif(length(cuts[, 1, ]), 0, 1),
    nrow = num,
    ncol = nrow(x)
  )

  k <- length(z$zeta) + 1
  for (j in 1:k)
    Ipr[, j, ] <- as.integer(tmp > cuts[, j, ])
  
  for (j in 1:nrow(x)) 
    pr[, j] <- 1 + rowSums(Ipr[, , j, drop = FALSE])
  
  factors <- factor(pr,
                    labels = lev[1:length(lev) %in% sort(unique(pr))],
                    ordered = TRUE)
  return(pr)
}
