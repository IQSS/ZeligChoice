#' Ordered Choice object for inheritance across models in ZeligChoice
#'
#' @import methods
#' @export Zelig-obinchoice
#' @exportClass Zelig-obinchoice




zobinchoice <- setRefClass("Zelig-obinchoice",
                           contains = "Zelig",
                           field = list(method = "character",
                                        linkinv = "function"
                           ))

zobinchoice$methods(
  initialize = function() {
    callSuper()
    .self$fn <- quote(MASS::polr)
    .self$authors <- "Matthew Owen, Olivia Lau, Kosuke Imai, Gary King"

    .self$year <- 2011
    .self$category <- "multinomial"
  }
)

zobinchoice$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL,
                   bootstrap = FALSE) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- match.call(expand.dots = TRUE)
    .self$model.call$method <- .self$method
    .self$model.call$Hess <- TRUE
    localformula <- update(formula, as.factor(.) ~ .)
    if (!is.null(weights)) 
        message('Note: Zelig weight results may differ from those in MASS::polr.')
    callSuper(formula = localformula, data = data, ..., weights = weights, 
              by = by, bootstrap = bootstrap)

    #rse<-plyr::llply(.self$zelig.out$z.out, (function(x) vcovHC(x,type="HC0")))
    #.self$test.statistics<- list(robust.se = rse)
  }
)

zobinchoice$methods(
  param = function(z.out, method="mvn") {
    coef <- coef(z.out)
    zeta <- z.out$zeta
    theta <- zeta[1]
    for (k in 2:length(zeta))
      theta[k] <- log(zeta[k] - zeta[k - 1])
    simalpha <- list(coef = coef, zeta = zeta, lev = z.out$lev)

    if(identical(method, "mvn")){
      localsimparam <- mvrnorm(.self$num, c(coef, theta), vcov(z.out))
      return(list(simparam = localsimparam, simalpha = simalpha))
    }else if(identical(method, "point")){
      return(list(simparam =t(as.matrix(c(coef, theta))), simalpha = simalpha))
    }
  }
)

zobinchoice$methods(
  # From ZeligChoice 4
  qi = function(simparam, mm) {
    # startup work
    simulations <- simparam$simparam
    coef <- simparam$simalpha$coef
    zeta <- simparam$simalpha$zeta
    lev <- simparam$simalpha$lev
    # simulations on coefficients
    sim.coef <- simulations[, 1:length(coef), drop = FALSE]
    # remove (Intercept), make sure matrix is numeric
    mat <- as.numeric(as.matrix(mm)[, -1])
    # compute eta
    eta <- t(mat %*% t(sim.coef))
    # simulations on zeta, and define theta
    sim.zeta <- sim.theta <- simulations[, (length(coef) + 1):ncol(simulations),
                                         drop = FALSE]
    sim.zeta[, -1] <- exp(sim.theta[, -1])
    sim.zeta <- t(apply(sim.zeta, 1, cumsum))

    ##----- Expected value

    k <- length(zeta) + 1
    # remove (Intercept), make sure matrix is numeric
    mat <- as.numeric(as.matrix(mm)[, -1])
    eta <- t(mat %*% t(sim.coef))
    rows <- as.matrix(mm)
    Ipv <- cuts <- tmp0 <- array(0, dim = c(.self$num, k, nrow(rows)),
                          dimnames = list(1:.self$num, lev, rownames(rows)))
    for (i in 1:.self$num) {
      cuts[i, , ] <- t(.self$linkinv(eta[i, ], sim.zeta[i, ]))
    }
    tmp0[, 2:k, ] <- cuts[, 2:k - 1, ] # 2:k-1 => 1, 2, 3, 4, ..., k-1
    ev <- cuts - tmp0
    dimnames(ev) <- list(1:.self$num, lev, rownames(mm))
    # remove unnecessary dimensions
    ev <- ev[, , 1]
    colnames(ev) <- lev

    ##----- Predicted value
    pv <- matrix(NA, nrow = .self$num, ncol = nrow(as.matrix(mm)))
    tmp <- matrix(runif(length(cuts[, 1, ]), 0, 1),
                  nrow = .self$num,
                  ncol = nrow(mm))
    for (j in 1:k)
      Ipv[, j, ] <- as.integer(tmp > cuts[, j, ])
    for (j in 1:nrow(mm))
      pv[, j] <- 1 + rowSums(Ipv[, , j, drop = FALSE])
    factors <- factor(pv,
                      labels = lev[1:length(lev) %in% sort(unique(pv))],
                      ordered = TRUE)

    return(list(ev = ev, pv = pv))
  }
)
