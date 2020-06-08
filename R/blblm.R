#' @import purrr
#' @import stats
#' @importFrom magrittr %>%
#' @details
#' Linear Regression with Little Bag of Bootstraps
utils::globalVariables(c("."))
#' Bag of Little Boostraps Linear Regression
#' @param formula formula
#' @param data dataset of interest
#' @param m int: number of groups to split data into, used in bootstrapping process
#' @param B int: number of bootstrap samples
#' @param parallel boolean: defines whether or not to use parallelization. Note: if TRUE, run furrr::plan(multisession, worker = desired_number_of_CPUs) in console before using function.
#' @param cl int: desired number of clusters
#' @return linear regression model
#' @export
blblm <- function(formula, data, m = 10, B = 5000, parallel = FALSE, cl = NULL) {
# function layout:
#   i) if not using parallelization, run blblm_under(), otherwise run blblm_under_parallel()
  if (parallel == FALSE) {
    blblm_under(formula, data, m = m, B = B)
  }
  else {
    blblm_under_parallel(formula, data, clusters = cl, m = m, B = B)
  }
}
blblm_under <- function(formula, data, m = 10, B = 5000) {
# function layout:
#   i) split data into m approximately equal parts, store in a list called data_list
#   ii) map these sub-datasets to lm_each_subsample(), store results in nested list called estimates
#   iii) store estimates, and formula in a list called res
  data_list <- split_data(data, m)
  estimates <- map(
    data_list,
    ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B)
  )
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}
blblm_under_parallel <- function(formula, data, clusters, m = 10, B = 5000) {
  # function layout:
  #   i) split data into m approximately equal parts, store in a list called data_list
  #   ii) initiate clusters
  #   iii) use parallel Lapply to map these sub-datasets to lm_each_subsample(),
  #        store results in nested list called estimates
  #   iv) stop clusters
  #   v) store estimates, and formula in a list called res
  data_list <- split_data(data, m)
  cl <- makeCluster(clusters)

  estimates <- parLapply(cl, data_list,
    function(data, formula, n, B) {
      lm_each_subsample(formula, data = data, n = n, B = B)
    },
    formula = formula, n = nrow(data), B = B
  )
  stopCluster(cl)
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}
split_data <- function(data, m) {
  # function layout:
  #  i) sample m integers from the interval [1, number of rows in dataset]
  # ii) split dataset based off these indexes
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}
lm_each_subsample <- function(formula, data, n, B) {
  # function layout:
  # i) replicate lm_each_boot() B times
  replicate(B, lm_each_boot(formula, data, n), simplify = FALSE)
}
lm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))

  lm1(formula, data, freqs)
}
lm1 <- function(formula, data, freqs) {
  # function layout:
  # i) set to current environmnet
  # ii) use glm() to fit a linear regression model to data
  # iii) store coefficients and sigma in a list
  environment(formula) <- environment()

  fit <- lm(formula, data, weights = freqs)

  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}
blbcoef <- function(fit) {
  coef(fit)
}
blbsigma <- function(fit) {
  p <- fit$rank
  y <- model.extract(fit$model, "response")
  e <- fitted(fit) - y
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}
#' Print
#' @param x blblm object
#' @param ... unused arguments
#' @return prints model
#' @export
#' @method print blblm
print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}
#' Standard Deviation
#' @param object blb_logreg object
#' @param confidence boolean: confidence interval
#' @param level int: confidence level
#' @param ... unused arguments
#' @return standard deviation
#' @export
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  #function layout:
  # i) obtain estimates
  # ii) obtain average sigma
  # iii) obstain confidence interval
  est <- object$estimates

  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))

  if (confidence) {
    alpha <- 1 - 0.95

    limits <- est %>%
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)

    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}
#' Coefficients
#' @param object blb_logreg object
#' @param ... unused arguments
#' @return coefficients of parameters in linear regression model
#' @export
#' @method coef blblm
coef.blblm <- function(object, ...) {
  # function layout
  # i) obtain estimates
  # ii) obtain means of coefficients
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}
#' Confidence interval of Mean
#' @param object blblm object
#' @param parm string/character vector: parameter(s) of interest
#' @param level numeric: confidence level
#' @param ... unused arguments
#' @return confidence interval of parameters of interest, all parameters by default
#' @export
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  # function layout:
  # i)   obtain parameters
  # ii)  use map_rbind to obtain a confidence interval for parameters
  # iii) label confidence intervals with corresponding parameter
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}
#' Predict
#' @param object blblm object
#' @param new_data data set of interest
#' @param confidence boolean: confidence interval for predicted value
#' @param level numeric: confidence level
#' @param ... unused arguments
#' @return vector of predicted values of response variable
#' @export
#' @method predict blblm
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  # function layout:
  # i)obtain estimates
  # ii) obtain design matrix of predictors
  # iii) obtain predictions; include confidence intervals if confidence = TRUE in arguments
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
      apply(1, mean_lwr_upr, level = level) %>%
      t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  }
}
# map then find upper and lower .025 and .975 quantiles from the mean
mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}
# map then find mean of results
map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}
# map and column bind
map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}
# map and rowbind
map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}
