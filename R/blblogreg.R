#' @import parallel
#' @import purrr
#' @import stats
#' @importFrom magrittr %>%
#' @importFrom utils capture.output
#' @details
#' Logistic Regression with Little Bag of Bootstraps
utils::globalVariables(c("."))
#' Bag of Little Boostraps Linear Regression
#' @param formula formula
#' @param data dataset of interest
#' @param m int: number of groups to split data into, used in bootstrap process
#' @param B int: number of bootstrap samples
#' @param parallel boolean: defines whether or not to use parallelization. Note: if TRUE, run furrr::plan(multisession, worker = desired_number_of_CPUs) in console before using function.
#' @param cl int: desired number of clusters
#' @return logisitic regression model
#' @export
blb_logreg <- function(formula, data, m = 10, B = 5000, parallel = FALSE, cl = NULL) {
  # function layout:
  #   i) if not using parallelization, run blb_logreg_under(), otherwise run blb_logreg_under_parallel()
  if (parallel == FALSE) {
    blb_logreg_under(formula, data, m = m, B = B)
  }
  else {
    blb_logreg_under_parallel(formula, data, clusters = cl, m = m, B = B)
  }
}
blb_logreg_under <- function(formula, data, m = 10, B = 5000) {
  # function layout:
  #   i) split data into m approximately equal parts, store in a list called data_list
  #   ii) map these sub-datasets to logreg_each_subsample(), store results in nested list called estimates
  #   iii) store estimates, and formula in a list called res
  data_list <- split_data(data, m)
  estimates <- map(
    data_list,
    ~ logreg_each_subsample(formula, data = ., n = nrow(data), B = B)
  )
  res <- list(estimates = estimates, formula = formula) # store results in a list
  class(res) <- "blb_logreg"
  invisible(res)
}
blb_logreg_under_parallel <- function(formula, data, clusters, m = 10, B = 5000) {
  # function layout:
  #   i) split data into m approximately equal parts, store in a list called data_list
  #   ii) initiate clusters
  #   iii) use parallel Lapply to map these sub-datasets to logreg_each_subsample(),
  #        store results in nested list called estimates
  #   iv) stop clusters
  #   v) store estimates, and formula in a list called res
  data_list <- split_data(data, m)
  cl <- makeCluster(clusters)
  estimates <- parLapply(cl, data_list,
    function(data, formula, n, B) {
      logreg_each_subsample(formula, data = data, n = n, B = B)
    },
    formula = formula, n = nrow(data), B = B
  )
  stopCluster(cl)
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blb_logreg"
  invisible(res)
}

split_data <- function(data, m) {
  # function layout:
  #  i) sample m integers from the interval [1, number of rows in dataset]
  # ii) split dataset based off these indexes
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}
logreg_each_subsample <- function(formula, data, n, B) {
  # function layout:
  # i) replicate logreg_each_boot() B times
  replicate(B, logreg_each_boot(formula, data, n), simplify = FALSE)
}
logreg_each_boot <- function(formula, data, n) {
  # function layout:
  # i) logreg1() is given 5 trials to converge
  trial <- 0
  while (trial <= 5) {
    trial <- trial + 1
    freqs <- rmultinom(1, n, rep(1, nrow(data)))
    res <- logreg1(formula, data, freqs)
    if (res$converged) {
      break
    }
  }
  res
}
logreg1 <- function(formula, data, freqs) {
  # function layout:
  # i) set to current environmnet
  # ii) use glm() to fit a logistic regression model to data
  # iii) store coefficients and converged bool in a list
  environment(formula) <- environment()
  fit <- suppressWarnings(glm(formula, data, family = "binomial", weights = freqs))
  list(coef = blbcoef(fit), converged = fit$converged)
}
blbcoef <- function(fit) {
  coef(fit)
}
#' Print
#' @param x blb_logreg object
#' @param ... unused arguments
#' @return prints model
#' @export
#' @method print blb_logreg
print.blb_logreg <- function(x, ...) {
  cat("blb_logreg model:", capture.output(x$formula))
  cat("\n")
}
#' Coefficients
#' @param object blb_logreg object
#' @param ... unused argments
#' @return coefficients of parameters in logistic regression model
#' @export
#' @method coef blb_logreg
coef.blb_logreg <- function(object, ...) {
  # function layout
  # i) retrieve estimates
  # ii) obtain means of coefficients
  est <- object$estimates
  map_mean(est, ~ rowMeans(map_cbind(., "coef")))
}
#' Confidence Interval for Mean
#' @param object blb_logreg object
#' @param parm string/character vector: parameter(s) of interest
#' @param level numeric: confidence level
#' @param ... unused arguments
#' @return confidence interval of parameters of interest, all parameters by default
#' @export
#' @method confint blb_logreg
confint.blb_logreg <- function(object, parm = NULL, level = 0.95, ...) {
  # function layout:
  # i)   obtain parameters
  # ii)  use map_rbind to obtain a confidence interval for parameters
  # iii) label confidence intervals with corresponding parameter
  if (is.null(parm)) {
    parm <- names(coef(object))[2:length(coef(object))]
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
#' @param object blb_logreg object
#' @param new_data data set of interest
#' @param p boolean: probability; if TRUE, returns P(Y = 1|X = x), instead of binary {0,1} prediction
#' @param ... unused arguments
#' @return vector of predicted values of response variable
#' @export
#' @method predict blb_logreg
predict.blb_logreg <- function(object, new_data, p = FALSE, ...) {
  # function layout:
  # i)obtain estimates
  # ii) obtain design matrix of predictors
  # iii) find P(Y = 1|X = x) for all observations
  # iv) if user wants prediction of actual binary values, round this probability, otherwise leave as is
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  vector_of_log_odds_ratios <- map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  prob_y_equals_1_given_x <- 1 / (1 + (exp(-1 * vector_of_log_odds_ratios)))
  if (p == FALSE) {
    prob_y_equals_1_given_x %>% round()
  }
  else {
    prob_y_equals_1_given_x
  }
}
mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}
map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}
map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}
map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}
