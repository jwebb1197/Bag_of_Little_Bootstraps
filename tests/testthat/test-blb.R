test_that("blblm works", {

  #load data
  library(datasets)
  data(iris)

  train = iris[1:75,]
  test = iris[76:150,]

  #generate models
  fit_blblm = blblm(Sepal.Length ~ Petal.Length, data = train, B = 100, parallel = TRUE, cl=2)
  fit_lm = lm(Sepal.Length ~ Petal.Length, data = train)

  #SIGMA: check if sigma_blblm is within [sigma_glm - 2], sigma_glm + 2]
  s_blblm = sigma(fit_blblm)
  s_lm = sigma(fit_lm)
  s_bool = ((s_lm - 2) <= s_blblm) & (s_blblm <= (s_lm + 2))

  #COEF: check if coef_blblm is within [coef_glm - 2], coef_glm + 2]
  coef_blblm = coef(fit_blblm)
  coef_lm = coef(fit_lm)
  coef_bool_vector = ((coef_lm - 2) <= coef_blblm) & (coef_blblm <= (coef_lm + 2))
  coef_bool = FALSE
  if(sum(coef_bool_vector) == length(coef_bool_vector)){coef_bool = TRUE}

  #CONFINT: check if confint_blblm is within [coef_glm - 2], coef_glm + 2]
  conf_blblm = confint(fit_blblm)
  conf_lm = confint(fit_lm)[2,]
  conf_bool_vector = ((conf_lm - 2) <= conf_blblm) & (conf_blblm <= (conf_lm + 2))
  conf_bool = FALSE
  if(sum(conf_bool_vector) == length(conf_bool_vector)){conf_bool = TRUE}

  #PREDICT: check that the predictions are reasonably accurate
  p_blblm = predict(fit_blblm, new_data=test)
  correlation = cor(p_blblm,test$Sepal.Length)
  pred_bool = correlation >= 0.75

  test_bool = all(c(s_bool, coef_bool, conf_bool, pred_bool))

  expect_equal(test_bool, TRUE)

})

test_that("blb_logreg works", {

  library(titanic)

  train = titanic_train[1:550,]
  test = titanic_train[551:891,]

  #generate models
  fit_blb_logreg = blb_logreg(Survived ~ Age + Sex + Pclass, data = train, m = 8, B = 100, parallel = TRUE, cl = 2)
  fit_glm = glm(Survived ~ Age + Sex + Pclass, data = train, family = "binomial")

  #COEF: check if coef_blblm is within [coef_glm - 3], coef_glm + 3]
  coef_blb_logreg = coef(fit_blb_logreg)
  coef_glm = coef(fit_glm)
  coef_bool_vector = ((coef_glm - 3.5) <= coef_blb_logreg) & (coef_blb_logreg <= (coef_glm + 3.5))
  coef_bool = FALSE
  if(sum(coef_bool_vector) == length(coef_bool_vector)){coef_bool = TRUE}

  #CONFINT: check if confint_blblm is within [coef_glm - 3], coef_glm + 3]
  conf_blb_logreg = confint(fit_blb_logreg)
  conf_glm = confint(fit_glm)[2:4,]
  conf_bool_vector = ((conf_glm - 3) <= conf_blb_logreg) & (conf_blb_logreg <= (conf_glm + 3))
  conf_bool = FALSE
  if(sum(conf_bool_vector) == length(conf_bool_vector)){conf_bool = TRUE}

  #PREDICT: check that the predictions are reasonably accurate

  #with p = FALSE, thus predicting actual values
  p_blb_logreg = predict(fit_blb_logreg, new_data=test)
  actual = test[complete.cases(test),]$Survived
  accuracy = sum(p_blb_logreg == actual)/length(actual)
  pred_bool = accuracy > 0.77

  test_bool = all(c(coef_bool, conf_bool, pred_bool))

  expect_equal(test_bool, TRUE)

})





