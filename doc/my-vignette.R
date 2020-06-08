## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(blb)

## -----------------------------------------------------------------------------
library(datasets)
data(iris)

train = iris[1:75,]
test = iris[76:150,]

## -----------------------------------------------------------------------------
fit = blblm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = train)

## -----------------------------------------------------------------------------
fit = blblm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = train, m = 8, B = 100, parallel = TRUE, cl = 4)

## -----------------------------------------------------------------------------
print(fit)

## -----------------------------------------------------------------------------
sigma(fit)

## -----------------------------------------------------------------------------
sigma(fit, confidence = TRUE, level = 0.9)

## -----------------------------------------------------------------------------
coef(fit)

## -----------------------------------------------------------------------------
confint(fit)

## -----------------------------------------------------------------------------
confint(fit, parm = "Sepal.Width", level = 0.90)



confint(fit, parm = c("Sepal.Width", "Petal.Width"), level = 0.90)

## -----------------------------------------------------------------------------
predicted_values = predict(fit, test)
head(predicted_values)

## -----------------------------------------------------------------------------
predicted_values = predict(fit, test, confidence = TRUE, level = 0.90)
head(predicted_values)

## -----------------------------------------------------------------------------
library(titanic)

train = titanic_train[1:550,]
test = titanic_train[551:891,]

## -----------------------------------------------------------------------------
fit_logreg = blb_logreg(Survived ~ Age + Sex + Pclass, data = train)
fit_logreg

## -----------------------------------------------------------------------------
fit_logreg = blb_logreg(Survived ~ Age + Sex + Pclass, data = train, m = 8, B = 100, parallel = TRUE, cl = 4)
fit_logreg

## -----------------------------------------------------------------------------
print(fit_logreg)

## -----------------------------------------------------------------------------
coef(fit_logreg)

## -----------------------------------------------------------------------------
confint(fit_logreg)

## -----------------------------------------------------------------------------
confint(fit_logreg, parm = c("Age", "Sexmale"), level = 0.9)



confint(fit_logreg, parm = c("Age"), level = 0.9)

## -----------------------------------------------------------------------------
predicted_values = predict(fit_logreg, test)
head(predicted_values)

## -----------------------------------------------------------------------------
predicted_values = predict(fit_logreg, test, p = TRUE)
head(predicted_values)

