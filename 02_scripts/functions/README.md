# Logistic Regression Functions

This directory contains reusable R functions developed for the manual
estimation of binary logistic regression models.

The functions implement the log-likelihood, score vector, Hessian matrix and
Newton-Raphson optimisation procedure associated with a binomial logit model.

## Main file

- `logit_funciones.R`

## Purpose

The purpose of `logit_funciones.R` is to provide a transparent implementation
of the main mathematical components involved in maximum-likelihood estimation
of binary logistic regression.

The functions can be sourced from other R scripts and used to estimate a
binary logistic regression model without relying directly on `glm()` for the
optimisation procedure.

The file is mainly intended for methodological, educational and verification
purposes.

## Statistical model

Let the dependent variable be:

- `y_i = 1` when the event of interest occurs.
- `y_i = 0` when the event of interest does not occur.

In this project, the event of interest is generally a home-team victory.

For observation `i`, the binary logistic regression model is:

`P(y_i = 1 | x_i) = exp(x_i' beta) / (1 + exp(x_i' beta))`

Equivalently:

`P(y_i = 1 | x_i) = 1 / (1 + exp(-x_i' beta))`

where:

- `x_i` is the vector of explanatory variables for observation `i`.
- `beta` is the vector of regression coefficients.
- `x_i' beta` is the linear predictor.

## Function overview

### `logit_logL()`

Calculates the log-likelihood of a binary logistic regression model.

General usage:

```r
logit_logL(
  beta,
  y,
  X
)
