# order are following the same as lin.Reg.R
test_that("vector to matrix convertion", {
  Y <- rnorm(10)
  X <- Y+rnorm(10,sd=2)

  lmod <- lm(Y ~ X)
  model <- lin.Reg(Y, X)

  expect_equal(unname(lmod$coefficients), unname(model$coefficients))
})

test_that("omit missing values in the data", {
  # missing in X
  Y <- c(1,2,3,4,5)
  X <- c(2,NA,6,8,10)
  lmod <- lm(Y ~ X)
  model<- lin.Reg(Y, X)
  expect_equal(unname(lmod$coefficients), unname(model$coefficients))

  # missing in Y
  Y <- c(1,2,NA,4,5)
  X <- c(2,4,6,8,10)
  lmod <- lm(Y ~ X)
  model<- lin.Reg(Y, X)
  expect_equal(unname(lmod$coefficients), unname(model$coefficients))
})

test_that("warning message check", {
  message <- function(code) {
    tryCatch(code,
             error = function(c) "Error",
             warning = function(c) "Warning",
             message = function(c) "Message"
    )
  }

  # check whether the rows of Y are the same as X
  Y = matrix(c(1,2,3,4,5),5,1)
  X = matrix(c(2,4,6,8),4,1)
  expect_equal("Error",message(lin.Reg(Y, X)))

  # check whether it has sufficient observations to build linear models
  Y = matrix(c(2,3),2,1)
  X = matrix(c(1,2,3,4,5,6),2,3)
  expect_equal("Error",message(linReg(Y, X)))
})

test_that("solve equation of coefficients", {
  # univariate
  Y <- matrix(c(1,2,3,4,5),5,1)
  X <- matrix(c(2,4,6,8,10),5,1)
  lmod <- lm(Y ~ X)
  model <- lin.Reg(Y, X)
  expect_equal(unname(lmod$coefficients), unname(model$coefficients))

  # multivariates
  set.seed(20211206)
  X1 = rnorm(500)
  X2 = rnorm(500)
  Y = 0.5*X1+ 2*X2 + rnorm(500,sd=10)
  lmod = lm(Y ~ X1 + X2)
  X= cbind(X1, X2)
  model = lin.Reg(Y,X)
  expect_equal(unname(lmod$coefficients), unname(model$coefficients))
})
