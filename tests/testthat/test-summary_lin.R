test_that("F_statistic works", {
  set.seed(20211206)

  X1 <- rnorm(500)
  X2 <- rnorm(500)
  Y <- 0.5*X1+ 2*X2 + rnorm(500,sd=10)

  lmod <- lm(Y ~ X1 + X2)

  X <- cbind(X1, X2)
  model <- lin.Reg(Y, X)
  conclusion = summary_lin.Reg(model)

  expect_equal("Predictors are significantly associated with Response (p <= 0.05)",
               conclusion)
})

test_that("F_statistic works_not_sig", {
  set.seed(20211206)

  Y <- rnorm(9)
  X <- Y+rnorm(9,sd=10)
  lmod <- lm(Y ~ X)
  model <- lin.Reg(Y,X)
  conclusion <- summary_lin.Reg(model)

  expect_equal("Predictors are NOT significantly associated with Response (p > 0.05)",
               conclusion)
})
