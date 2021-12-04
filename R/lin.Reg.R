#'Linear Regression
#'
#'This function implements the linear regression models.
#'It can work for both univariate linear regression, as well as multivariate linear regression.
#'
#'@param Y A vector or a single column matrix response variable.
#'@param X A vector for univariate or matrix for multivariate of predictors.
#'@param has_b0 A binary variable indicates whether an intercept is included.
#'
#'@return A list of attributes from the regression model.
#'
#'
#'@export
#'


lin.Reg <- function(Y, X, has_b0 = TRUE) { #function with a default setting assume it contains intercept
  # ---------- Validation and Cleaning ---------- #
  # 1. check whether the input X and Y are valid (not empty)
  # 2. convert vector to matrix
  if(is.null(attributes(Y))) {
    attr(Y, "dim") = c(length(Y), 1)
  }

  if(is.null(attributes(X))) {
    attr(X, "dim") = c(length(X), 1)
  }

  # 3. omit missing values in the data
  if(sum(is.na(X)) > 0) {
    Y = Y[-unique(which(is.na(X), arr.ind = TRUE)[1]),, drop=FALSE]
    X = X[-unique(which(is.na(X), arr.ind = TRUE)[1]),, drop=FALSE]
  } else if(sum(is.na(Y)) > 0) {
    X = X[-which(is.na(Y)),, drop=FALSE]
    Y = Y[-which(is.na(Y)),, drop=FALSE]
  }

  # 4. check whether the rows of Y are the same as X
  if(nrow(Y) != nrow(X)) {
    stop("Error: Inconsistant number of observations.")
  }

  # 5. check whether it has sufficient observations to build linear models
  if(nrow(X) < ncol(X)) {
    stop("Error: Insufficient number of observations.")
  }


  # ---------- Processing the Preparation ---------- #
  # Design matrix
  if(has_b0) {
    X = cbind(rep(1, nrow(X)), X)
  }
  a = t(X) %*% X
  b = t(X) %*% Y


  # ---------- Coefficients ---------- #
  # solve equation of coefficients
  # save estimation in names of beta
  beta = solve(a, b, tol = 0, transpose = FALSE)
  Y_hat = X %*% beta


  # ---------- Metrics ---------- #
  n = nrow(Y)
  q = ncol(X)

  # 1. goodness of fit
  residuals = Y[,, drop=TRUE] - Y_hat[,, drop=TRUE]
  SSE = sum(residuals * residuals) #sum of square errors
  SST = sum((Y-mean(Y)) * (Y-mean(Y))) #sum of square total
  SSR = SST - SSE #sum of square regression
  MSE = SSE/(n-q) #mean square errors
  R_2 = SSR/SST
  Adj_R_2 = 1- MSE/(SST/(n-1))

  # 2. variance matrix
  v_mat = solve(a, diag(MSE, q, q), tol = 0, transpose = FALSE)
  std.error = c(sqrt(diag(v_mat)))
  t_statistic = beta[,, drop=TRUE] / std.error
  pt_value = 2 * pt(-abs(t_statistic), (n - q))

  #3. F-test statistics
  F_statistic = SSR / (q-1) / MSE
  pf_value = pf(F_statistic, (q-1), (n-q), lower.tail = FALSE)


  # ---------- Output the results ---------- #
  beta = beta[,,drop=TRUE]
  names(beta)[1] = ("(Intercept)")

  return(list(coefficients = beta, n=n, q=q,
              residuals=residuals, SSE=SSE, SSR=SSR,SST=SST, MSE=MSE,
              R_2=R_2, Adj_R_2=Adj_R_2,
              v_mat=v_mat, std.error=std.error,
              t_statistic=t_statistic, pt_value=pt_value,
              F_statistic=F_statistic, pf_value=pf_value))
}
