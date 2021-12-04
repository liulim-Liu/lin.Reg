#'Display the result of a linear regression model
#'
#'This function take the output of lin.Reg as input
#'then display the results in better organization,
#'includes beta coefficents, standard errors, t test statistics and p values
#'For model evaluation, the goodness of fit metric also shown, such as
#'R Squared, Adjusted R Squared, Mean Standard Errors, F test statistics, and p-value
#'
#'@param lmod The output object returned from lin.Reg
#'
#'@return displayed result of the fitted model
#'
#'@export
#'

summary.lin.Reg <- function(lmod) {
  #1. Display the model fitted
  cat("Call:\nlin.Reg(Y = X * beta)\n")

  #2. Show the results summary
  cat("\nResiduals:\n")
  lmod$residuals = round(lmod$residuals,5)

  if(length(lmod$residuals)<=10) {
    cat("\nAll Results are:\n")
    names(lmod$residuals) = c(1:length(lmod$residuals))
    print(lmod$residuals)
  } else {
    cat("\nSummary Statistics of residuals:\n")
    print(round(summary(lmod$residuals),5))
  }

  #3. Display the estimations of coefficients
  cat("\nCoefficients:\n")
  coef_df = as.data.frame(list(round(lmod$coefficients, 5),
                               round(lmod$std.error,5),
                               round(lmod$t_statistic,5),
                               round(lmod$pt_value,5)),
                          row.names = c("(Intercept)",
                                        sprintf("X%d", 1:(lmod$q - 1))),
                          col.names = c("Coefficients",
                                        "Std.error",
                                        "t value",
                                        "P(<=t).value"))
  coef_df$sig <- ""
  coef_df$sig[coef_df$`P(<=t).value` <= 0.1] <- "*"
  coef_df$sig[coef_df$`P(<=t).value` <= 0.05] <- "**"
  coef_df$sig[coef_df$`P(<=t).value` <= 0.01] <- "***"

  print(coef_df)
  cat("\n\nSignificance: '***': <= 0.01, '**': <= 0.05, '*': <= 0.1")
  cat("\n\n---\n")

  #4. Display Goodness of Fit
  cat(sprintf("\nResidual standard error: %.5f on %d degrees of freedom",
              sqrt(lmod$SSE), (lmod$n-lmod$q)))
  cat(sprintf("\nMultiple R-squared: %.4f,\tAdjusted R-squared: %.4f",
              lmod$R_2,lmod$Adj_R_2))

  if(lmod$pf_value < 2.2e-16) {
    cat(sprintf("\nF-statistic: %.0f on %d and %d DF, p-value: < 2.2e-16",
                lmod$F_statistic, (lmod$q-1),(lmod$n-lmod$q)), "\n")
  }else {
    cat(sprintf("\nF-statistic: %.6f on %d and %d DF, p-value: %f",
                lm$F_statistic, (lmod$q-1),(lmod$n-lm$q),lmod$pf_value), "\n")
  }

  #5. Significance
  if(lmod$pf_value <= 0.05) return("Predictors are significantly associated with Response (p <= 0.05)")
  else {
    return("Predictors are NOT significantly associated with Response (p > 0.05)")
  }
}
