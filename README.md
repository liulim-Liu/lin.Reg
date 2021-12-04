# Univariate and Multivariate Linear Regression using `lin.Reg`

## Description

`lin.Reg ` is a simplified version of R function lm(), which stands for linear regression, and performs for both univariate and multivaraite linear regression models.

This package contains two functions:
- `lin.Reg(Y,X,has_b0)` reads response and predictors and estimate the coefficient of each predictors.
- `summary.lin.Reg(lmod)` reads the return value of `lin.Reg()` and display in a more organized manner. It also returns the evaluatio of the model, such as residual measures and goodness of fit.

## Installation
```r
# install useing devtools from GitHub website:
devtools::install_github("liulim-Liu/lin.Reg")

# you can also install with vignittes:
devtools::install_github("liulim-Liu/lin.Reg", build_vignettes = T)
```

## Usages
```r
library(lin.Reg)

# simulate the dataset
set.seed(2021)
X = matrix(rnorm(100), 50, 2)
Y = matrix((X[,1]+ 5*X[,2] + rnorm(50,sd=2)), 50, 1)

# fit a multivariate model
model_multi = lin.Reg(Y,X)

# display the summary
summary.lin.Reg(model_multi)

#> Call:
#> lin.Reg(Y = X * beta)
#> 
#> Residuals:
#> 
#> Summary Statistics of residuals:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> -5.1288 -1.2795  0.1966  0.0000  1.2236  5.7342 
#> 
#> Coefficients:
#>             Coefficients Std.error  t.value P...t..value sig
#> (Intercept)     -0.35998   0.35094 -1.02576      0.31025    
#> X1               0.70514   0.28991  2.43223      0.01887    
#> X2               4.60521   0.37173 12.38856      0.00000    
#> 
#> 
#> Significance: '***': <= 0.01, '**': <= 0.05, '*': <= 0.1
#> 
#> ---
#> 
#> Residual standard error: 15.67940 on 47 degrees of freedom
#> Multiple R-squared: 0.7806,  Adjusted R-squared: 0.7713
#> F-statistic: 83.609684 on 2 and 47 DF, p-value: 0.000000
#> [1] "Predictors are significantly associated with Response (p <= 0.05)"
```

## to-do:
2. fix the CMD error of no faraway model installation
3. unit testing


