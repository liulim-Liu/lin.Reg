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

# create test data:
set.seed(2021)
X1 = rnorm(100)
X2 = rnorm(100)
Y = X1+ 5*X2 + rnorm(100,sd=2)
X= cbind(X1,X2)

# fit linear model:
model = lin.Reg(Y,X)

# check the results:
summary.lin.Reg(model)

```

## to-do:
1. add the result of example
2. fix the CMD error of no faraway model installation
3. unit testing


