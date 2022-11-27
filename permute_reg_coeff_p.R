#' Obtain permuted p-value for partial regression coefficient
#' 
#' Calculate permuted p-value for a partial regression coefficient based using
#' formula outlined by Manly et al. (1997), whereby permuted p-value is equal to
#' the probability of a randomly permuted regression coefficients having an 
#' absolute value greater than or equal to the absolute value of the observed
#' regression coefficient.
#' 
#' @param reg_form String specifying the regression formula in the format 'DV
#'  ~ IV1 + IV2 ... + IV3'.
#' @param var String specifying the name of the variable for which the partial
#' regression coefficient will be obtained.
#' @param data Dataframe containing data for regression model.
#' @param n_perms Integer specifying the number of iterations of random
#' permutation.
#' @param reg_type String specifying type of regression model (OLS - lm) or
#' robust regression (rlm)
#'  
#' @return Integer containing permuted p-value for partial regression 
#' coefficient.
#' @export
#' 
#' @examples
#' permuted_p <- permute_reg_coeff_p('mpg ~ cyl + disp + hp + wt', 'hp', mtcars)
#'  
#' @author Rory Boyle rorytboyle[at]gmail.com
#' @references Manly B. F. J. (1997) Randomization, bootstrap and Monte Carlo
#' methods in biology. (2nd ed.) London: Chapman & Hall.
#' @references Anderson & Legendre (1999) An empirical comparison of permutation 
#' methods for test of partial regression coefficients in a linear model. 
#' <https://doi.org/10.1080/00949659908811936>
#' 
#' @seealso <https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/rlm.html>
permute_reg_coeff_p <- function(reg_form, var, data, n_perms=5000, reg_type='OLS') {

# Get necessary libraries
library(modelr); library(purrr)
  
# Permute dependent variable (Y)
perm_data <- permute(data, n_perms, var)

# Run observed/actual regression model and permuted regression models
if (reg_type == 'OLS') {
  # Run observed model
  obs_model <- lm(reg_form, data = data)
  
  # Run permuted regressions
  perm_models <- map(perm_data$perm, ~ lm(reg_form, data = .))
  
} else if (reg_type == 'ROBUST') {
  # Load MASS package
  library(MASS)
  
  # Convert to formula object otherwise rlm() will return error
  robust_reg_form <- as.formula(reg_form)
  
  # Run observed model
  obs_model <- rlm(robust_reg_form, data = data)
  
  # Run permuted regressions
  perm_models <- map(perm_data$perm, ~ rlm(robust_reg_form, data = .))
}

# Obtain observed coefficient
obs_coeff <- obs_model$coefficients[var]

# Extract coefficient values
perm_coeffs_all <- lapply(perm_models, '[[', 'coefficients')
perm_coeffs <- unlist(lapply(perm_coeffs_all, '[[', var))

# Show location of observed p-value in empirical null distribution
hist(perm_coeffs)
abline(v=obs_coeff, col='red', lwd=3)

# Calculate permuted p value
perm_p <- sum(abs(perm_coeffs) >= abs(obs_coeff)) / length(perm_coeffs)

return(perm_p)
}
