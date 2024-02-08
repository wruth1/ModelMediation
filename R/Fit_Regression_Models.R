# Functions which fit regression models to the observed data to predict Y and M.
# Ultimately, I would like to create constructor functions which provide a high-level interface for people to specify the names of their outcome, treatment, mediator and confounder variables (as in the multimediate package), as well as which covariates have fixed and random effects. For now though, I'm just going to write these fitting functions explicitly.
# For now, this file must be updated anytime the model must be changed. Ultimately, these changes will be implemented by calling the constructor with new arguments.


fix_package_version <- function(){
  oo <- options(repos = "https://cran.r-project.org/")
  install.packages("Matrix")
  install.packages("lme4")
  options(oo)
}




#' Fit a regression model to the observed data with outcome as response variable
#'
#' @param data The observed data. A data.frame.
#'
#' @return A GLMM fit using `glmer` from the `lme4` package.
#' @export
#'
#' @examples
#' n = 20
#' K = 3
#' all_reg_pars = make_all_reg_pars()
#' data = make_validation_data(n, K, all_reg_pars)
#'
#' fit_mod_Y(data)
fit_mod_Y <- function(data){
  fix_package_version()
  suppressMessages(
    lme4::glmer(Y ~ M + X + C1 + C2 + (M + X | group), data = data, family = "binomial")
  )
}




#' Fit a regression model to the observed data with mediator as response variable
#'
#' @param data The observed data. A data.frame.
#'
#' @return A GLMM fit using `glmer` from the `lme4` package.
#' @export
#'
#' @examples
#' n = 20
#' K = 3
#' all_reg_pars = make_all_reg_pars()
#' data = make_validation_data(n, K, all_reg_pars)
#'
#' fit_mod_M(data)
fit_mod_M <- function(data){
  fix_package_version()
  suppressMessages(
    lme4::glmer(M ~ X + C1 + C2 + (X | group), data = data, family = "binomial")
  )
}
