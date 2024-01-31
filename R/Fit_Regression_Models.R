# Functions which fit regression models to the observed data to predict Y and M.
# Ultimately, I would like to create constructor functions which provide a high-level interface for people to specify the names of their outcome, treatment, mediator and confounder variables (as in the multimediate package), as well as which covariates have fixed and random effects. For now though, I'm just going to write these fitting functions explicitly.
# For now, this file must be updated anytime the model must be changed. Ultimately, these changes will be implemented by calling the constructor with new arguments.

# Really literally, these are just placeholder functions until I'm ready to start doing the analysis for real.






#' Fit a regression model to the observed data with outcome as response variable
#'
#' @param data The observed data. A data.frame-like object (e.g. a tibble).
#'
#' @return A GLMM fit using `glmer` from the `lme4` package.
#' @export
#'
#' @examples
#' data = 0
#' fit_mod_Y(data)
fit_mod_Y <- function(data){
  return(0)
}




#' Fit a regression model to the observed data with mediator as response variable
#'
#' @param data The observed data. A data.frame-like object (e.g. a tibble).
#'
#' @return A GLMM fit using `glmer` from the `lme4` package.
#' @export
#'
#' @examples
#' data = 0
#' fit_mod_M(data)
fit_mod_M <- function(data){
  return(0)
}
