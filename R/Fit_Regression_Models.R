# Functions which fit regression models to the observed data to predict Y and M.
# Ultimately, I would like to create constructor functions which provide a high-level interface for people to specify the names of their outcome, treatment, mediator and confounder variables (as in the multimediate package), as well as which covariates have fixed and random effects. For now though, I'm just going to write these fitting functions explicitly.
# For now, this file must be updated anytime the model must be changed. Ultimately, these changes will be implemented by calling the constructor with new arguments.






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
  suppressMessages(
    lme4::glmer(M ~ X + C1 + C2 + (X | group), data = data, family = "binomial")
  )
}




###############################
#### More general versions ####
###############################

# In this section, I make strong assumptions about the structure of the data frame which is passed to the fitting functions. I will refer to any data frame which must meet these assumptions as data_formal. The requirements are as follows:
#   Must contain columns named Y, M, X and group
#   May also contain other columns. Each such extra column must be a confounder.      !!!! I'm not sure yet whether I want to require that these columns be named C1, C2, ...


#' Fit regression model for mediator (`M`) using a formal dataset
#'
#' @details
#' I make strong assumptions about the structure of the data frame which is passed to this function. Such a structured data frame is referred to as `data_formal`. The requirements are as follows:
#'
#' - Must contain columns named Y, M, X and group
#' - May also contain other columns. Each such extra column must be a confounder.      !!!! I'm not sure yet whether I want to require that these columns be named C1, C2, ...
#'
#' @param data_formal A formal data frame. See Details for requirements.
#'
#' @return A GLMM fit using `glmer` from the `lme4` package.
#' @export
#'
#' @examples
#' n = 20
#' K = 3
#' all_reg_pars = make_all_reg_pars()
#' data_formal = make_validation_data(n, K, all_reg_pars)
#'
#' fit_mod_M_formal(data_formal)
fit_mod_M_formal <- function(data_formal){
  suppressMessages(
    lme4::glmer(M ~ . - Y + (X | group), data = data_formal, family = "binomial")
  )
}
