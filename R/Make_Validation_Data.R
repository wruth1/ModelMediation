

#' Generate a dataset for validating methodology
#'
#' Generate a validation dataset with `n` observations in each of `K` groups. Variables include one outcome (Y), one exposure (X), one mediator (M) and two confounders (C1 and C2). All variables are binary. Default values are provided for regression parameters (fixed effects and covariances of random effects).
#'
#' @param n Number of observations in each group.
#' @param K Number of groups
#' @param all_reg_pars A named list containing fixed and mixed effects parameters for both regression models. Best obtained from `make_all_reg_pars()`.
#' @param output_list Should output be formatted as a list with one component per group (of size n-by-5) or a single tibble of size (Kn)-by-6 with a column labelled `group`?
#'
#' @return A simulated dataset.
#' @export
#'
#' @examples
#' n = 20
#' K = 3
#' all_reg_pars = make_all_reg_pars()
#'
#' # Format output as a list
#' make_validation_data(n, K, all_reg_pars, output_list = TRUE)
#'
#' # Format output as a tibble
#' make_validation_data(n, K, all_reg_pars, output_list = FALSE)
make_validation_data <- function(n, K, all_reg_pars = NULL, output_list = TRUE){
  if(is.null(all_reg_pars)) all_reg_pars = make_all_reg_pars()

  # Generate data as a list
  data_list = make_validation_data_list(n, K, all_reg_pars)


  # Optionally, stack groups into a single tibble
  if(!output_list){
    data_tibble = list_2_data(data_list)
    return(data_tibble)
  } else{
    return(data_list)
  }
}


make_validation_data_list <- function(n, K, all_reg_pars){
  purrr::map(1:K, ~ make_one_group_validation(n, all_reg_pars))
}


make_one_group_validation <- function(n, all_reg_pars){
  X = make_X_validation(n)
  all_Cs = make_C_validation(n)

  M = make_M_validation(X, all_Cs, all_reg_pars)
  Y = make_Y_validation(M, X, all_Cs, all_reg_pars)

  output = tibble::tibble(Y=Y, M=M, X=X, all_Cs)
  return(output)
}

make_X_validation <- function(n){
  return(stats::rbinom(n, 1, 0.5))
}

make_C_validation <- function(n){
  C1 = stats::rbinom(n, 1, 0.5)
  C2 = stats::rbinom(n, 1, 0.5)

  return(tibble::tibble(C1 = C1, C2 = C2))
}



make_M_validation <- function(X, all_Cs, all_reg_pars){
  beta_M = all_reg_pars$beta_M    # Coefficient vector for fixed effects
  Gamma_M = all_reg_pars$Gamma_M  # Covariance matrix of random effects

  n = length(X)
  if(nrow(all_Cs) != n) stop("In make_M_validation: X and all_Cs must have same number of rows.")

  # Organize X and all_Cs into datasets
  data_fix = data.frame(X = X, all_Cs)
  data_ran = data.frame(X = X)


<<<<<<< HEAD
  lin_preds = get_lin_preds(data_fix, data_ran, 1*beta_M, Gamma_M, add_intercept = TRUE)
  all_probs = boot::inv.logit(lin_preds)

  M = stats::rbinom(n, 1, all_probs)
=======
  lin_preds = get_lin_preds(data_fix, data_ran, 0.05*beta_M, Gamma_M, add_intercept = TRUE)
  all_probs = boot::inv.logit(lin_preds)

  M = rbinom(n, 1, all_probs)
>>>>>>> 212b18ba2b98b8ae401b92efd166ce624b31d186
  return(M)
}

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Placeholder
make_Y_validation <- function(M, X, C, all_reg_pars){
  n = length(M)
  if((length(X) != n) || (nrow(C) != n)) stop("In make_Y_validation: M, X and C must have same number of rows.")
  return(stats::rbinom(n, 1, 0.5))
}












# Helper Functions ----

#' Store all regression parameters in a single list
#'
#' @param beta_Y Vector of length 4 containing fixed-effects for the outcome model. Order of variables is: Intercept, X, M, C1, C2.
#' @param Gamma_Y Covariance matrix of size 3x3 containing random effects for the outcome model. Order of variables is: Intercept, X, M.
#' @param beta_M Vector of length 3 containing fixed-effects for the outcome model. Order is: Intercept, X, C1, C2.
#' @param Gamma_M Covariance matrix of size 2x2 containing random effects for the outcome model. Order of variables is: Intercept, X.
#'
#' @return A named list containing fixed and random effects parameters for both the outcome and mediator regression models. Names in list match names of arguments to this function.
#' @export
#'
#' @examples
#' make_all_reg_pars()
make_all_reg_pars <- function(beta_Y = 0.2 * c(1, -1, 1, -1, 1), Gamma_Y = 0.25 * diag(3), beta_M = 0.3 * c(-1, 1, -1, 1), Gamma_M = 0.25 * diag(2)){
  list(beta_Y = beta_Y, Gamma_Y = Gamma_Y, beta_M = beta_M, Gamma_M = Gamma_M)
}

