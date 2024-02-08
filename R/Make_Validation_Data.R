

#' Generate a dataset for validating methodology
#'
#' @description
#' Generate a validation dataset with `n` observations in each of `K` groups. Variables include one outcome (Y), one mediator (M), one exposure (X), and two confounders (C1 and C2). All variables are binary. Default values are available for regression parameters (fixed effects and covariances of random effects).
#'
#' Order of variables is `Y`, `M`, `X`, `C1`, `C2`
#'
#' @param n Number of observations in each group.
#' @param K Number of groups
#' @param all_reg_pars A named list containing fixed and mixed effects parameters for both regression models. Best obtained from `make_all_reg_pars()`.
#' @param output_list Should output be formatted as a list with one component per group (of size n-by-5) or a single data.frame of size (Kn)-by-6 with a column labelled `group`?
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
#' # Format output as a data.frame
#' make_validation_data(n, K, all_reg_pars, output_list = FALSE)
make_validation_data <- function(n, K, all_reg_pars = NULL, output_list = FALSE){
  if(is.null(all_reg_pars)) all_reg_pars = make_all_reg_pars()

  # Generate data as a list
  data_list = make_validation_data_list(n, K, all_reg_pars)


  # Optionally, stack groups into a single data.frame
  if(!output_list){
    data_stack = list_2_data(data_list)
    return(data_stack)
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

  output = data.frame(Y=Y, M=M, X=X, all_Cs)
  return(output)
}

make_X_validation <- function(n){
  return(stats::rbinom(n, 1, 0.5))
}

make_C_validation <- function(n){
  C1 = stats::rbinom(n, 1, 0.5)
  C2 = stats::rbinom(n, 1, 0.5)

  return(data.frame(C1 = C1, C2 = C2))
}



make_M_validation <- function(X, all_Cs, all_reg_pars){
  beta_M = all_reg_pars$beta_M    # Coefficient vector for fixed effects
  Gamma_M = all_reg_pars$Gamma_M  # Covariance matrix of random effects

  n = length(X)
  if(nrow(all_Cs) != n) stop("In make_M_validation: X and all_Cs must have same number of rows.")

  # Organize X and all_Cs into datasets
  data_fix = data.frame(X = X, all_Cs)
  data_ran = data.frame(X = X)


  lin_preds = get_lin_preds(data_fix, data_ran, beta_M, Gamma_M, add_intercept = TRUE)
  all_probs = boot::inv.logit(lin_preds)

  M = stats::rbinom(n, 1, all_probs)
  return(M)
}

make_Y_validation <- function(M, X, all_Cs, all_reg_pars){
  beta_Y = all_reg_pars$beta_Y    # Coefficient vector for fixed effects
  Gamma_Y = all_reg_pars$Gamma_Y  # Covariance matrix of random effects

  n = length(M)
  if((length(X) != n) || (nrow(all_Cs) != n)) stop("In make_Y_validation: M, X and all_Cs must have same number of rows.")

  # Organize M, X and all_Cs into datasets
  data_fix = data.frame(M = M, X = X, all_Cs)
  data_ran = data.frame(M = M, X = X)

  lin_preds = get_lin_preds(data_fix, data_ran, beta_Y, Gamma_Y, add_intercept = TRUE)
  all_probs = boot::inv.logit(lin_preds)

  Y = stats::rbinom(n, 1, all_probs)

  return(stats::rbinom(n, 1, 0.5))
}

