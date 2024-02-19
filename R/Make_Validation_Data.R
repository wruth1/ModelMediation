
#########################################################################################
#### To Do: Finish modifying functions to facilitate the returning of random effects ####
#########################################################################################










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
#' @param return_REs Should random effects also be returned? If TRUE, output is a list containing two components: `data` and `REs`.
#'
#' @return A simulated dataset, optionally inside a list which also contains random effects.
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
make_validation_data <- function(n, K, all_reg_pars = NULL, output_list = FALSE, return_REs = FALSE){
  if(is.null(all_reg_pars)) all_reg_pars = make_all_reg_pars()

  # Generate data as a list
  data_list_output = make_validation_data_list(n, K, all_reg_pars, return_REs)
  if(!return_REs){
    data_list = data_list_output
  } else{
    data_list = data_list_output["data"]
    all_REs = data_list_output["all_REs"]
  }


  # Optionally, stack groups into a single data.frame
  if(!output_list){
    data_output = list_2_data(data_list)
  } else{
    data_output = data_list
  }

  # Return data, optionally alongside random effects
  if(!return_REs){
    return(data_output)
  } else{
    return(list(data = data_output, REs = all_REs))
  }
}


make_validation_data_list <- function(n, K, all_reg_pars, return_REs = FALSE){
  output_list = purrr::map(1:K, ~ make_one_group_validation(n, all_reg_pars, return_REs))

  if(!return_REs){
    return(output_list)
  } else{
    data_list = purrr::map(output_list, "data")
    RE_list = purrr::map(output_list, "REs")
    return(list(data = data_list, REs = RE_list))
  }
}


make_one_group_validation <- function(n, all_reg_pars, return_REs = FALSE){
  X = make_X_validation(n)
  all_Cs = make_C_validation(n)

  M_info = make_M_validation(X, all_Cs, all_reg_pars, return_REs)

  if(!return_REs){
    M = M_info
  } else{
    M = M_info["M"]
    REs_M = M_info["REs"]
  }


  Y_info = make_Y_validation(M, X, all_Cs, all_reg_pars, return_REs)

  if(!return_REs){
    Y = Y_info
  } else{
    Y = Y_info["Y"]
    REs_Y = Y_info["REs"]
  }

  output_data = data.frame(Y=Y, M=M, X=X, all_Cs)

  if(!return_REs){
    return(output_data)
  } else{
    return(list(data = output_data, REs = list(M = REs_M, Y = REs_Y)))
  }
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



# I added the argument but haven't modified the function yet for returning REs !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
make_M_validation <- function(X, all_Cs, all_reg_pars, return_REs = FALSE){
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

# I added the argument but haven't modified the function yet for returning REs !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
make_Y_validation <- function(M, X, all_Cs, all_reg_pars, return_REs = FALSE){
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

