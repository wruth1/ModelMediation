


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
make_validation_data <-
  function(n,
           K,
           all_reg_pars = NULL,
           output_list = FALSE,
           return_REs = FALSE) {
    if (is.null(all_reg_pars))
      all_reg_pars = make_all_reg_pars()

    # Generate data as a list
    data_list_output = make_validation_data_list(n, K, all_reg_pars, return_REs)
    if (!return_REs) {
      data_list = data_list_output
    } else{
      data_list = data_list_output[["data"]]
      all_REs = data_list_output[["all_REs"]]
    }


    # Optionally, stack groups into a single data.frame
    if (!output_list) {
      data_output = list_2_data(data_list)
    } else{
      data_output = data_list
    }

    # Return data, optionally alongside random effects
    if (!return_REs) {
      return(data_output)
    } else{
      return(list(data = data_output, all_REs = all_REs))
    }
  }


make_validation_data_list <-
  function(n, K, all_reg_pars, return_REs = FALSE) {
    output_list = purrr::map(1:K,
                             ~ make_one_group_validation(n, all_reg_pars, return_REs))

    if (!return_REs) {
      return(output_list)
    } else{
      data_list = purrr::map(output_list, "data")
      RE_list = purrr::map(output_list, "REs")
      return(list(data = data_list, all_REs = RE_list))
    }
  }


make_one_group_validation <-
  function(n, all_reg_pars, return_REs = FALSE) {
    X = make_X_validation(n)
    all_Cs = make_C_validation(n)

    M_info = make_M_validation(X, all_Cs, all_reg_pars, return_REs)

    if (!return_REs) {
      M = M_info
    } else{
      M = M_info[["M"]]
      REs_M = M_info[["REs"]]
    }



    Y_info = make_Y_validation(M, X, all_Cs, all_reg_pars, return_REs)

    if (!return_REs) {
      Y = Y_info
    } else{
      Y = Y_info[["Y"]]
      REs_Y = Y_info[["REs"]]
    }

    output_data = data.frame(Y = Y, M = M, X = X, all_Cs)

    if (!return_REs) {
      return(output_data)
    } else{
      return(list(
        data = output_data,
        REs = list(M = REs_M, Y = REs_Y, data = output_data)
      ))
    }
    return(output)
  }

make_X_validation <- function(n) {
  return(stats::rbinom(n, 1, 0.5))
}

make_C_validation <- function(n) {
  C1 = stats::rbinom(n, 1, 0.5)
  C2 = stats::rbinom(n, 1, 0.5)

  return(data.frame(C1 = C1, C2 = C2))
}



make_M_validation <-
  function(X, all_Cs, all_reg_pars, return_REs = FALSE) {
    beta_M = all_reg_pars$beta_M    # Coefficient vector for fixed effects
    Gamma_M = all_reg_pars$Gamma_M  # Covariance matrix of random effects

    n = length(X)
    if (nrow(all_Cs) != n)
      stop("In make_M_validation: X and all_Cs must have same number of rows.")

    # Organize X and all_Cs into datasets
    data_fix = data.frame(X = X, all_Cs)
    data_ran = data.frame(X = X)


    lin_pred_info = get_lin_preds(
      data_fix,
      data_ran,
      beta_M,
      Gamma_M,
      add_intercept = TRUE,
      return_REs = return_REs
    )
    if (!return_REs) {
      lin_preds = lin_pred_info
    } else{
      lin_preds = lin_pred_info[["lin_preds"]]
      REs = lin_pred_info[["REs"]]
    }

    all_probs = boot::inv.logit(lin_preds)

    M = stats::rbinom(n, 1, all_probs)

    if (!return_REs) {
      return(M)
    } else{
      return(list(M = M, REs = REs))
    }
  }

make_Y_validation <-
  function(M, X, all_Cs, all_reg_pars, return_REs = FALSE) {
    beta_Y = all_reg_pars$beta_Y    # Coefficient vector for fixed effects
    Gamma_Y = all_reg_pars$Gamma_Y  # Covariance matrix of random effects

    n = length(M)
    if ((length(X) != n) ||
        (nrow(all_Cs) != n))
      stop("In make_Y_validation: M, X and all_Cs must have same number of rows.")

    # Organize M, X and all_Cs into datasets
    data_fix = data.frame(M = M, X = X, all_Cs)
    data_ran = data.frame(M = M, X = X)

    lin_pred_info = get_lin_preds(
      data_fix,
      data_ran,
      beta_Y,
      Gamma_Y,
      add_intercept = TRUE,
      return_REs = return_REs
    )
    if (!return_REs) {
      lin_preds = lin_pred_info
    } else{
      lin_preds = lin_pred_info[["lin_preds"]]
      REs = lin_pred_info[["REs"]]
    }

    all_probs = boot::inv.logit(lin_preds)

    Y = stats::rbinom(n, 1, all_probs)

    if (!return_REs) {
      return(Y)
    } else{
      return(list(Y = Y, REs = REs))
    }
  }



# Generic simulators for GLMM response ----

# I must have thought these were a good idea at some point, but they're not currently being used.

# make_response_RE_cov <-
#   function(data_fix, data_ran, beta_fix, Gamma) {
#     lin_preds = get_lin_preds_RE_cov(data_fix, data_ran, beta_fix, Gamma)
#     all_probs = boot::inv.logit(lin_preds)
#     response = stats::rbinom(nrow(data_fix), 1, all_probs)
#     return(response)
#   }
#
#
# make_response_RE_vec <-
#   function(data_fix, data_ran, beta_fix, beta_ran) {
#     lin_preds = get_lin_preds_RE_vec(data_fix, data_ran, beta_fix, beta_ran)
#     all_probs = boot::inv.logit(lin_preds)
#     response = stats::rbinom(nrow(data_fix), 1, all_probs)
#     return(response)
#   }





# one_par_boot_dataset <- function(mod_Y, mod_M) {
#   # Start with M ----
#
#   ## Compute contribution to the linear predictor of the fixed and random effects separately ----
#
#   data_fix_M = stats::model.matrix(mod_M, type = "fixed")
#   data_ran_M = stats::model.matrix(mod_M, type = "random") %>% as.matrix() %>% as.data.frame()
#
#
#   ## Fixed ----
#   fixed_contrib_M = data_fix_M %*% lme4::fixef(mod_M)
#
#
#   ## Random ----
#   ran_coef_mat_M = lme4::ranef(mod_M)[[1]]
#
#   all_ran_coefs_M = c()
#   for (i in 1:nrow(ran_coef_mat)) {
#     all_ran_coefs_M = c(all_ran_coefs_M, as.numeric(ran_coef_mat_M[i, ]))
#   }
#
#   ran_contrib_M = as.matrix(data_ran_M) %*% all_ran_coefs_M
#
#
#   ## Simulate new M ----
#   lin_preds_M = fixed_contrib_M + ran_contrib_M
#   probs_M = boot::inv.logit(lin_preds_M)
#   new_M = stats::rbinom(nrow(data_fix_M), 1, probs_M)
#
#
#   # Now onto Y ----
#   data_fix_Y = stats::model.matrix(mod_Y, type = "fixed")
#   data_ran_Y = stats::model.matrix(mod_Y, type = "random") %>% as.matrix() %>% as.data.frame(check.names = T)
#
#
#   ## Inject simulated M into the data for Y ----
#
#   ### Fixed-effects data ----
#   data_fix_Y[, "M1"] = new_M
#
#
#   ### Random effects data ----
#   new_M_by_group = split(new_M, real_data$group)
#   q = ncol(data_ran_Y) / length(all_groups)
#   n = nrow(data_ran_Y) / length(all_groups)
#
#   for (i in 1:length(all_groups)) {
#     col_ind = q * (i-1) + 2
#     row_start = n * (i-1) + 1
#     row_end = n * i
#
#     this_group = all_groups[i]
#     this_M = new_M_by_group[[this_group]]
#
#     data_ran_Y[row_start:row_end, col_ind] = this_M
#   }
#
#
#   ## Compute contributions to the linear predictor ----
#
#   fixed_contrib_Y = data_fix_Y %*% lme4::fixef(mod_Y)
#
#
#   ran_coef_mat_Y = lme4::ranef(mod_Y)[[1]]
#
#   all_ran_coefs_Y = c()
#   for (i in 1:nrow(ran_coef_mat)) {
#     all_ran_coefs_Y = c(all_ran_coefs_Y, as.numeric(ran_coef_mat_Y[i, ]))
#   }
#
#   ran_contrib_Y = as.matrix(data_ran_Y) %*% all_ran_coefs_Y
#
#
#
#   ## Simulate new Y ----
#   lin_preds_Y = fixed_contrib_Y + ran_contrib_Y
#   probs_Y = boot::inv.logit(lin_preds_Y)
#   new_Y = stats::rbinom(nrow(data_fix_Y), 1, probs_Y)
#
#
#
#   # Construct and return new dataset ----
#   data_new = real_data
#   data_new$Y = new_Y
#   data_new$M = new_M
#   return(data_new)
# }
