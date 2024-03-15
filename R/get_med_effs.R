
#' Compute mediation effects from regression coefficients
#'
#' @description
#' `get_med_effs_DF()` takes as input a data frame of regression coefficients. Specifically, this data frame must contain variables called `X_in_Y`, `M_in_Y` and `X_in_M`. Optionally, this data frame may contain other identifying variables, such as group label or bootstrap replicate number. These identifying variables are included in the output.
#'
#' `get_med_effs_lme4()` takes as input two regression models fit using the `lme4` package, one for predicting the outcome variable, `Y`, the other for predicting the mediator variable, `M`. Relevant coefficients and group labels are extracted from these model objects, and are then passed to `get_med_effs_DF()` to compute mediation effects. Final output contains a column of group labels and no other identifying variables.
#'
#'
#' @param all_reg_coeffs A data frame containing regression coefficients for mediation and, optionally, one or more other identifying variables.
#' @param mod_Y,mod_M Regression models for predicting the outcome and mediators respectively
#' @name mult_med_effs
#'
#' @return A data frame of mediation effects. Optionally also contains other identifying variables (e.g. group labels)
#'
#' @examples
#' n = 20
#' K = 3
#' all_reg_pars = make_all_reg_pars()
#' data = make_validation_data(n, K, all_reg_pars)
#'
#' mod_Y = fit_mod_Y(data)
#' mod_M = fit_mod_M(data)
#'
#' all_reg_coeffs = reg_coeffs_for_mediation(mod_Y, mod_M)
#' get_med_effs_DF(all_reg_coeffs)
#'
#' # get_med_effs_lme4 combines the previous two steps
#' get_med_effs_lme4(mod_Y, mod_M)



#' @rdname mult_med_effs
#' @export
get_med_effs_DF <- function(all_reg_coeffs){
  # Compute mediation effects ----
  all_med_effs = suppressMessages(
    all_reg_coeffs %>%
      purrr::pmap(\(X_in_Y, M_in_Y, X_in_M, ...) data.frame(get_med_effs(X_in_Y, M_in_Y, X_in_M))) %>% # Everything below this just fixes formatting
      purrr::list_cbind() %>%
      t() %>%
      data.frame() %>%
      tibble::remove_rownames()
  )

  # Restore identifying variables ----
  if(ncol(all_reg_coeffs) > 3){
    extra_vars = dplyr::select(all_reg_coeffs, -X_in_Y, -M_in_Y, -X_in_M)
    output = data.frame(all_med_effs, extra_vars)
  } else{
    output = all_med_effs
  }

  return(output)
}

#' @rdname mult_med_effs
#' @export
get_med_effs_lme4 <- function(mod_Y, mod_M){
  all_reg_coeffs = reg_coeffs_for_mediation(mod_Y, mod_M)
  all_med_effs = get_med_effs_DF(all_reg_coeffs)

  return(all_med_effs)
}


# Reformat mediation effects from output of get_med_effs_X() to input for get_boot_CIs()
med_effs_wide_2_tall <- function(wide_med_effs){
  wide_med_effs %>%
    tidyr::pivot_longer(!group, names_to = "med_type", values_to = "estimate")
}



#' Compute mediation effects with binary response and binary mediator
#'
#' @param X_in_Y Coefficient of X in the regression model for Y.
#' @param M_in_Y Coefficient of M in the regression model for Y.
#' @param X_in_M Coefficient of X in the regression model for M.
#'
#' @return A (named) vector of mediation effects on odds-ratio scale. Order is direct effect, indirect effect, total effect.
#' @export
#'
#' @examples
#' X_in_Y <- 1
#' M_in_Y <- 1
#' X_in_M <- 1
#' get_med_effs(X_in_Y, M_in_Y, X_in_M)
get_med_effs <- function(X_in_Y, M_in_Y, X_in_M){
  de = exp(X_in_Y)
  ie = exp(X_in_M * M_in_Y)
  te = exp(X_in_Y + X_in_M * M_in_Y)

  output = c(de, ie, te)
  names(output) = c("de", "ie", "te")

  return(output)
}



# dplyr::filter(boot_output, group=="fixed")
