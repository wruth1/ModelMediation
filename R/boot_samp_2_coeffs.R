

boot_samp_2_coeffs <- function(boot_data){
  # Fit regression models to bootstrap sample
  mod_Y = fit_mod_Y(boot_data)
  mod_M = fit_mod_M(boot_data)

  # Extract coefficients for mediation analysis
  output = reg_coeffs_for_mediation(mod_Y, mod_M)
  return(output)
}



#' Extract regression coefficients which are relevant for mediation analysis
#'
#' @param mod_Y,mod_M Regression models for predicting the outcome and mediators respectively
#'
#' @return A data frame containing regression coefficients for mediation analysis, as well as group labels
#' @export
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
#' reg_coeffs_for_mediation(mod_Y, mod_M)
reg_coeffs_for_mediation <- function(mod_Y, mod_M){
  fix_coeffs = fix_coeffs_for_mediation(mod_Y, mod_M)

  mix_coeffs = mix_coeffs_for_mediation(mod_Y, mod_M)

  all_coeffs = rbind(fix_coeffs, mix_coeffs)
  return(all_coeffs)
}


# Fixed coefficients ----
fix_coeffs_for_mediation <- function(mod_Y, mod_M){
  fix_coeffs_Y = lme4::fixef(mod_Y)
  fix_coeffs_M = lme4::fixef(mod_M)

  output = data.frame(X_in_Y = fix_coeffs_Y["X"],
                      M_in_Y = fix_coeffs_Y["M"],
                      X_in_M = fix_coeffs_M["X"],
                      group = "fixed")
  rownames(output) = NULL
  return(output)
}



# Mixed coefficients (i.e. group-level) ----
mix_coeffs_for_mediation <- function(mod_Y, mod_M){
  mix_coeffs_Y = stats::coefficients(mod_Y)[[1]]
  mix_coeffs_M = stats::coefficients(mod_M)[[1]]

  mix_coeffs_2_data(mix_coeffs_Y, mix_coeffs_M)
}

mix_coeffs_2_data <- function(mix_coeffs_Y, mix_coeffs_M){
  all_mix_coeffs = cbind(Y=mix_coeffs_Y, M=mix_coeffs_M)
  all_mix_coeffs$group = rownames(all_mix_coeffs)

  output = all_mix_coeffs %>%
    dplyr::select(Y.M, Y.X, M.X, group) %>%
    dplyr::rename(X_in_Y = "Y.X", M_in_Y = "Y.M", X_in_M = "M.X")
  rownames(output) = NULL

  return(output)
}
