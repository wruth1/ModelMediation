

boot_samp_2_coeffs <- function(boot_data){
  # Fit regression models to bootstrap sample
  mod_Y = fit_mod_Y(boot_data)
  mod_M = fit_mod_M(boot_data)

  # Extract coefficients for mediation analysis
  output = reg_coeffs_for_mediation(mod_Y, mod_M)
  return(output)
}

reg_coeffs_for_mediation <- function(mod_Y, mod_M){
  # fix_coeffs = fix_coefs_for_mediation(mod_Y, mod_M)
  #
  # mix_coefs = mix_coefs_for_mediation(mod_Y, mod_M)
  #
  # output = combine_fix_mix_coefs(fix_coefs, mix_coefs)
  # return(output)
  return(0)
}

#
#
#
#
# reg_coeffs_for_mediation <- function(mod_Y, mod_M){
#   X_in_Y = get_X_coeff(mod_Y)
#   M_in_Y = get_M_coeff(mod_Y)
#   X_in_M = get_X_coeff(mod_M)
#
#   output = c(X_in_Y, M_in_Y, X_in_M)
#   names(output) = c("X_in_Y", "M_in_Y", "X_in_M")
#
#   return(output)
# }
#
#
# get_X_coeff <- function(reg_model){
#
# }
