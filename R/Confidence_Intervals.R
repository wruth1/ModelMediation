

get_boot_med_effs <- function(boot_reg_coeffs){
  all_med_effs = boot_reg_coeffs %>%
    purrr::pmap_dfr(\(X_in_Y, M_in_Y, X_in_M, B, group) get_med_effs(X_in_Y, M_in_Y, X_in_M))

  output = data.frame(all_med_effs, B = boot_reg_coeffs$B, group = boot_reg_coeffs$group)
  return(output)
}

