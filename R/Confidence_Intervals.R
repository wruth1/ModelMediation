
#' Construct bootstrap confidence intervals
#'
#' @description
#' Construct percentile or basic bootstrap confidence intervals (or both) from the provided bootstrap sample. Note: If basic intervals are requested then either the mediation effects or regression models fit the the observed data must be supplied.
#'
#' Design note: I allow fitted effects from the observed data to be passed as mediation effects but not as regression coefficients. This avoids any ambiguity about whether regression coefficients can be a raw dump from `mod_Y` and `mod_M`. I can imagine relaxing this requirement, but it would require more input checking and I don't want to do that right now.
#'
#'
#' @param boot_reg_coeffs A bootstrap sample of regression coefficients
#' @param type Which type(s) of confidence interval should be constructed? Currently only `"percentile"` and `"basic"` are supported.
#' @param fitted_med_effs Data frame of mediation effects obtained from the observed data (i.e. not from any bootstrap sample).
#' @param mod_Y,mod_M Regression models fit to the observed data (i.e. not from any bootstrap sample) for predicting the outcome and mediators respectively.
#'
#' @return A data frame containing bootstrap CIs for each group and mediation effect. Columns include:
#'  * Group label (`group`)
#'  * Type of mediation effect (`med_type`, values include `de`, `ie` and `te`)
#'  * Type of confidence interval (`CI_type`, values include `pct` and `bas`)
#'  * Lower and upper confidence limits (`lcl` and `ucl` respectively)
#'  * If available, the point estimate for each mediation effect on each group (`estimate`, repeated for the two types of interval if both are requested)
#' @export
#'
#' @examples
#' 1+1
get_boot_CIs <- function(boot_reg_coeffs, type = c("percentile", "basic"), fitted_med_effs = NULL, mod_Y = NULL, mod_M = NULL){
  # Ensure enough information has been supplied ----
  models_present = !is.null(mod_Y) && !is.null(mod_M)
  if("basic" %in% type){
    if(!is.null(fitted_med_effs)){
      # Do nothing
    } else if(models_present){
      # Do nothing
    } else if(models_present && !is.null(fitted_med_effs)){
      stop("Please provide only one of fitted_med_effs or both mod_Y and mod_M")
    } else{
      stop("Insufficient information for constructing basic bootstrap intervals. Please supply either fitted_med_effs or both mod_Y and mod_M.")
    }
  }



  # Get percentile intervals ----
  # Note: These intervals are required to compute basic intervals too, so we compute them whether they will be in the final output or not.
  boot_med_effs = get_boot_med_effs(boot_reg_coeffs)
  percentile_CIs = get_percentile_CIs(boot_med_effs)

  # If possible, add estimates from the observed data to output----
  if(models_present){
    estimates = get_med_effs_lme4(mod_Y, mod_M) %>%
      med_effs_wide_2_tall()
    percentile_CIs = dplyr::full_join(percentile_CIs, estimates, by = c("group", "med_type"))
  } else if(!is.null(fitted_med_effs)){
    estimates = med_effs_wide_2_tall(fitted_med_effs)
    percentile_CIs = dplyr::full_join(percentile_CIs, estimates, by = c("group", "med_type"))
  }

  # Add label for percentile intervals
  percentile_CIs$CI_type = "pct"

  # If only percentile intervals are requested, we're done ----
  if(identical(type, "percentile")) return(percentile_CIs)

  # Construct basic CIs ----
  basic_CIs = percentile_CIs %>%
    dplyr::mutate(lcl_new = 2*estimate - ucl, ucl_new = 2*estimate - lcl, estimate = estimate, .keep = "unused") %>%
    dplyr::rename(lcl = lcl_new, ucl = ucl_new) %>%
    dplyr::mutate(CI_type = "bas") %>%
    dplyr::relocate(estimate, CI_type, .after=last_col())

  # If only basic intervals are requested, we're done
  if(identical(type, "basic")) return(basic_CIs)


  # Combine both intervals
  all_intervals = dplyr::full_join(percentile_CIs, basic_CIs, by = c("group", "med_type"))
  all_intervals = rbind(percentile_CIs, basic_CIs)

  return(all_intervals)
}




get_boot_med_effs <- function(boot_reg_coeffs){

  all_med_effs = get_med_effs_DF(boot_reg_coeffs)

  output = data.frame(all_med_effs, group = boot_reg_coeffs$group, b = boot_reg_coeffs$b)
  return(output)
}


# med_effs_par = get_boot_med_effs(boot_results_par)



# Construct percentile CIs for mediation effects across groups
#
# param: `boot_med_effs` Data frame containing mediation effects computed from a bootstrap sample.
#
# return: A data frame contining group labels (`group`), mediation type labels (`med_type`) and lower/upper confidence limits (`lcl` and `ucl` respectively).
get_percentile_CIs <- function(boot_med_effs){
  boot_med_effs %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(de_lcl = stats::quantile(de, 0.025), de_ucl = stats::quantile(de, 0.975),
                     ie_lcl = stats::quantile(ie, 0.025), ie_ucl = stats::quantile(ie, 0.975),
                     te_lcl = stats::quantile(te, 0.025), te_ucl = stats::quantile(te, 0.975)) %>%
    tidyr::pivot_longer(!group, names_to = c("med_type", "endpoint"), names_sep = "_") %>%
    tidyr::pivot_wider(names_from = "endpoint", values_from = "value")

}


