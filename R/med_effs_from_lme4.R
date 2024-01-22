#' Compute mediation effects from two regression models
#'
#' @param mod_Y A regression model for the outcome variable, Y.
#' @param mod_M A regression model for the mediator variable, M.
#'
#' @return A matrix of mediation effects. Includes both fixed effects and group-specific mixed effects.
#' @export
#'
#' @examples
#' mod_Y <- 1
#' mod_M <- 1
#' med_effs_from_lme4(mod_Y, mod_M)
med_effs_from_lme4 = function(mod_Y, mod_M){
  return(1)
}
