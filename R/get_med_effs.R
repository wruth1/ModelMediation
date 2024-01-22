#' Compute mediation effects based on regression parameters, respecting the variable types of Y and M
#'
#' @param X_in_Y Coefficient of X in the regression model for Y.
#' @param M_in_Y Coefficient of M in the regression model for Y.
#' @param X_in_M Coefficient of X in the regression model for M.
#' @param Y_binary Is the outcome variable binary (TRUE) or continuous (FALSE)
#' @param M_binary Is the mediator variable binary (TRUE) or continuous (FALSE)
#'
#' @return A vector of mediation effects on odds-ratio scale. Order is direct effect, indirect effect, total effect.
#' @export
#'
#' @examples
#' # Binary outcome, binary mediator
#' X_in_Y <- 1
#' M_in_Y <- 1
#' X_in_M <- 1
#' get_med_effs(X_in_Y, M_in_Y, X_in_M)
get_med_effs <- function(X_in_Y, M_in_Y, X_in_M, Y_binary=TRUE, M_binary=TRUE){
  if(Y_binary && M_binary){
    return(get_med_effs_binY_binM(X_in_Y, M_in_Y, X_in_M))
  }
}
