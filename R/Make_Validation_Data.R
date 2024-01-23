

#' Generate fixed covariates for validation dataset (i.e. intercept, X and C)
#'
#' @param K Number of groups
#' @param N Number of observations per group
#'
#' @return A list of tibbles. One entry for each group, containing all observations for that group.
#' @export
#'
#' @examples
#' K=2
#' N=10
#' build_valid_fixed_covs(K, N)
build_valid_fixed_covs <- function(K, N){
  preds_by_K = lapply(1:K, function(x){
    this_Xs = stats::rbinom(N, 1, 0.5)
    this_Cs = stats::rbinom(N, 1, 0.5)

    this_data = tibble::tibble(X = this_Xs, C = this_Cs)
    return(this_data)
  })
}


