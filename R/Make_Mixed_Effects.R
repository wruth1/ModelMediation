
#' Generate random effects from given standard deviation matrix
#'
#' @param SD_mat q-by-q standard deviation matrix of random effects (i.e. Cholesky factor of estimated random effects covariance matrix), where q is the number of covariates with random effects
#' @param K Number of groups
#'
#' @return A list of random effect vectors. One element per group. Number of random effects to generate is inferred from the size of SD_mat
#' @export
#'
#' @examples
#' K=2
#' # Number of covariates with random effects in each group
#' q = 4
#' # SD matrix of random effects (i.e. Cholesky factor of its covariance matrix)
#' SD_mat = matrix(stats::rnorm(q^2), nrow = q, ncol = q)
#' all_REs = make_REs(K, SD_mat)
make_REs <- function(K, SD_mat){
  num_REs = nrow(SD_mat)

  U_by_K = replicate(K, {
    sph_RE = stats::rnorm(num_REs)
    RE = SD_mat %*% sph_RE
    return(RE)
  }, simplify = F)

  return(U_by_K)
}



#########################################################################################################
### This should be split into multiple functions. Particularly the extraction of random-only effects. ###
### In particular, splitting will better facilitate testing.                                          ###
#########################################################################################################


#' Augment vector of fixed effects by adding random effects to get mixed
#'
#' @param fix_effs Vector of fixed effects
#' @param ran_effs_by_K List containing one vector of random effects for each group
#' @param mix_eff_ind_pairs List of pairs of indices, (i,j), indicating which fixed (i) and random (j) effects have the same covariate. Note: Don't forget about the intercept if necessary
#'
#' @return A list containing one vector of fixed and mixed effects for each group.
#' @export
#'
#' @examples
#' fix_effs = c(1,2,3)
#' K = 2
#' ran_effs_by_K = list(1, 2)
#' mix_eff_ind_pairs = list(c(2,1))
#' fixed_plus_mixed(fix_effs, ran_effs_by_K, mix_eff_ind_pairs)
fixed_plus_mixed <- function(fix_effs, ran_effs_by_K, mix_eff_ind_pairs){
  # Construct list for mixed effect vectors ----
  K = length(ran_effs_by_K)
  mix_effs_by_K = rep(list(fix_effs), times = K)

  # Construct mixed effects by adding random effects to fixed effects in each group
  for(this_pair in mix_eff_ind_pairs){
    i = this_pair[1]
    j = this_pair[2]

    for(k in 1:K){
      mix_effs_by_K[[k]][i] = mix_effs_by_K[[k]][i] + ran_effs_by_K[[k]][j]
    }
  }

  return(mix_effs_by_K)
}


#' Construct mixed effects from given fixed and random effects
#'
#' @param fix_effs Vector of fixed effects
#' @param ran_effs_by_K List containing one vector of random effects for each group
#' @param mix_eff_ind_pairs List of pairs of indices, (i,j), indicating which fixed (i) and random (j) effects have the same covariate. Note: Don't forget about the intercept if necessary
#'
#' @return A list containing one vector of all regression coefficients for each group. Fixed and mixed effects come first, followed by any random-only effects.
#' @export
#'
#' @examples
#' fix_effs = c(1,2,3)
#' K = 2
#' ran_effs_by_K = list(1, 2)
#' mix_eff_ind_pairs = list(c(2,1))
#' make_MEs(fix_effs, ran_effs_by_K, mix_eff_ind_pairs)
make_MEs <- function(fix_effs, ran_effs_by_K, mix_eff_ind_pairs){

  # Construct mixed effects
  mix_effs_by_K = fixed_plus_mixed(fix_effs, ran_effs_by_K, mix_eff_ind_pairs)

  # Remove random effects with corresponding fixed effect ----

  ## Get indices of random effects to remove ----
  ## I.e. The random effect indices which show up in mix_eff_ind_pairs
  ME_inds_in_REs = get_index_vec(mix_eff_ind_pairs, i=2)

  ## Construct list of vectors containing random-only effects ----
  K = length(ran_effs_by_K)
  ran_only_effs_by_K = ran_effs_by_K
  for(k in 1:K){
    ran_only_effs_by_K[[k]] = delete_at(ran_effs_by_K[[k]], ME_inds_in_REs)
  }


  # Concatenate random effects onto end of vectors of fixed+mixed effects
  for(k in 1:K){
    mix_effs_by_K[[k]] = c(mix_effs_by_K[[k]], ran_only_effs_by_K[[k]])
  }

  return(mix_effs_by_K)

}
