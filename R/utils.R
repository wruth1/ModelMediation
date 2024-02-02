

# list_2_data and its helpers ----


#' Construct a single data frame by `rbind`-ing elements of a list. Add/Create group labels.
#'
#' @param X_list A list of data frames. Must all have the same number of columns.
#' @param group_labels Optionally, a vector of labels for each element of `X_list`. Must be either the same length as `X_list` or equal to the total number of rows among elements in `X_list`. If `NULL`, labels are `G1`, `G2`,...
#'
#' @return A data frame containing all elements of `X_list` with an extra column for labels.
#' @export
#'
#' @examples
#' data = as.data.frame(matrix(c(1,0,0,1), nrow = 2))
#' more_data = as.data.frame(matrix(c(0,1,1,0), nrow = 2))
#' data_list = list(data, more_data)
#'
#' list_2_data(data_list)
#'
#' data_names = c("Alice", "Bob")
#' list_2_data(data_list, data_names)
list_2_data <- function(X_list, group_labels = NULL){
  # Stack elements of X_list ----
  X_data = purrr::list_rbind(X_list)

  # Create group labels ----
  all_labels = list_2_data_make_labels(X_list, group_labels)

  X_data$group = all_labels

  return(X_data)
}






#' Create group labels
#'
#' @param X_list A list of data frames.
#' @param group_labels Optionally, a vector of labels for each element of `X_list`. Must be either the same length as `X_list` or equal to the total number of rows among elements in `X_list`. If `NULL`, labels are `G1`, `G2`,...
#'
#' @return A vector of group labels with length equal to the total number of rows among elements of `X_list`.
#' @export
#' @keywords internal
#'
#' @examples
#' data = as.data.frame(matrix(c(1,0,0,1), nrow = 2))
#' more_data = as.data.frame(matrix(c(0,1,1,0), nrow = 2))
#' data_list = list(data, more_data)
#'
#' list_2_data_make_labels(data_list)
#'
#' data_names = c("Alice", "Bob")
#' list_2_data_make_labels(data_list, data_names)
list_2_data_make_labels <- function(X_list, group_labels = NULL){
  group_sizes = purrr::map_int(X_list, nrow)

  if(is.null(group_labels)){                              # Nothing supplied. Construct names and labels.
    K = length(X_list)
    group_names = paste0("G", 1:K)
    all_labels = rep(group_names, times = group_sizes)
  } else if(length(group_labels) == length(X_list)){      # Group names supplied. Construct labels.
    all_labels = rep(group_labels, times = group_sizes)
  } else if(length(group_labels) == sum(group_sizes)){        # Labels supplied.
    all_labels = group_labels
  } else{                                                 # Non-conformable arguments.
    stop("In list_2_data: Non-conformable sizes between X_list and group_labels.")
  }

  return(all_labels)
}







# Compute linear predictors ----



#' Compute the contribution to the linear predictor due to the provided dataset and coefficient vector
#'
#' @param data A dataset. Can be a data frame or a matrix (former is converted to latter internally)
#' @param beta A vector of coefficients. Length must either match number of columns of `data` or be 1 less, if `add_intercept` is `FALSE` or `TRUE` respectively. In the latter case, intercept should be the first element of beta.
#' @param add_intercept Should `data` be augmented with a column of `1`s to represent an intercept? If so, this column is added to the left of `data`.
#'
#' @return A vector of linear predictors with length equal to the number of rows in `data`.
#' @export
#' @keywords internal
#'
#' @examples
#' data = data.frame(X1 = c(1,0), X2 = c(0,1))
#'
#' # No intercept
#' beta1 = c(1,2)
#' lin_pred_contrib(data, beta1, add_intercept = FALSE)
#'
#' # With intercept
#' beta2 = c(1,2,3)
#' lin_pred_contrib(data, beta2, add_intercept = TRUE)
lin_pred_contrib <- function(data, beta, add_intercept = TRUE){
  # Validate dimensions of input
  p_data = ncol(data)
  p_beta = length(beta)
  if((p_data == p_beta) && (!add_intercept)){
    # Do nothing
  } else if((p_data == p_beta - 1) && (add_intercept)){
    # Do nothing
  } else{
    stop("In lin_pred_contrib(): Incompatible dimensions of data and beta.")
  }

  # Setup data for multiplication by beta
  data_mat = as.matrix(data)
  if(add_intercept) data_mat = cbind(1, data_mat)

  # Compute linear predictor
  eta_vec = data_mat %*% beta
  eta = as.numeric(eta_vec)
  return(eta)
}



make_REs <- function(Gamma){
  MASS::mvrnorm(1, rep(0, times = nrow(Gamma)), Gamma)
}


lin_pred_RE_contrib <- function(data_ran, Gamma, add_intercept = TRUE, return_REs = FALSE){
  REs = make_REs(Gamma)
  contrib = lin_pred_contrib(data_ran, REs, add_intercept)

  if(return_REs){
    output = list(contrib = contrib, REs = REs)
  } else{
    output = contrib
  }
  return(output)
}


#' Compute the linear predictor based on provided datasets and parameters
#'
#' Note : Please specify either add_intercept or both add_intercept_fix and add_intercept_ran. Do not provide all three!
#'
#' @param data_fix A dataset containing covariates with fixed effects. Intercept can optionally be added later.
#' @param data_ran A dataset containing coveriates with random effects. Intercept can optionally be added later.
#' @param beta Vector of fixed effects coefficients.
#' @param Gamma Covariance matrix of the random effects coefficients.
#' @param add_intercept Should an intercept column be added to the datasets for both fixed and random effects?
#' @param add_intercept_fix,add_intercept_ran Should an intercept column be added to the datasets for fixed and random effects respectively?
#' @param return_REs Should generated random effects be returned?
#'
#' @return A vector containing a linear predictor for each observation in the provided datasets.
#' @export
#'
#' @examples
#' data_fix = data.frame(X1 = c(1,0), X2 = c(0,1))
#' data_ran = data_fix
#'
#' # Fixed and random effects for intercept
#' beta1 = c(1,2,3)
#' Gamma1 = diag(3)
#'
#' get_lin_preds(data_fix, data_ran, beta1, Gamma1,
#'   add_intercept = TRUE)
#'
#'
#' # Fixed effect only for intercept
#' beta2 = c(1,2,3)
#' Gamma2 = diag(2)
#'
#' get_lin_preds(data_fix, data_ran, beta2, Gamma2,
#'   add_intercept_fix = TRUE, add_intercept_ran = FALSE)
#'
#'
#' # Return generated random effects in the second example
#' get_lin_preds(data_fix, data_ran, beta2, Gamma2,
#'  add_intercept_fix = TRUE, add_intercept_ran = FALSE, return_REs = TRUE)
get_lin_preds <- function(data_fix, data_ran, beta, Gamma, add_intercept = NULL, add_intercept_fix = NULL, add_intercept_ran = NULL, return_REs = FALSE){
  # Check that fixed and random effects datasets have the same number of observations
  if(nrow(data_fix) != nrow(data_ran)) stop("In get_lin_preds(): Different number of observations in fixed and random effects datasets.")

  # Where should intercepts be added?
  if(is.logical(add_intercept) && is.null(add_intercept_fix) && is.null(add_intercept_ran)){
    add_intercept_fix = add_intercept
    add_intercept_ran = add_intercept
  } else if(is.null(add_intercept) && is.logical(add_intercept_fix) && is.logical(add_intercept_ran)){
    # Do nothing
  } else{
    stop("In get_lin_preds(): Please specify either add_intercept or both add_intercept_fix and add_intercept_ran, but not all three.")
  }


  contrib_fix = lin_pred_contrib(data_fix, beta, add_intercept_fix)

  info_ran = lin_pred_RE_contrib(data_ran, Gamma, add_intercept_ran, return_REs)
  if(return_REs){
    contrib_ran = info_ran$contrib
    REs = info_ran$REs
  } else{
    contrib_ran = info_ran
  }

  lin_preds = contrib_fix + contrib_ran

  if(return_REs){
    output = list(lin_preds = lin_preds, REs = REs)
  } else{
    return(lin_preds)
  }
}





# Check for membership in a data frame ----
# These are only used for testing the non-parametric bootstrap sampler


is_in_DF <- function(x, data){
  for(i in 1:nrow(data)){
    y = dplyr::slice(data, i)

    if(all(x == y)) return(TRUE)
  }

  return(FALSE)
}


is_DF1_subset_DF2 <- function(DF1, DF2){
  all_checks = rep(FALSE, times = nrow(DF1))

  for(i in 1:nrow(DF1)){
    all_checks[i] = is_in_DF(DF1[i,], DF2)
  }

  is_subset = all(all_checks)
  return(is_subset)
}




# Define %do% and %dopar% for use with the foreach package ----
`%do%` <- foreach::`%do%`
`%dopar%` <- foreach::`%dopar%`
