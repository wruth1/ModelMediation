

# For setting model parameters

#' Store all regression parameters in a single list
#'
#' @param beta_Y Vector of length 4 containing fixed-effects for the outcome model. Order of variables is: Intercept, M, X, C1, C2.
#' @param Gamma_Y Covariance matrix of size 3x3 containing random effects for the outcome model. Order of variables is: Intercept, M, X.
#' @param beta_M Vector of length 3 containing fixed-effects for the outcome model. Order is: Intercept, X, C1, C2.
#' @param Gamma_M Covariance matrix of size 2x2 containing random effects for the outcome model. Order of variables is: Intercept, X.
#'
#' @return A named list containing fixed and random effects parameters for both the outcome and mediator regression models. Names in list match names of arguments to this function.
#' @export
#'
#' @examples
#' make_all_reg_pars()
make_all_reg_pars <- function(beta_Y = c(1, -1.5, 1, -1.5, 1), Gamma_Y = diag(3), beta_M = c(-1, 1, -1, 1), Gamma_M = diag(2)){
  names(beta_Y) = c("Int", "M", "X", "C1", "C2")
  names(beta_M) = c("Int", "X", "C1", "C2")
  list(beta_Y = beta_Y, Gamma_Y = Gamma_Y, beta_M = beta_M, Gamma_M = Gamma_M)
}





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
