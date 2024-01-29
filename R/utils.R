

# list_2_tibble and its helpers ----


#' Construct a tibble by `rbind`-ing elements of a list. Add/Create group labels.
#'
#' @param X_list A list of objects able to be passed recursively to `rbind`. In particular, must all have the same number of columns.
#' @param group_labels Optionally, a vector of labels for each element of `X_list`. Must be either the same length as `X_list` or equal to the total number of rows among elements in `X_list`. If `NULL`, labels are `G1`, `G2`,...
#'
#' @return A tibble containing all elements of `X_list` with an extra column for labels.
#' @export
#'
#' @examples
#' a_matrix = matrix(c(1,0,0,1), nrow = 2)
#' another_matrix = matrix(c(0,1,1,0), nrow = 2)
#' some_matrices = list(a_matrix, another_matrix)
#'
#' list_2_tibble(some_matrices)
#'
#' matrices_names = c("Alice", "Bob")
#' list_2_tibble(some_matrices, matrices_names)
list_2_tibble <- function(X_list, group_labels = NULL){
  # Stack elements of X_list ----
  X_tibble = list_2_tibble_unnamed(X_list)

  # Create group labels ----
  all_labels = list_2_tibble_make_labels(X_list, group_labels)

  output = tibble::add_column(X_tibble, all_labels)
  colnames(output) = c(colnames(X_tibble), "group")

  return(output)
}




#' Stack elements of `X_list` and convert to a tibble
#'
#' @param X_list A list of objects able to be passed recursively to `rbind`. In particular, must all have the same number of columns.
#'
#' @return A single tibble containing the elements of `X_list`.
#' @export
#' @keywords internal
#'
#' @examples
#' a_matrix = matrix(c(1,0,0,1), nrow = 2)
#' another_matrix = matrix(c(0,1,1,0), nrow = 2)
#' some_matrices = list(a_matrix, another_matrix)
#'
#' list_2_tibble_unnamed(some_matrices)
list_2_tibble_unnamed <- function(X_list){
  # Stack elements of X_list ----
  # Uses some tricks from the rlang package (see Wickham's "Advanced R" book, available online)
  output_raw = rlang::inject(rbind(!!!X_list))

  # Convert stack to a tibble, adding variable names if necessary
  output_vals = tibble::as_tibble(output_raw, .name_repair = "universal_quiet")

  return(output_vals)
}




#' Create group labels
#'
#' @param X_list A list of objects able to be passed recursively to `rbind`. In particular, must all have the same number of columns.
#' @param group_labels Optionally, a vector of labels for each element of `X_list`. Must be either the same length as `X_list` or equal to the total number of rows among elements in `X_list`. If `NULL`, labels are `G1`, `G2`,...
#'
#' @return A vector of group labels with length equal to the total number of rows in elements of `X_list`.
#' @export
#' @keywords internal
#'
#' @examples
#' a_matrix = matrix(c(1,0,0,1), nrow = 2)
#' another_matrix = matrix(c(0,1,1,0), nrow = 2)
#' some_matrices = list(a_matrix, another_matrix)
#'
#' list_2_tibble_make_labels(some_matrices)
#'
#' matrices_names = c("Alice", "Bob")
#' list_2_tibble_make_labels(some_matrices, matrices_names)
list_2_tibble_make_labels <- function(X_list, group_labels = NULL){
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
    stop("In list_2_tibble: Non-conformable sizes between X_list and group_labels.")
  }

  return(all_labels)
}
