

#' Extract the `i`th element of each vector in `all_vecs`
#'
#'Extracts the `i`th component of each element in `all_vecs`. This function is nearly identical to `get_index_list()`, but requires that no element of `all_vecs` be a list.
#'
#' @param all_vecs A list of vectors. Throws an error if any contain fewer than `i` entries.
#' @param i Index to extract.
#'
#' @return A vector of the same length as `all_vecs`, containing the `i`th component of each element in `all_vecs`. If any components of `all_vecs` have fewer than `i` elements, `get_index()` will throw an error.
#' @export
#' @keywords internal
#'
#' @examples
#' A = c(1,2,3)
#' B = c("a", "b")
#' C = 1:1000
#' all_vecs = list(A, B, C)
#' get_index_vec(all_vecs, 2)
get_index_vec <- function(all_vecs, i){
  # Check whether any elements of all_vecs are shorter than i
  all_lengths = sapply(all_vecs, length)
  min_length = min(all_lengths)
  if(min_length < i) stop("In get_index_vec(): Index to extract is larger than length of the shortest vector.")

  # Check whether any element of all_vecs is a list
  all_types = sapply(all_vecs, typeof)
  if(any(all_types == "list")) warning("In get_index_vec(): List detected. Consider using get_index_list() instead.")

  # Perform extraction
  sapply(all_vecs, `[`, i=i)
}

#' Extract the `i`th element of each list in `all_lists`
#'
#' Extracts the `i`th component of each element in `all_lists`. This function is nearly identical to `get_index_vec()`, but instead correctly deals with elements themselves being lists.
#'
#' @param all_lists A list of lists. Throws an error if any contain fewer than `i` entries.
#' @param i Index to extract.
#'
#' @return A list of the same length as `all_lists`, containing the `i`th component of each element in `all_lists`. If any components of `all_lists` have fewer than `i` elements, `get_index_list()` will throw an error.
#' @export
#' @keywords internal
#'
#' @examples
#' A = list(1,2,3)
#' B = list("a", "b")
#' all_lists = list(A, B)
#' get_index_list(all_lists, 2)
get_index_list <- function(all_lists, i){
  # Check whether any elements of all_vecs are shorter than i
  all_lengths = sapply(all_lists, length)
  min_length = min(all_lengths)
  if(min_length < i) stop("In get_index_list(): Index to extract is larger than length of the shortest list.")

  # Check whether each element of all_lists is a list
  all_types = sapply(all_lists, typeof)
  if(any(all_types != "list")) warning("In get_index_list(): Non-list detected. Consider using get_index() instead.")

  # Perform extraction
  lapply(all_lists, `[[`, i=i)
}


#' Remove the elements of X corresponding to the indices in inds
#'
#' This function is a wrapper for negative indexing, with syntax inspired by the `Julia` function `deleteat!()`. If any of the indices in `inds` are larger than the length of `X`, throw an error.
#'
#' @param X A vector
#' @param inds A vector of indices
#'
#' @return A copy of `X` with the elements given in `inds` removed.
#' @export
#' @keywords internal
#'
#' @examples
#' A = c(1, 3, 5, 7)
#' delete_at(A, c(2,3))
delete_at <- function(X, inds){
  # Check for index out of bounds
  if(max(inds) > length(X)) stop("In delete_at(): Largest index is larger than length of X.")

  X_new = X
  for(i in sort(inds, decreasing = T)){
    X_new = X_new[-i]
  }

  return(X_new)
}



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
  group_sizes = sapply(X_list, nrow)

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
