

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
