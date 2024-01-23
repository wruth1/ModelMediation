

#' Extract the ith element of each vector in all_vecs
#'
#' @param all_vecs A list of vectors. Throws an error if any contain fewer than i entries.
#' @param i Index to extract.
#'
#' @return A vector of the same length as all_vecs, containing the ith component of each element in all_vecs. If any components of all_vecs have fewer than i elements, get_index() will throw an error.
#' @export
#'
#' @examples
#' A = c(1,2,3)
#' B = c("a", "b")
#' C = 1:1000
#' all_vecs = list(A, B, C)
#' get_index(all_vecs, 2)
get_index <- function(all_vecs, i){
  # Check whether any elements of all_vecs are shorter than i
  all_lengths = sapply(all_vecs, length)
  min_length = min(all_lengths)
  if(min_length < i) stop("In get_index(): Index to extract is larger than length of the shortest vector.")

  # Perform extraction
  sapply(all_vecs, `[`, i=i)
}
