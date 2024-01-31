

# list_2_data and its helpers ----


#' Construct a single data frame by `rbind`-ing elements of a list. Add/Create group labels.
#'
#' @param X_list A list of data frame-like objects (e.g. data frames, tibbles). Must all have the same number of columns.
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
