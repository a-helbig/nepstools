# misc funcs


#' Set missings in dataframe to NA
#'
#' @param data A dataset to apply the function to.
#' @param vars Specify vars where missing values should be replaced with NA.
#' @param values_to_replace Specify values that should be replaced with NA. Default are all standard NEPS missing codes.
#'
#' @returns A dataframe.
#' @export
#'
#' @examples replace_values_with_na(data, c("var1","var2"))
replace_values_with_na <- function(data, vars = NULL, values_to_replace = c(seq(-99, -90), seq(-56, -51), seq(-29, -22))) {
  # Case 1: If input is a dataframe
  if (is.data.frame(data)) {
    if (is.null(vars)) {
      # Use function on all variables in the dataframe
      for (var in names(data)) {
        for (value in values_to_replace) {
          data[[var]][data[[var]] == value] <- NA
        }
      }
    } else {
      # Use function on selected variables in the dataframe
      for (var in vars) {
        for (value in values_to_replace) {
          data[[var]][data[[var]] == value] <- NA
        }
      }
    }

    # Case 2: If input is a vector
  } else if (is.vector(data)) {
    for (value in values_to_replace) {
      data[data == value] <- NA
    }

    # Case 3: If input is neither a dataframe nor a vector
  } else {
    stop("Input data must be either a dataframe or a vector.")
  }

  return(data)
}



#' Replace season codes with corresponding months
#'
#' @param data A dataset to apply the function to.
#' @param vars Specify vars where season codes should be replaced with months. Optional, if NULL, all vars are being taken into account, which can lead to problems in case of non-date variables.
#' @param values_to_replace These are the season codes. Usually doesnt need to be modified.
#'
#' @returns A Dataframe.
#' @export
#'
#' @examples replace_season_codes(data, c("month_var_1", "month_var_2"))
replace_season_codes <- function(data, vars = NULL, values_to_replace=c(21,24,27,30,32)) {
  if (is.null(vars)) {
    for(var in names(data)) {
      for(value in values_to_replace) {
        data[[var]][data[[var]] == value] <- value - 20
      }
    }
  } else {
    for(var in vars) {
      for(value in values_to_replace) {
        data[[var]][data[[var]] == value] <- value -20
      }
    }
  }
  return(data)
}

#' Expand episode data to monthly structure
#'
#' @param data A dataset that will be expanded.
#' @param duration Specify the variable that will be used for expanding the data, usually this is a duration (of episode) variable. It needs to be a vector with only valid non-negative numbers and no NAs.
#'
#' @returns A Dataframe.
#' @export
#'
#' @examples expand(data, duration)
expand <- function(data, duration){
  duration <- substitute(duration) # makes sure the argument can be supplied without quot. marks.
  if(any(data[[duration]] < 0) | any(is.na(data[[duration]]))) stop("Please ensure the duration argument is a valid non-negative number and not NA.") # duration must be > 0 and not NA
  data[rep(seq_len(nrow(data)), data[[duration]]), 1:ncol(data)] # expanding feature
}
