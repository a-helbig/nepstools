# misc funcs


#' Set missings in dataframe to NA
#'
#' @param data A dataset to apply the function to.
#' @param vars Specify vars where missing values should be replaced with NA. If set to NULL, all variables will be used.
#' @param values_to_replace Specify values that should be replaced with NA. Default are all standard NEPS missing codes.
#'
#' @returns A dataframe.
#' @export
#'
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
#' @param vars Specify vars where season codes should be replaced with months. Optional, if set to NULL, all vars are being taken into account, which can lead to problems in case of non-date variables.
#' @param values_to_replace These are the season codes. Usually doesnt need to be modified.
#'
#' @returns A Dataframe.
#' @export
#'
replace_season_codes <- function(data, vars = NULL, values_to_replace = c(21, 24, 27, 30, 32)) {
  # Helper function to check if a variable has a label attribute containing "month" or "monat"
  has_month_label <- function(x) {
    lbl <- attr(x, "label")
    if (is.null(lbl)) return(FALSE)
    lbl_lower <- tolower(as.character(lbl))
    return(grepl("month|monat", lbl_lower))
  }

  if (is.data.frame(data)) {
    # Select variables based on label if vars not provided
    if (is.null(vars)) {
      vars <- names(data)[sapply(data, has_month_label)]
    } else {
      # Validate provided variables exist in the dataframe
      if (!all(vars %in% names(data))) {
        stop("Some variables specified in 'vars' are not present in the data frame.")
      }
    }

    # actual replace of month vars values by subtracting 20 from each value
    for (var in vars) {
      for (value in values_to_replace) {
        data[[var]][data[[var]] == value] <- value - 20
      }
    }
  } else {
    stop("Input data must be a data frame.")
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
expand <- function(data, duration){
  duration <- substitute(duration) # makes sure the argument can be supplied without quot. marks.
  if(any(data[[duration]] < 0) | any(is.na(data[[duration]]))) stop("Please ensure the duration argument is a valid non-negative number and not NA.") # duration must be > 0 and not NA
  data[rep(seq_len(nrow(data)), data[[duration]]), 1:ncol(data)] # expanding feature
}


#' Prints questiontext of variable to console
#'
#' @param data Specify the dataset that was generated with read_neps function
#' @param variable Specify the variable for which the question text should be printed.
#'
#' @returns A String.
#' @export
#'
#' @examples question(data, "variable")
question <- function(data, variable) {
  # Check if variable exists in data
  if (!variable %in% names(data)) {
    stop("Variable '", variable, "' not found in the data frame.")
  }

  # Extract attribute "question" (non-exact match)
  q_attr <- attr(data[[variable]], "NEPS_questiontext_", exact = FALSE)

  # Check if attribute exists and is non-empty
  if (is.null(q_attr) || length(q_attr) == 0) {
    warning("Variable '", variable, "' does not have a questiontext attached")
    return(invisible(NULL))
  }

  return(q_attr)
}

