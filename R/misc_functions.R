# misc funcs


#' Set missings in dataframe to NA
#'
#' `replace_values_with_na()` sets NEPS specific missing codes to NA. Default missing codes are c(seq(-99, -90), seq(-56, -51), seq(-29, -22)). If only specific variables should be taken into account, use the vars argument and supply a vector of variable names.
#'
#' @param data A dataset to apply the function to.
#' @param vars Specify vars where missing values should be replaced with NA. If set to NULL, all variables will be used.
#' @param values_to_replace Specify values that should be replaced with NA. Default are all standard NEPS missing codes.
#'
#' @returns A dataframe.
#'
#' @examples
#' # Example with NEPS SC6 semantic structures spGap file
#' path <- system.file("extdata", "SC6_spGap_S_15-0-0.dta", package = "nepstools")
#' df_neps <- read_neps(path, col_select = c("ID_t", "ts2912m"))
#'
#' # create some artificial datapoints
#' artificial_datapoints <- data.frame(
#'  ID_t = c(1, 2, 3, 4, 5),
#'  ts2912m = c(-97, 12, NA, 4, -98))
#'
#' # add these artificial datapoints to the empty neps dataset
#' df_neps <- rbind(df_neps, artificial_datapoints)
#' print(df_neps)
#'
#' df_neps_replaced <- replace_values_with_na(df_neps)
#' print(df_neps_replaced)
#'
#' # Example with a vector
#' v <- c(1, -97, 3, -29, 5)
#' replace_values_with_na(v)
#'
#'
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


#' Replace season codes with months
#'
#' `replace_season_codes()` replaces NEPS specific season codes in date variables to standard month codes. For example code 27: "Summer" will be replaced with Code 7: "July".
#'
#' @param data A dataset to apply the function to.
#' @param vars Specify vars where season codes should be replaced with months. Optional, if set to NULL, all vars are being taken into account, which can lead to problems in case of non-date variables.
#' @param values_to_replace These are the season codes. Usually doesnt need to be modified.
#'
#' @returns A Dataframe.
#'
#' @examples
#' # Example with NEPS SC6 semantic structures spGap file
#' path <- system.file("extdata", "SC6_spGap_S_15-0-0.dta", package = "nepstools")
#' df_neps <- read_neps(path, col_select = c("ID_t", "ts2912m"))
#'
#' # create some artificial datapoints
#' artificial_datapoints <- data.frame(
#'  ID_t = c(1, 2, 3, 24, 5),
#'  ts2912m = c(-97, 24, 5, 32, 30))
#'
#' # add these artificial datapoints to the empty neps dataset
#' df_neps <- rbind(df_neps, artificial_datapoints)
#' print(df_neps)
#'
#' df_neps_replaced <- replace_season_codes(df_neps)
#' print(df_neps_replaced)
#'
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

#' Expand data
#'
#' `expand()` duplicates rows by a variable specified in the duration argument. It is inspired by statas expand function.
#' @param data A dataset that will be expanded.
#' @param duration Specify the variable that will be used for expanding the data, usually this is a duration (of episode) variable. It needs to be a vector with only valid non-negative numbers and no NAs.
#'
#' @returns A Dataframe.
#'
#' @examples
#' df <- data.frame(id = 1:3, duration = c(2, 1, 3))
#' expand(df, duration)
#'
#' @export
expand <- function(data, duration){
  # Check if data is a data.frame
  if(!is.data.frame(data)) stop("The data argument must be a data.frame.")

  # Capture duration as name
  duration <- substitute(duration)

  # Check if duration is a symbol (i.e., variable name)
  if (!is.symbol(duration)) stop("The duration argument must be an unquoted variable name.")

  duration_name <- as.character(duration)

  # Check if duration variable exists in data
  if (!(duration_name %in% names(data))) stop(paste0("The variable '" ,duration_name,  "', specified in the duration argument, is not in data."))

  # Check if data[[duration]] is numeric and non-negative, no NA
  if (!is.numeric(data[[duration_name]])) stop("The duration variable must be numeric.")
  if(any(data[[duration_name]] < 0) | any(is.na(data[[duration_name]]))) stop("Please ensure the duration argument is a valid non-negative number and not NA.")

  # Expand data by the duration variable
  data[rep(seq_len(nrow(data)), data[[duration_name]]), 1:ncol(data)]
}


#' Print questiontext
#'
#' `question()` is a convenient wrapper for the attr() function. It prints the attached questiontext from specified variable in the variable argument to the console.
#'
#' @param data Specify the dataset that was generated with read_neps function
#' @param variable Specify the variable for which the question text should be printed.
#'
#' @returns A String.
#' @export
#'
#' @examples
#' # Example with NEPS SC6 semantic structures spGap file
#' path <- system.file("extdata", "SC6_spGap_S_15-0-0.dta", package = "nepstools")
#' df_neps <- read_neps(path, col_select = c("ID_t", "ts2912m"))
#'
#' question(df_neps, "ts2912m")
#'
question <- function(data, variable) {
  # Check if data is a data.frame
  if (!is.data.frame(data)) {
    stop("The data argument must be a data frame.")
  }

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

