# misc funcs


#' Set missings in dataframe to NA
#'
#' `replace_values_with_na()` sets NEPS specific missing codes to NA. Default missing codes are c(seq(-99, -90), seq(-56, -51), seq(-29, -22)). If only specific variables should be taken into account, use the vars argument and supply a vector of variable names.
#'
#' @param data A dataframe to apply the function to.
#' @param vars Specify variables where missing values should be replaced with NA. If set to NULL, all variables will be used.
#' @param values_to_replace Specify values that should be replaced with NA. Default are all standard NEPS missing codes.
#'
#' @returns A dataframe.
#'
#' @examples
#' # Example with NEPS SC6 semantic structures spGap file
#' path <- system.file("extdata", "SC6_spGap_S_15-0-0.dta", package = "nepstools")
#' df_neps <- read_neps(path, english = TRUE, col_select = c("ID_t", "ts2912m"))
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
#' @param vars Specify vars where season codes should be replaced with months. Optional, if set to NULL, all vars are being taken into account. The function than only select variables that have the label "month" or "monat". It is recommended to specify the variables to which the function should be applied.
#' @param values_to_replace These are the season codes. Usually doesnt need to be modified.
#'
#' @returns A Dataframe.
#'
#' @examples
#' # Example with NEPS SC6 semantic structures spGap file
#' path <- system.file("extdata", "SC6_spGap_S_15-0-0.dta", package = "nepstools")
#' df_neps <- read_neps(path, english = TRUE, col_select = c("ID_t", "ts2912m"))
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
      vars <- names(data)[sapply(data, has_month_label)] # find variables with label "month" or "monat"
    } else {
      # Validate provided variables exist in the dataframe
      if (!all(vars %in% names(data))) {
        stop("Some variables specified in 'vars' are not present in the data frame.")
      }

      # Check if specified variables have labels containing "month" or "monat"
      vars_without_month_label <- vars[!sapply(data[vars], has_month_label)]
      if (length(vars_without_month_label) > 0) {
        warning(
          sprintf(
            "The following specified variables do not have labels containing 'month' or 'monat': %s",
            paste(vars_without_month_label, collapse = ", ")
          )
        )
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
#' `expand()` duplicates rows by a integer variable specified in the duration argument, typically a months counter. It is inspired by statas expand function and often used in the context of episode data that must be transformed into monthly data.
#' @param data A dataframe.
#' @param duration Specify the integer variable to be used for expanding the data. You can provide this variable either as a quoted string or unquoted variable name. Typically, this is a duration (e.g., episode length) variable. It must be a numeric vector containing only non-negative integers, with no missing (NA) values. Note: If there are zeros in the duration variable, the function will delete these rows. So make sure, that any generated duration variables are > 1 without NAs.
#'
#' @returns A Dataframe.
#'
#' @examples
#'
#' path <- system.file("extdata", "SC6_spGap_S_15-0-0.dta", package = "nepstools")
#' df_neps_ex <- read_neps(path, col_select = c("ID_t", "ts2911m", "ts2911y", "ts2912m", "ts2912y"))
#' df_neps_ex <- df_neps_ex[-1,] # get rid of NA row that is in every semantic structure file from NEPS
#'
#' # create some artificial datapoints
#' artificial_datapoints <- data.frame(
#'   ID_t = c(1, 2, 3, 4, 5),
#'  ts2911m = c(2, 12, 11, 4, 12),
#'   ts2911y = c(2008, 2006, 2005, 2009, 2010),
#'   ts2912m = c(4, 1, 2, 5, 2),
#'   ts2912y = c(2008, 2007, 2006, 2009, 2011)
#' )
#'
#' # add these artificial datapoints to the empty neps dataset
#' df_neps_ex <- rbind(df_neps_ex, artificial_datapoints)
#'
#' # generate date and duration variables
#' df_neps_ex <- df_neps_ex |>
#'   dplyr::mutate(start = ((ts2911y-1960)*12)+ts2911m - 1, # months since january 1960
#'   end = ((ts2912y-1960)*12)+ts2912m - 1, # months since january 1960
#'   duration = (end - start) + 1) # + 1 required so we dont get episode durations of 0 months
#'
#' print(df_neps_ex)
#'
#' # expand the dataframe
#' expanded_df <- nepstools::expand(df_neps_ex, duration)
#' print(expanded_df)
#'
#' @export
expand <- function(data, duration){
  # Check if data is a data.frame
  if(!is.data.frame(data)) stop("The data argument must be a data.frame.")

  # Capture duration argument expression
  duration_expr <- substitute(duration)

  # Determine duration column name
  if (is.symbol(duration_expr)) {
    # unquoted name, e.g. dur
    duration_name <- as.character(duration_expr)
  } else if (is.character(duration_expr)) {
    # quoted name, e.g. "dur"
    duration_name <- duration_expr
  } else {
    stop("The duration argument must be an unquoted or quoted variable name (string).")
  }

  # Check if duration variable exists in data
  if (!(duration_name %in% names(data))) stop(
    paste0("The variable '", duration_name, "' specified in the duration argument is not in data."))

  # Check if data[[duration_name]] is numeric and non-negative, no NA
  if (!is.numeric(data[[duration_name]])) stop("The duration variable must be numeric.")
  if(any(data[[duration_name]] < 0) | any(is.na(data[[duration_name]]))) stop(
    "Please ensure the duration argument is a valid non-negative number and not NA.")

  # Warn if zeros are present in the duration variable
  if(any(data[[duration_name]] == 0)) {
    warning("Zero values found in the duration variable. Corresponding rows will be dropped during expansion.")
  }

  # check if duration variable is an integer (after NA check!)
  if (any(data[[duration_name]] %% 1 != 0)) {
    stop("The duration variable must contain integer values.")
  }

  # Expand data by the duration variable
  data[rep(seq_len(nrow(data)), data[[duration_name]]), , drop = FALSE]
}



#' Print questiontext
#'
#' `question()` is a convenient wrapper for the attr() function. It prints the attached questiontext from specified variable in the variable argument to the console.
#'
#' @param data Specify the dataset that was generated with read_neps function
#' @param variable Specify the variable for which the question text should be printed. Argument can be a string or an unquoted variable name.
#'
#' @returns A String.
#' @export
#'
#' @examples
#' # Example with NEPS SC6 semantic structures spGap file
#' path <- system.file("extdata", "SC6_spGap_S_15-0-0.dta", package = "nepstools")
#' df_neps <- read_neps(path, english = TRUE, col_select = c("ID_t", "ts2912m"))
#'
#' question(df_neps, "ts2912m")
#'
question <- function(data, variable) {
  # Check if data is a data.frame
  if (!is.data.frame(data)) {
    stop("The data argument must be a data frame.")
  }

  # Capture the variable argument expression
  var_expr <- substitute(variable)

  # Determine the variable name as a character string
  if (is.symbol(var_expr)) {
    # unquoted variable name
    variable_name <- as.character(var_expr)
  } else if (is.character(var_expr)) {
    # quoted variable name
    variable_name <- var_expr
  } else {
    stop("The variable argument must be an unquoted or quoted variable name (string).")
  }

  # Check if variable exists in data
  if (!variable_name %in% names(data)) {
    stop("Variable '", variable_name, "' not found in the data frame.")
  }

  # Extract attribute "question" (non-exact match)
  q_attr <- attr(data[[variable_name]], "NEPS_questiontext_", exact = FALSE)

  # Check if attribute exists and is non-empty
  if (is.null(q_attr) || length(q_attr) == 0) {
    warning("Variable '", variable_name, "' does not have a questiontext attached")
    return(invisible(NULL))
  }

  return(q_attr)
}


#' Search for keywords in attributes of variables
#'
#' This function searches through the attributes of each column (variable) in a data frame
#' for specified keyword(s) and returns a named list of all attribute values
#' that contain any of the search words.
#' Data frame-level attributes are **not searched**.
#' The search is case-insensitive by default.
#'
#' @param df A data frame whose columns' attributes will be searched.
#' @param search_words A character vector of one or more keywords to search for within attributes.
#' @param ignore.case Logical; if \code{TRUE} (default), the search is case-insensitive.
#'
#' @return
#' A named list of matched attribute values.
#' The names describe where each match was found as:
#' \code{"Variable <column_name>: @<attribute_name>"}
#' If no matches are found, the function prints a message and returns \code{NULL}.
#'
#' @export
#'
#' @examples
#' # Example with NEPS SC6 semantic structures spGap file
#' path <- system.file("extdata", "SC6_spGap_S_15-0-0.dta", package = "nepstools")
#' df_neps <- read_neps(path, english = TRUE)
#'
#' # Search for keyword "type" to identify all variables dealing with the type of the gap in some way
#' lookfor_meta(df_neps, "type")
lookfor_meta <- function(df, search_words, ignore.case = TRUE) {
  # Check input types
  if (!is.data.frame(df)) {
    stop("Argument 'df' must be a data.frame.")
  }
  if (!(is.character(search_words) && length(search_words) >= 1)) {
    stop("Argument 'search_words' must be a character vector of length >= 1.")
  }
  if (!(is.logical(ignore.case) && length(ignore.case) == 1)) {
    stop("Argument 'ignore.case' must be a single logical value (TRUE or FALSE).")
  }

  # Helper to check if any search word matches attribute's printed value
  attr_matches <- function(attr_value, words) {
    if (is.null(attr_value)) return(FALSE)
    # Convert the attribute value to a single character string (capture.output(print()) handles complex attribute types by capturing their printed output)
    attr_char <- paste(utils::capture.output(print(attr_value)), collapse = " ")
    # For each search word, check if it appears (grepl) in the attribute character string
    any(sapply(words, function(w) grepl(w, attr_char, ignore.case = ignore.case)))
  }

  results <- list()

  # Search attributes of columns (variables)
  for (col_name in names(df)) {
    # Get all attributes of the current column vector
    col_attrs <- attributes(df[[col_name]])
    if (length(col_attrs) == 0) next

    # Loop over each attribute of the current column
    for (attr_name in names(col_attrs)) {
      # Check if this column attribute contains any of the search words by using the above helper
      if (attr_matches(col_attrs[[attr_name]], search_words)) {
        # If match found, add the match to the results list. Label it with column name and attribute name
        results[[paste0("Variable ", col_name, ": @", attr_name)]] <- col_attrs[[attr_name]]
      }
    }
  }

  # If no matching attributes found, print message and return NULL
  if (length(results) == 0) {
    message("No attributes matching search words found.")
    return(NULL)
  }

  # return list
  return(results)
}
