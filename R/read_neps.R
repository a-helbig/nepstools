

#' Function, that reads neps data and attracts available meta infos to each variable. Select variables with col_select, specify language: German (default) or English and lastly specify if only most important meta info (default) should be attracted or all of it.
#'
#' @param datasetpath A datapath to a NEPS data file
#' @param col_select Specify variables that will be included in the dataset. If set to NULL, data is being loaded with all available variables.
#' @param english If set to TRUE, the dataset will be loaded with English variable and value labels, and metadata. If set to FALSE, German labels and metadata will be used instead.
#' @param compact_meta If set to TRUE, only a selection of important metadata will be added to the data, including question texts, interviewer texts, harmonization rules, and instrument variable names. If set to FALSE, all available metadata will be included.
#' @param charren If set to TRUE, instrument variable names will replace the standard variable names. If set to FALSE, the standard variable names will be retained.
#'
#' @returns A Dataframe.
#' @export
#'
read_neps <- function(datasetpath, col_select = NULL, english = FALSE, compact_meta = TRUE, charren = FALSE) {
  # prevents devtools::check() from complaining about Non-standard evaluation use in dplyr functions
  variable <- type <- value <- NULL

  if (!file.exists(datasetpath)) {
    stop("Dataset file does not exist at the specified path.")
  }
  # read data with haven::read_dta
  if(is.null(col_select))
    data <- haven::read_dta(datasetpath)
  else
    data <- haven::read_dta(datasetpath, col_select = dplyr::all_of(col_select))

  # read expansionsfields with meta infos in neps data
  meta_data <- read_exp_fields(datasetpath)

  # English labels ----------------------------------------------------------

  if(english){

    # Start with value labels -------------------------------------------------

    # load names of value labels
    names_of_val_labels <- meta_data |> dplyr::filter(type == "_lang_l_en")

    # extract attracted val labels from 'label.table' as a named integer list along with names of val labels. suppress warnings here because some labels are missing and factor codes are being detected
    suppressWarnings(label_table <- attr(readstata13::read.dta13(datasetpath, select.rows = 1), 'label.table'))

    # Loop through each variable in exp_fields
    for (i in 1:nrow(names_of_val_labels)) {
      variable_name <- names_of_val_labels$variable[i]
      value_name <- names_of_val_labels$value[i]
      #
      # Check if the value exists in label_table
      if (value_name %in% names(label_table)) {
        # Get the labels from label_table
        new_labels <- label_table[[value_name]]

        # Convert integer labels to double (haven::write_dta requires double)
        if (is.integer(new_labels)) {
          new_labels <- as.double(new_labels)
          names(new_labels) <- names(label_table[[value_name]])
        }

        # Check if the attribute exists in data and then assign new labels
        if (variable_name %in% names(data) && !is.null(attr(data[[variable_name]], 'labels'))) {
          attr(data[[variable_name]], 'labels') <- new_labels
        }
      }
    }

    # now, variable labels ----------------------------------------------------

    # pull english var labels from meta_data
    en_labels <- meta_data |> dplyr::filter(type == "NEPS_varlabel_en") |> dplyr::select(-type)

    # Iterate through each variable in the data
    for (var in names(data)) {
      # Find the corresponding label of each variable in data in en_labels df
      label <- en_labels |> dplyr::filter(variable == var) |> dplyr::pull(value)
      # If a label is found, apply it to the variable in data
      if (length(label) > 0) {
        attr(data[[var]], 'label') <- label
      }
    }

    # generate vector with all meta data types
    meta <- unique(meta_data$type)
    # get rid of the german meta types
    meta <- meta[!stringr::str_detect(meta, "_de$")]
  }

  # if german is selected
  else {
    # generate vector with all meta data types
    meta <- unique(meta_data$type)
    # get rid of the german meta types
    meta <- meta[!stringr::str_detect(meta, "_en$")]
  }

  # Add specified meta info to data ---------------------------------------------

  # filter meta data when compact_meta is selected
  if(compact_meta){
    meta <- meta[stringr::str_detect(meta, "(questiontext|interviewerinstruction|harmonization|alias)")]
  }
  meta_data <- meta_data |>
    dplyr::filter(type %in% meta)

  # attract the meta data to the data
  # Loop through each row in meta_data and retrieve variable, value and type name.
  for (i in 1:nrow(meta_data)) {
    variable_name <- meta_data$variable[i]
    value_name <- meta_data$value[i]
    type_name <- meta_data$type[i]

    # Check if the variable in the meta df exists in dataframe data
    if (variable_name %in% names(data)) {
      # attract meta data to variables
      attr(data[[variable_name]], which = type_name) <- value_name
    }
  }


  # original instrument names -----------------------------------------------
  # if charren is TRUE, switch variable names to original instrument names
  if(charren)
    data <- switch_var_names(data)

  return(data)
}

#' A helper to extract meta info from data
#'
#' @keywords internal
read_exp_fields <- function(datapath, cols=NULL, attr_type=NULL, only_value=F) {
  # prevents devtools::check() from complaining about Non-standard evaluation use in dplyr functions
  variable <- type <- value <- NULL

  # Read the data with only the "ID_t" column and the first row for performance. We use the slower read.dta13 function here because it reads all attracted meta data while havens read_dta func does not
  data <- readstata13::read.dta13(datapath, select.cols = "ID_t", select.rows = 1)
  # Extract the expansion fields attribute
  exp_fields <- attr(data, "expansion.fields")
  # check if there were expansion.fields, if not throw an informative error message
  if(is.null(exp_fields) | length(exp_fields)==0){
    stop("Cant find attracted meta data in selected dataset. Please ensure you select only NEPS-SUF datasets here.")
  }
  # Extract variable, type, and value from the expansion fields and unite these vectors into a df
  fields_df <- data.frame(
    variable = sapply(exp_fields, `[[`, 1),
    type = sapply(exp_fields, `[[`, 2),
    value = sapply(exp_fields, `[[`, 3),
    stringsAsFactors = FALSE
  )
  # access the required information from the "value" column for the selected variable and selected type of information
  if (!is.null(attr_type) & !is.null(cols)) {
    fields_df <- fields_df |>
      dplyr::filter(variable %in% cols & type == attr_type)
  }
  if (is.null(attr_type) & !is.null(cols)) {
    fields_df <- fields_df |>
      dplyr::filter(variable %in% cols)
  }
  if (!is.null(attr_type) & is.null(cols)) {
    fields_df <- fields_df |>
      dplyr::filter(type == attr_type)
  }
  if(only_value)
    fields_df <- fields_df |>
    dplyr::pull(value)

  return(fields_df)
}

#' A Helper to switch variable names in NEPS data to original instrument variable names. This feature is inspired by stata nepstools "charren" function. Background: During the data preparation process for the NEPS SUFs, the original variable names from the survey instruments are replaced with SUF-specific variable names. However, for some data users it may be useful to get the original instrument names. This function provides a quick way to do this, utilizing the metadata "NEPS_alias" stored within the datasets when being loaded with read_neps function.
#'
#' @keywords internal
switch_var_names <- function(data){
  # get current variable names
  vars <- names(data)

  # iterate over them
  for(i in seq_along(vars)) {
    var <- vars[i]

    # get NEPS_alias attribute of the variable
    var_name <- attr(data[[var]], "NEPS_alias")

    # only replace if available and non-empty string
    if (!is.null(var_name) && nzchar(var_name)) {
      # replace the name in the names vector
      vars[i] <- var_name
    }
  }

  # assign modified names back to data
  names(data) <- vars

  return(data)
}
