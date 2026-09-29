#' Rename variables by the words of their variable labels
#'
#' `rename_by_label()` gives variables readable names built from their
#' `"label"` attribute, e.g. after loading data with [read_neps()].
#'
#' Each variable gets the first word of its label (or the first `min_words`
#' words). If several variables end up with the same name, the next word of
#' their labels is added for all of them (e.g. "age mother" / "age father" ->
#' `age_mother` / `age_father`; "birth place mother" / "birth place father" ->
#' `birth_place_mother` / `birth_place_father`). This repeats until the names
#' are unique or the labels run out of words. Variables whose labels are
#' completely identical get a numeric suffix `_1`, `_2`, ...
#'
#' Labels are cleaned before use: German umlauts and sharp s are transliterated
#' (ae, oe, ue, ss), all other characters that are not letters or digits are
#' treated as word separators. Variables without a (non-empty) label keep their
#' current name as the basis.
#'
#' Only the variables selected via `vars` / `exclude` are renamed. All other
#' variables keep their names, and new names never clash with them.
#' The labels themselves are kept unchanged.
#'
#' @param df A data frame whose columns carry a `"label"` attribute.
#' @param vars Variables to rename, in tidyselect syntax as in
#'   [dplyr::select()]: names (`c(a, b)`), character vectors (`c("a", "b")`),
#'   ranges (`a:d`), helpers (`starts_with("ts")`), or negative selection
#'   (`-c(ID_t, wave)` = all except these). If `NULL` (default), all variables
#'   are renamed.
#' @param exclude Variables that keep their name while all others are renamed
#'   (tidyselect syntax). Only allowed if `vars` is `NULL`; equivalent to
#'   `vars = -c(...)`.
#' @param sep Separator between words and before numeric suffixes.
#' @param lower Convert names to lower case?
#' @param min_words Minimum number of label words to use per variable (if the
#'   label has that many; otherwise all available words). More words are only
#'   added when names would otherwise be duplicated. Default is 1.
#'
#' @returns The data frame with new variable names.
#'
#' @examples
#' # Example with NEPS SC6 semantic structures spGap file
#' path <- system.file("extdata", "SC6_spGap_S_15-0-0.dta", package = "nepstools")
#' df_neps <- read_neps(path, english = TRUE)
#'
#' # rename all variables by their labels
#' names(rename_by_label(df_neps))
#'
#' # keep the identifier variables, rename everything else
#' names(rename_by_label(df_neps, exclude = c(ID_t, wave, splink)))
#'
#' # rename only variables starting with "ts", use at least two label words
#' names(rename_by_label(df_neps, vars = dplyr::starts_with("ts"), min_words = 2))
#'
#' @export
rename_by_label <- function(df, vars = NULL, exclude = NULL,
                            sep = "_", lower = TRUE, min_words = 1) {
  if (!is.data.frame(df)) {
    stop("Argument 'df' must be a data.frame.")
  }
  if (!(is.character(sep) && length(sep) == 1)) {
    stop("Argument 'sep' must be a single character string.")
  }
  if (!(is.logical(lower) && length(lower) == 1 && !is.na(lower))) {
    stop("Argument 'lower' must be a single logical value (TRUE or FALSE).")
  }
  if (!(is.numeric(min_words) && length(min_words) == 1 &&
        !is.na(min_words) && min_words >= 1)) {
    stop("Argument 'min_words' must be a single number >= 1.")
  }

  # 0. Which variables are renamed? (tidyselect, like dplyr::select)
  vars_q    <- rlang::enquo(vars)
  exclude_q <- rlang::enquo(exclude)
  has_vars    <- !rlang::quo_is_null(vars_q)
  has_exclude <- !rlang::quo_is_null(exclude_q)
  if (has_vars && has_exclude) {
    stop("Use either `vars` (rename only these) or `exclude` ",
         "(rename all except these), not both.", call. = FALSE)
  }

  sel_idx <- if (has_vars) {
    unname(tidyselect::eval_select(vars_q, df, allow_rename = FALSE))
  } else if (has_exclude) {
    setdiff(seq_along(df),
            tidyselect::eval_select(exclude_q, df, allow_rename = FALSE))
  } else {
    seq_along(df)
  }
  if (length(sel_idx) == 0) return(df)

  reserved <- names(df)[-sel_idx]   # names that stay unchanged
  old_sel  <- names(df)[sel_idx]

  # 1. Collect labels; fall back to the current name if a label is missing
  labels <- vapply(df[sel_idx], function(x) {
    l <- attr(x, "label", exact = TRUE)
    if (is.null(l) || length(l) == 0 || is.na(l[1]) || !nzchar(trimws(l[1]))) {
      NA_character_
    } else {
      as.character(l[1])
    }
  }, character(1))
  labels[is.na(labels)] <- old_sel[is.na(labels)]

  # 2. Clean up and split into words
  # umlauts are matched on their UTF-8 bytes, so this works in every locale
  clean <- enc2utf8(unname(labels))
  utf8_bytes <- list(c(0xc3, 0xa4), c(0xc3, 0xb6), c(0xc3, 0xbc),  # ae oe ue
                     c(0xc3, 0x84), c(0xc3, 0x96), c(0xc3, 0x9c),  # Ae Oe Ue
                     c(0xc3, 0x9f))                                # ss
  from <- vapply(utf8_bytes, function(b) rawToChar(as.raw(b)), character(1))
  to   <- c("ae", "oe", "ue", "Ae", "Oe", "Ue", "ss")
  for (i in seq_along(from)) {
    clean <- gsub(from[i], to[i], clean, fixed = TRUE, useBytes = TRUE)
  }
  clean <- gsub("[^A-Za-z0-9]+", " ", clean, useBytes = TRUE)
  if (lower) clean <- tolower(clean)
  clean <- trimws(clean)
  words <- strsplit(clean, "\\s+")
  # labels that are empty after cleaning -> use the old variable name
  empty <- lengths(words) == 0 |
    vapply(words, function(w) all(w == ""), logical(1))
  words[empty] <- as.list(old_sel[empty])

  n_words <- lengths(words)
  k <- pmin(as.integer(min_words), n_words)  # number of words used per variable

  build <- function() {
    vapply(seq_along(words), function(i) {
      paste(words[[i]][seq_len(k[i])], collapse = sep)
    }, character(1))
  }

  # names that clash with another new name or with an unchanged variable
  clashes <- function(nm) {
    all_nm <- c(nm, reserved)
    nm %in% all_nm[duplicated(all_nm)]
  }

  # 3. Add words to clashing names until unique or no more words available
  repeat {
    nm   <- build()
    grow <- clashes(nm) & k < n_words
    if (!any(grow)) break
    k[grow] <- k[grow] + 1L
  }

  # 4. Remaining clashes (identical labels) -> suffix _1, _2, ...
  taken <- c(reserved, nm[!clashes(nm)])
  for (d in unique(nm[clashes(nm)])) {
    idx <- which(nm == d)
    j <- 0L
    for (i in idx) {
      repeat {
        j <- j + 1L
        cand <- paste0(d, sep, j)
        if (!cand %in% taken) break
      }
      nm[i] <- cand
      taken <- c(taken, cand)
    }
  }

  names(df)[sel_idx] <- nm
  df
}
