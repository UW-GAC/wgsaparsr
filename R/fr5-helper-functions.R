#' add column_name_unparsed column to tibble prior to parsing (for debugging, mostly)
#' @import dplyr
#' @noRd
.preserve_raw <- function(selected_columns, to_parse) {
  selected_columns <- selected_columns %>%
    # first copy unparsed columns to colum_name_unparsed
    bind_cols(select_at(
      .,
      .vars = !!to_parse,
      .funs = funs(paste0(., "_unparsed"))
    ))
  return(selected_columns)
}

#' remove spurious {*} strings from fields:
#' .{n} -> .
#' .{n}; -> .;
#' .{n}. -> .,. (or .{n}. -> . ?)
#' 
#' import dplyr
#' @noRd
.parse_clean <- function(selected_columns, to_clean){
  if (length(to_clean) == 0){
    return(selected_columns)
  }

  # if no {*}, no parsing needed.
  if (!any(
    selected_columns %>%
    dplyr::select(to_clean) %>%
    stringr::str_detect("\\{[^\\}]+\\}"))){
    return(selected_columns)
  }

  selected_columns <-
    selected_columns %>%
    #.{n} -> .
    dplyr::mutate_at(.vars = to_clean,
                     .funs = dplyr::funs(
                       stringr::str_replace_all(., "\\{[^\\}]+\\}$", "")
                       )
                     ) %>%
    # .{n}; -> .;
    dplyr::mutate_at(.vars = to_clean,
                     .funs = dplyr::funs(
                       stringr::str_replace_all(., "\\{[^\\}]+\\};", ";")
                       )
                     ) %>%
    # .{n}. -> .,. (or should it be .{n}. -> . ?)
    dplyr::mutate_at(.vars = to_clean,
                     .funs = dplyr::funs(
                       stringr::str_replace_all(., "\\.\\{[^\\}]+\\}(?!;)", ".,")
                       )
                     )
  return(selected_columns)
}

#' pick maximum value from compound entry in column
#' @noRd
.parse_max_columns <- function(selected_columns, max_columns) {
  if (length(max_columns) == 0){
    return(selected_columns)
  }

  # if no ;, only single values, so call parseClean and return
  if (!any(
    selected_columns %>%
    dplyr::select(max_columns) %>%
    stringr::str_detect(";"))){
    return(.parse_clean(selected_columns, max_columns))
  }

  selected_columns <-
    suppressWarnings(
      selected_columns %>%
        # first copy unparsed columns to colum_name_unparsed
        dplyr::bind_cols(
          dplyr::select_at(.,
                    .vars = !!max_columns,
                    .funs = funs(paste0(., "_unparsed")))) %>%
        # next replace ; or {*} with a space
        dplyr::mutate_at(
          .vars = vars(!!max_columns),
          .funs = funs(str_replace_all(., "(?:\\{.*?\\})|;", " "))
        ) %>%
        # trim final space to be safe
        dplyr::mutate_at(
          .vars = vars(!!max_columns),
          .funs = funs(str_replace(., "\\s+$", ""))
        ) %>%
        # split the string at the space
        dplyr::mutate_at(
          .vars = vars(!!max_columns),
          .funs = funs(str_split(., "\\s+"))
        ) %>%
        # make values numeric
        dplyr::mutate_at(
          .vars = vars(!!max_columns),
          .funs = funs(map(., as.numeric))
        ) %>%
        # get the max values
        dplyr::mutate_at(
          .vars = vars(!!max_columns),
          .funs = funs(map_dbl(., max, na.rm = TRUE))
        ) %>%
        # change to character
        dplyr::mutate_at(
          .vars = vars(!!max_columns),
          .funs = funs(as.character)
        ) %>%
        # change "-Inf" to "."
        dplyr::mutate_at(
          .vars = vars(!!max_columns),
          .funs = funs(ifelse((. == "-Inf"), ".", .)) #nolint
        )
    )
  return(selected_columns)
}
