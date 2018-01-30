#' remove spurious {*} strings from fields
#' @noRd
# add column_name_unparsed even if bypass? maybe if not already there -
# consider .parse_max_columns(), etc.
.parse_clean <- function(selected_columns, clean_columns){
  if (length(clean_columns) == 0){
    return(selected_columns)
  }

  # if no {*}, no parsing needed.
  if (!any(
    selected_columns %>%
    dplyr::select(clean_columns) %>%
    stringr::str_detect("(?:\\{.*?\\})"))){
    return(selected_columns)
  }

  selected_columns <-
    suppressWarnings(
      selected_columns %>%
        # first copy unparsed columns to colum_name_unparsed
        dplyr::bind_cols(select_at(
          .,
          .vars = !!clean_columns,
          .funs = funs(paste0(., "_unparsed"))
        )) %>%
        # then parse: replace the {*}s with a space
        # (using a noncapturing group in Regex) - do I need it?
        dplyr::mutate_at(.vars = vars(!!clean_columns),
                  .funs = funs(str_replace_all(., "(?:\\{.*?\\})", " "))
        ) %>%
        dplyr::mutate_at(.vars = vars(!!clean_columns),
                  .funs = funs(str_trim(., side = "right"))
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
