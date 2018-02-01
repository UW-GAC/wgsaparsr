# new internal helper functions for freeze 5 refactoring -----------------------

#' add column_name_unparsed column to tibble prior to parsing (for debugging, 
#' mostly)
#' @importFrom magrittr "%>%"
#' @noRd
.preserve_raw <- function(selected_columns, to_parse) {
  selected_columns <- selected_columns %>%
    # first copy unparsed columns to colum_name_unparsed
    dplyr::bind_cols(dplyr::select_at(
      .,
      .vars = to_parse,
      .funs = dplyr::funs(paste0(., "_unparsed"))
    ))
  return(selected_columns)
}

#' remove spurious {*} strings from fields:
#' .{n} -> .
#' .{n}; -> .;
#' .{n}. -> .,. (or .{n}. -> . ?)
#' 
#' @importFrom magrittr "%>%"
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
    # .{n}. -> .,. (or should it be .{n}. -> . ? or .{n}. -> .;. ?)
    dplyr::mutate_at(.vars = to_clean,
                     .funs = dplyr::funs(
                       stringr::str_replace_all(.,
                        "\\.\\{[^\\}]+\\}(?!;)", ".,")
                       )
                     )
  return(selected_columns)
}

#' pick maximum value from compound entry in column
#' @importFrom magrittr "%>%"
#' @noRd

.parse_max_columns <- function(selected_columns, max_columns) {
  if (length(max_columns) == 0){
    return(selected_columns)
  }

  # if no ; or {*}, only single values, so no parsing needed
  if (!any(
    selected_columns %>%
    dplyr::select(max_columns) %>%
    stringr::str_detect("\\{[^\\}]+\\}|;"))){
    return(selected_columns)
  }

  # if ; or {*}, replace with a space, split on whitespace, and return max
  # value
  selected_columns <-
    suppressWarnings(
      selected_columns %>%
        # replace ; or {*} with a space
        dplyr::mutate_at(
          .vars = max_columns,
          .funs = dplyr::funs(
            stringr::str_replace_all(., "(?:\\{.*?\\})|;", " "))
        ) %>%
        # trim white space padding to be safe
        dplyr::mutate_at(
          .vars = max_columns,
          .funs = dplyr::funs(stringr::str_trim(., side = "both"))
        ) %>%
        # also trim multiple spaces to be safe
        dplyr::mutate_at(
          .vars = max_columns,
          .funs = dplyr::funs(stringr::str_replace(., "\\s{2,}", " ")) #nolint
        ) %>%
        # split the string at the space
        dplyr::mutate_at(
          .vars = max_columns,
          .funs = dplyr::funs(stringr::str_split(., "\\s+"))
        ) %>%
        # make values numeric
        dplyr::mutate_at(
          .vars = max_columns,
          .funs = dplyr::funs(purrr::map(., as.numeric))
        ) %>%
        # get the max values
        dplyr::mutate_at(
          .vars = max_columns,
          .funs = dplyr::funs(purrr::map_dbl(., max, na.rm = TRUE))
        ) %>%
        # change to character
        dplyr::mutate_at(
          .vars = max_columns,
          .funs = dplyr::funs(as.character)
        ) %>%
        # change "-Inf" to "."
        dplyr::mutate_at(
          .vars = max_columns,
          .funs = dplyr::funs(ifelse((. == "-Inf"), ".", .)) #nolint
        )
    )
  return(selected_columns)
}

#' pick minimmum value from compound entry in column
#' @importFrom magrittr "%>%"
#' @noRd

.parse_min_columns <- function(selected_columns, min_columns) {
  if (length(min_columns) == 0){
    return(selected_columns)
  }

  # if no ; or {*}, only single values, so no parsing needed
  if (!any(
    selected_columns %>%
    dplyr::select(min_columns) %>%
    stringr::str_detect("\\{[^\\}]+\\}|;"))){
    return(selected_columns)
  }

  # if ; or {*}, replace with a space, split on whitespace, and return min
  # value
  selected_columns <-
    suppressWarnings(
      selected_columns %>%
        # replace ; or {*} with a space
        dplyr::mutate_at(
          .vars = min_columns,
          .funs = dplyr::funs(
            stringr::str_replace_all(., "(?:\\{.*?\\})|;", " "))
        ) %>%
        # trim white space padding to be safe
        dplyr::mutate_at(
          .vars = min_columns,
          .funs = dplyr::funs(stringr::str_trim(., side = "both"))
        ) %>%
        # also trim multiple spaces to be safe
        dplyr::mutate_at(
          .vars = min_columns,
          .funs = dplyr::funs(stringr::str_replace(., "\\s{2,}", " ")) #nolint
        ) %>%
        # split the string at the space
        dplyr::mutate_at(
          .vars = min_columns,
          .funs = dplyr::funs(stringr::str_split(., "\\s+"))
        ) %>%
        # make values numeric
        dplyr::mutate_at(
          .vars = min_columns,
          .funs = dplyr::funs(purrr::map(., as.numeric))
        ) %>%
        # get the min values
        dplyr::mutate_at(
          .vars = min_columns,
          .funs = dplyr::funs(purrr::map_dbl(., min, na.rm = TRUE))
        ) %>%
        # change to character
        dplyr::mutate_at(
          .vars = min_columns,
          .funs = dplyr::funs(as.character)
        ) %>%
        # change "Inf" to "."
        dplyr::mutate_at(
          .vars = min_columns,
          .funs = dplyr::funs(ifelse((. == "Inf"), ".", .)) #nolint
        )
    )
  return(selected_columns)
}

# TODO: is there a way to short-circuit parsing on trivial case?
#' @importFrom magrittr "%>%"
#' @noRd
.parse_yes_columns <- function(selected_columns, yes_columns){
  if (length(yes_columns) == 0){
    return(selected_columns)
  }
  selected_columns <-
    suppressWarnings(
      selected_columns %>%
        # parse:  Y if Y present, else N if N present, else .
        dplyr::mutate_at(.vars = yes_columns,
                         .funs = dplyr::funs(ifelse(
                           stringr::str_detect(., "Y"),
                           "Y",
                           ifelse(stringr::str_detect(., "N"),
                                  "N", ".")
                         )))
    )
  return(selected_columns)
}

#' @noRd
.last <- function() {
  message("You're a rock star!")
}

#' Get path to load wgsaparsr example
#' 
#' \code{wgsaparsr} comes bundled with sample files in its \code{inst/extdata}
#' diretory. This function makes them easier to access. Based on
#' \code{readr::readr_example()}
#' 
#' @param path Name of file
#'
#' @examples
#' wgsaparsr_example(path = "fr_5_config.tsv")
#' 
#' @export
wgsaparsr_example <- function(path) {
  system.file("extdata", path, package = "wgsaparsr", mustWork = TRUE)
}
