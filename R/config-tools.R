# functions for working with configuration files--------------------------------

#' Load and validate configuration file
#'
#' WGSAParsr configuration files are flexible tab-separated files. They must
#' include a header with field names, and the following fields, in any order:
#' \itemize{
#'   \item \strong{field} column headings matching the WGSA output file to be
#'   parsed
#'   \item \strong{SNV} logical (TRUE/FALSE) indicating whether the field
#'   should be parsed for snv annotation
#'   \item \strong{indel} logical (TRUE/FALSE) indicating whether the field
#'   should be parsed for indel annotation
#'   \item \strong{dbnsfp} logical (TRUE/FALSE) indicating whether the field
#'   should be parsed for dbnsfp annotation
#'   \item \strong{sourceGroup} numerical value for column grouping/ordering in
#'   output
#'   \item \strong{pivotGroup} numerical value to group annotations for pivoting
#'   \item \strong{pivotChar} character separating fields that should be used
#'   for pivoting
#'   \item \strong{parseGroup} numerical value to group annotations for other
#'   parsing
#'   \item \strong{transformation} a string describing the transformation to be
#'   performed. Values may include:
#'     \itemize{
#'       \item \strong{max} select the maximum value
#'       \item \strong{min} select the minimum value
#'       \item \strong{pick_Y} select "Y" if present
#'       \item \strong{pick_N} select "N" if present
#'       \item \strong{pick_A} select A>D>P>N (MutationTaster_pred field)
#'       \item \strong{clean} remove the \{n\}. E.g.: "Enhancer\{4\}" ->
#'       "Enhancer"
#'       \item \strong{distinct} select unique values. NOTE: must have a
#'       pivotGroup and pivotChar = |
#'     }
#' }
#' Other columns (such as notes) may be included in the configuration file,
#' but will not be validated or imported with this function. The configuration
#' file may include comments beginning with #.
#'
#' @param config_path Path to the WGSAParsr configuration file to load and
#' validate
#'
#' @return a tibble that can be used for building field lists for parsing
#'
#' @examples
#' \dontrun{
#' local_config <- load_config("config.tsv")
#'}
#'
#' freeze_5_config <- load_config(system.file("extdata",
#'                                            path = "fr_5_config.tsv",
#'                                            package = "wgsaparsr",
#'                                            mustWork = TRUE))
#'
#' @importFrom readr read_tsv
#' @importFrom tidyr replace_na
#' @noRd

.load_config <- function(config_path) {
  required_columns <-
    c(
      "field",
      "SNV",
      "indel",
      "dbnsfp",
      "sourceGroup",
      "pivotGroup",
      "pivotChar",
      "parseGroup",
      "transformation"
    )

  raw_config <- read_tsv(
    config_path,
    col_names = TRUE,
    comment = "#",
    col_types = cols()
  )

  if (!(all(required_columns %in% colnames(raw_config)))) {
    stop("Required columns are not in config file")
  }

  # some clean-up:

  # remove rows with field == NA, select required cols, and order output
  cleaned_config <- raw_config %>%
    filter(!(is.na(.data$field))) %>%
    select(field, SNV, indel, dbnsfp, sourceGroup, pivotGroup, #nolint
           pivotChar, parseGroup, transformation) #nolint

  # replace NA values with FALSE for some columns
  cleaned_config <- cleaned_config %>%
    replace_na(list(SNV = FALSE, indel = FALSE, dbnsfp = FALSE))

  # drop rows that don't have at a TRUE for SNV, indel, or dbnsfp
  cleaned_config <- cleaned_config %>%
    filter(.data$SNV | .data$indel | .data$dbnsfp)

  return(cleaned_config)
}


#' extract the appropriate fields from a configuration tibble (such as produced
#' by .load_config())
#'
#' @param config_df Tibble containing configuration parameters. Required columns
#'   include "field", "SNV", "indel", "dbnsfp", "sourceGroup", "pivotGroup",
#'   "pivotChar", "parseGroup", "transformation"
#'
#' @param which_list A string describing list to extract. Values may include
#'   "desired", "max", "min", "pick_y", "pick_n", "pick_a", "clean", "distinct",
#'   "pivots", or "groups"
#'
#' @param type "SNV", "indel", or "dbnsfp"
#'
#' @return list (or tibble for "pivots" or "groups") containing fields matching
#'   desired which_list and type
#'
#' @examples
#' \dontrun{
#' local_config <- load_config("config.tsv")
#'
#' freeze_5_config <- load_config(system.file("extdata",
#'                                            path = "fr_5_config.tsv",
#'                                            package = "wgsaparsr",
#'                                            mustWork = TRUE))
#'
#' snv_parse_max <- .get_list_from_config(freeze_5_config, "max", "SNV")
#' }
#' @noRd
.get_list_from_config <- function(config_df, which_list, type){
  # check arguments
  required_columns <-
    c(
      "field",
      "SNV",
      "indel",
      "dbnsfp",
      "sourceGroup",
      "pivotGroup",
      "pivotChar",
      "parseGroup",
      "transformation"
    )
  if (!(all(required_columns %in% colnames(config_df)))) {
    stop("Required columns are not in config_df")
  }
  if (!any(which_list == c("desired", "max", "min", "pick_Y", "pick_N",
                           "pick_A", "clean", "distinct", "pivots", "groups"))
      ) {
    msg = paste0('which_list must be one of: "desired", "max", "min", ',
                 '"pick_Y", "pick_N", "pick_A", "clean", "distinct", ','
                 "pivots", or "groups"')
    stop(msg)
  }
  if (!any(type == c("SNV", "indel", "dbnsfp"))) {
    stop('type must be one of: "SNV", "indel", or "dbnsfp"')
  }

  type <- as.name(type)
  if (which_list == "desired"){ # returns list
    desired_fields <- config_df %>%
      dplyr::filter(!!type) %>%
      select(field) %>%
      purrr::flatten()
    return(desired_fields)
  } else if (which_list == "max"){ #returns list
    max_fields <- config_df %>%
      dplyr::filter(.data$transformation == "max" &
                      is.na(.data$parseGroup) &
                      !!type) %>%
      dplyr::select(.data$field) %>%
      purrr::flatten()
    return(max_fields)
  } else if (which_list == "min"){ #returns list
    min_fields <- config_df %>%
      dplyr::filter(.data$transformation == "min" &
                      is.na(.data$parseGroup) &
                      !!type) %>%
      dplyr::select(.data$field) %>%
      purrr::flatten()
    return(min_fields)
  } else if (which_list == "pick_Y"){ #returns list
    y_fields <- config_df %>%
      dplyr::filter(.data$transformation == "pick_Y" &
                      is.na(.data$parseGroup) &
                      !!type) %>%
      dplyr::select(.data$field) %>%
      purrr::flatten()
    return(y_fields)
  } else if (which_list == "pick_N"){ #returns named (?) list
    n_fields <- config_df %>%
      dplyr::filter(.data$transformation == "pick_N" &
                      is.na(.data$parseGroup) &
                      !!type) %>%
      dplyr::select(.data$field) %>%
      purrr::flatten()
    return(n_fields)
  } else if (which_list == "pick_A"){ #returns list
    a_fields <- config_df %>%
      dplyr::filter(.data$transformation == "pick_A" &
                      is.na(.data$parseGroup) &
                      !!type) %>%
      dplyr::select(.data$field) %>%
      purrr::flatten()
    return(a_fields)
  } else if (which_list == "clean"){ #returns list
    clean_fields <- config_df %>%
      dplyr::filter(.data$transformation == "clean" &
                      is.na(.data$parseGroup) &
                      !!type) %>%
      dplyr::select(.data$field) %>%
      purrr::flatten()
    return(clean_fields)
  } else if (which_list == "distinct"){ #returns list
    distinct_fields <- config_df %>%
      dplyr::filter(.data$transformation == "distinct" &
                      is.na(.data$parseGroup) &
                      !!type) %>%
      dplyr::select(.data$field) %>%
      purrr::flatten()
    return(distinct_fields)
  } else if (which_list == "groups"){ # returns tibble # TEST THIS ONE OUT CAREFULLY!
    parse_groups <- config_df %>%
      dplyr::filter(!is.na(.data$parseGroup) & !!type) %>%
      dplyr::select(.data$field,
                    .data$parseGroup,
                    .data$transformation,
                    !!type) %>% # do I need this?
      dplyr::arrange(.data$parseGroup, .data$transformation)
    return(parse_groups)
  } else if (which_list == "pivots"){ # returns tibble # TEST THIS ONE OUT CAREFULLY!
    pivot_groups <- config_df %>%
      dplyr::filter(!is.na(.data$pivotGroup) & !!type) %>%
      dplyr::select(.data$field,
                    .data$pivotGroup,
                    .data$pivotChar) %>%
      dplyr::arrange(.data$pivotGroup, .data$pivotChar)
#    pivot_groups <- split(pivot_groups, pivot_groups$pivotGroup) # would make list of tibbles
    return(pivot_groups)
  } else {
    stop("Unknown list.")
  }
}
