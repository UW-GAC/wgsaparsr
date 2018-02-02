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
#' freeze_5_config <- load_config(wgsaparsr_example("fr_5_config.tsv"))
#'
#' @export

load_config <- function(config_path) {
  raw_config <- readr::read_tsv(
    config_path,
    col_names = TRUE,
    comment = "#",
    col_types = readr::cols()
  )
  .validate_config(raw_config)
  config <- .clean_config(raw_config)
  return(config)
}

#' @importFrom magrittr "%>%"
#' @noRd
.validate_config <- function(config_tibble) {
  # check required columns are there
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

  if (!(all(required_columns %in% colnames(config_tibble)))) {
    stop("Required columns are not in config tibble")
  }

  # check SNV field has allowed values
  if (!all(levels(as.factor(config_tibble$SNV)) %in% c(TRUE, FALSE))) {
    stop("SNV field has values other than TRUE, FALSE, or NA")
  }

  # check indel field is logical
  if (!all(levels(as.factor(config_tibble$indel)) %in% c(TRUE, FALSE))) {
    stop("indel field has values other than TRUE, FALSE, or NA")
  }

  # check dbnsfp field is logical
  if (!all(levels(as.factor(config_tibble$dbnsfp)) %in% c(TRUE, FALSE))) {
    stop("dbnsfp field has values other than TRUE, FALSE, or NA")
  }

  # check transformation field for allowed values
  if (!all(levels(as.factor(config_tibble$transformation)) %in%
           c("max", "min", "pick_Y", "pick_N", "pick_A", "clean",
             "distinct"))) {
    stop("transformation field has unrecognized values")
  }

  # pivotChar is the same within pivotGroup
  char_count <- config_tibble %>%
    dplyr::filter(!is.na(.$pivotGroup)) %>% #nolint
    dplyr::group_by(pivotGroup) %>% #nolint
    dplyr::summarize_at(
      .funs = dplyr::funs(dplyr::n_distinct(levels(as.factor(.)))),
      .vars = "pivotChar")

    if (any(char_count$pivotChar != 1)) { #nolint
      stop("all pivotChar values must be the same withinin a pivotGroup")
    }

  # transformation is the same within parseGroup

  # first, define a summary function to check whether any transformation
  # fields are not in the same as the first one in that group or NA
  # for example, a pair, transformation may be c("max", NA), but cannot
  # be c("max", "min")
  check_too_many <- function(character) {
    !all(character %in% c(character[[1]], NA))
  }

  # now group the transformations by parseGroup and summarize
  # with check_too_many()
  trans_count <- config_tibble %>%
    dplyr::filter(!is.na(.$parseGroup)) %>% #nolint
    dplyr::group_by(parseGroup) %>% #nolint
    dplyr::arrange(transformation, .by_group = TRUE) %>%
    dplyr::summarize_at(
      .funs = dplyr::funs(check_too_many(.)),
      .vars = "transformation")

  if (any(trans_count$transformation)) {
    stop("all transformation values must be the same withinin a parseGroup")
  }

  # other validation possibilities:
  # values in config_tibble$field match column headings in WGSA file
  # sourceGroup numerical values
  # pivotGroup numerical values
  # pivotChar single character
  # parseGroup numerical values
}

#' @importFrom magrittr "%>%"
#' @noRd
.clean_config <- function(config_tibble) {
  # remove rows with field == NA, select required cols, and order output
  cleaned_config <- config_tibble %>%
    dplyr::filter(!(is.na(.data$field))) %>%
    dplyr::select(field, SNV, indel, dbnsfp, sourceGroup, pivotGroup, #nolint
           pivotChar, parseGroup, transformation) #nolint

  # replace NA values with FALSE for some columns
  cleaned_config <- cleaned_config %>%
    tidyr::replace_na(list(SNV = FALSE, indel = FALSE, dbnsfp = FALSE))

  # drop rows that don't have at a TRUE for SNV, indel, or dbnsfp
  cleaned_config <- cleaned_config %>%
    dplyr::filter(.data$SNV | .data$indel | .data$dbnsfp)

  return(cleaned_config)
}


#' extract the appropriate fields from a configuration tibble (such as produced
#' by loadConfig())
#'
#' @param config_df Tibble containing configuration parameters. Required columns
#'   include "field", "SNV", "indel", "dbnsfp", "sourceGroup", "pivotGroup",
#'   "pivotChar", "parseGroup", "transformation"
#'
#' @param which_list A string describing list to extract. Values may include
#'   "desired", "max", "min", "pick_y", "pick_n", "pick_a", "clean", "distinct",
#'   "pivots", or "groups"
#'
#' @param list_type "SNV", "indel", "dbnsfp", or "all"
#'
#' @return list (or tibble for "pivots" or "groups") containing fields matching
#'   desired which_list and list_type
#'
#' @examples
#' \dontrun{
#' local_config <- loadConfig("config.tsv")
#'
#' freeze_5_config <- loadConfig(system.file("extdata",
#'                                            path = "fr_5_config.tsv",
#'                                            package = "wgsaparsr",
#'                                            mustWork = TRUE))
#'
#' snv_parse_max <- .getListFromConfig(freeze_5_config, "max", "SNV")
#' }
#' 
#' @importFrom magrittr "%>%"
#' @noRd

.get_list_from_config <- function(config_df, which_list, list_type){
  # check arguments
  .validate_config(config_df)
  if (!any(which_list == c("desired", "max", "min", "pick_Y", "pick_N",
                           "pick_A", "clean", "distinct", "pivots", "groups"))
      ) {
    msg <- paste0('which_list must be one of: "desired", "max", "min", ',
                 '"pick_Y", "pick_N", "pick_A", "clean", "distinct", ',
                 '"pivots", or "groups"')
    stop(msg)
  }
  if (!any(list_type == c("SNV", "indel", "dbnsfp", "all"))) {
    stop('list_type must be one of: "SNV", "indel", "dbnsfp", or "all"')
  }

  # first filter by list_type
  if (list_type == "all") {
    fields_by_list_type <- config_df
  } else {
    list_type <- rlang::sym(list_type) # I confess I don't understand this well
    fields_by_list_type <- config_df %>% dplyr::filter(!!list_type)
  }

  if (which_list == "desired") {
    # returns list
    desired_fields <- fields_by_list_type %>%
      dplyr::select(field) %>%
      purrr::flatten()
    return(desired_fields)
  } else if (which_list == "max") {
    # returns list
    max_fields <- fields_by_list_type %>%
      dplyr::filter(.data$transformation == "max" &
                      is.na(.data$parseGroup)) %>% #nolint
      dplyr::select(.data$field) %>%
      purrr::flatten()
    return(max_fields)
  } else if (which_list == "min") {
    # returns list
    min_fields <- fields_by_list_type %>%
      dplyr::filter(.data$transformation == "min" &
                      is.na(.data$parseGroup)) %>% #nolint
      dplyr::select(.data$field) %>%
      purrr::flatten()
    return(min_fields)
  } else if (which_list == "pick_Y") {
    #returns list
    y_fields <- fields_by_list_type %>%
      dplyr::filter(.data$transformation == "pick_Y" &
                      is.na(.data$parseGroup)) %>% #nolint
      dplyr::select(.data$field) %>%
      purrr::flatten()
    return(y_fields)
  } else if (which_list == "pick_N") {
    #returns list
    n_fields <- fields_by_list_type %>%
      dplyr::filter(.data$transformation == "pick_N" &
                      is.na(.data$parseGroup)) %>% #nolint
      dplyr::select(.data$field) %>%
      purrr::flatten()
    return(n_fields)
  } else if (which_list == "pick_A") {
    #returns list
    a_fields <- fields_by_list_type %>%
      dplyr::filter(.data$transformation == "pick_A" &
                      is.na(.data$parseGroup)) %>% #nolint
      dplyr::select(.data$field) %>%
      purrr::flatten()
    return(a_fields)
  } else if (which_list == "clean") {
    #returns list
    clean_fields <- fields_by_list_type %>%
      dplyr::filter(.data$transformation == "clean" &
                      is.na(.data$parseGroup)) %>% #nolint
      dplyr::select(.data$field) %>%
      purrr::flatten()
    return(clean_fields)
  } else if (which_list == "distinct") {
    #returns list
    distinct_fields <- fields_by_list_type %>%
      dplyr::filter(.data$transformation == "distinct" &
                      is.na(.data$parseGroup)) %>% #nolint
      dplyr::select(.data$field) %>%
      purrr::flatten()
    return(distinct_fields)
  } else if (which_list == "groups") {
    # returns tibble # TEST THIS ONE OUT CAREFULLY!
    parse_groups <- fields_by_list_type %>%
      dplyr::filter(!is.na(.data$parseGroup)) %>% #nolint
      dplyr::select(.data$field,
                    .data$parseGroup, #nolint
                    .data$transformation) %>%
      dplyr::arrange(.data$parseGroup, .data$transformation) #nolint
    return(parse_groups)
  } else if (which_list == "pivots") {
    # returns tibble # TEST THIS ONE OUT CAREFULLY!
    pivot_groups <- fields_by_list_type %>%
      dplyr::filter(!is.na(.data$pivotGroup)) %>% #nolint
      dplyr::select(.data$field,
                    .data$pivotGroup, #nolint
                    .data$pivotChar) %>% #nolint
      dplyr::arrange(.data$pivotGroup, .data$pivotChar) #nolint
    # could split(pivot_groups, pivot_groups$pivotGroup) to make list of tibbles
    return(pivot_groups)
  } else {
    stop("Unknown list.")
  }
}
