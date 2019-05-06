# functions for working with configuration files--------------------------------

#' @importFrom magrittr "%>%"
#' @importFrom rlang UQ
#' @noRd
.clean_config <- function(config_tibble) {
  # remove rows with field == NA, select required and optional cols, and order
  # output

  # required columns in config file
  desired_columns <-
    c("field", "SNV", "indel", "dbnsfp", "pivotGroup", "pivotChar",
      "parseGroup", "transformation")

  # add in optional columns from config file
  if ("outputOrder" %in% colnames(config_tibble)) {
    desired_columns <- append(desired_columns, "outputOrder")
  }

  if ("outputName" %in% colnames(config_tibble)) {
    desired_columns <- append(desired_columns, "outputName")
  }

  if ("toRemove" %in% colnames(config_tibble)) {
    desired_columns <- append(desired_columns, "toRemove")

  # make the toRemove values into regular expressions (don't get carried away!)
    cleaned_config <- config_tibble %>%
      # first replace "." with "\\."
      dplyr::mutate_at(dplyr::vars(toRemove), #nolint
                       dplyr::funs(
                         stringr::str_replace(
                           ., "^\\.$", "\\\\."))) %>%#nolint
      # prepend "^" for start of string
      dplyr::mutate_at(dplyr::vars(toRemove), #nolint
                       dplyr::funs(paste0("^", .))) %>%
      # append "$" for end of string
      dplyr::mutate_at(dplyr::vars(toRemove), #nolint
                       dplyr::funs(paste0(., "$")))
  } else {
    cleaned_config <- config_tibble
  }

  # remove rows that don't have field names and select desired columns
  cleaned_config <- cleaned_config %>%
    dplyr::filter(!(is.na(.data$field))) %>%
    dplyr::select(UQ(desired_columns))

  # replace NA values in config file with FALSE for some columns
  cleaned_config <- cleaned_config %>%
    tidyr::replace_na(list(SNV = FALSE, indel = FALSE, dbnsfp = FALSE))

  # drop rows that don't have at a TRUE for SNV, indel, or dbnsfp
  cleaned_config <- cleaned_config %>%
    dplyr::filter(.data$SNV | .data$indel | .data$dbnsfp)

  # sort the rows by the outputOrder column, if it's there
  if ("outputOrder" %in% colnames(cleaned_config)) {
    cleaned_config <- cleaned_config %>%
      dplyr::arrange(outputOrder) #nolint
  }

  return(cleaned_config)
}


#' Validate configuration tibble
#'
#' WGSAParsr configuration can be loaded from a file with load_config() or
#' passed as a data frame/tibble. This function validates the tibble as meeting
#' the following criteria:
#'
#' Contains required columns: "field", "SNV", "indel", "dbnsfp", "pivotGroup",
#'   "pivotChar", "parseGroup", "transformation".
#'
#' the SNV, indel, and dbnsfp fields may contain NA or logical values
#'
#' the transformation field may contain NA or "max", "min", "pick_Y", "pick_N",
#' "pick_A", "clean", "distinct".
#'
#' all members of each pivotGroup must have the same pivotChar
#'
#' a parseGroup may not have multiple transformations
#'
#' @param config_tibble tibble containing parsing configuration details.
#'
#' @examples
#' \dontrun{
#'
#' validate_config(config_tibble)
#' }
#'
#' @importFrom magrittr "%>%"
#' @export
validate_config <- function(config_tibble) {
  # check required columns are there
  required_columns <-
    c(
      "field",
      "SNV",
      "indel",
      "dbnsfp",
      "pivotGroup",
      "pivotChar",
      "parseGroup",
      "transformation"
    )

  if (!(all(required_columns %in% colnames(config_tibble)))) {
    stop("Required columns missing")
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

  # if outputOrder is a column, are rows in order?
  if ("outputOrder" %in% colnames(config_tibble)) {
    if (is.unsorted(config_tibble$outputOrder)) { #nolint
      stop("configuration rows not arranged by outputOrder")
    }
  }

  # other validation possibilities:
  # values in config_tibble$field match column headings in WGSA file
  # pivotGroup numerical values
  # pivotChar single character
  # parseGroup numerical values
}

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
#'
#' Additionally, the following fields may be included, and are processed during
#' configuration file loading, but are not required:
#' \itemize{
#'   \item \strong{outputOrder} numerical value for column ordering in parsed
#'   output
#'   #'   \item \strong{outputName} a string that should be used for the field
#'   name in the output file (useful for renaming fields)
#'   \item \strong{toRemove} any characters to remove in the output tsv. For
#'   example, if a WGSA field uses a character used to encode a NULL value, it
#'   may need to be removed to facilitate database import. If this field is
#'   included in the config file, WGSAParsr will convert the specified string to
#'   a blank field in output
#' }
#'
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
#' @importFrom rlang UQ
#' @export

load_config <- function(config_path) {
  raw_config <- readr::read_tsv(
    config_path,
    col_names = TRUE,
    comment = "#",
    col_types = readr::cols()
  )
  validate_config(raw_config)
  config <- .clean_config(raw_config)
  return(config)
}

#' extract the appropriate fields from a configuration tibble (such as produced
#' by load_config())
#'
#' @param config_df Tibble containing configuration parameters. Required columns
#'   include "field", "SNV", "indel", "dbnsfp", "pivotGroup", "pivotChar",
#'   "parseGroup", and "transformation". Optional columns include "outputOrder",
#'   "outputName", and "toRemove".
#'
#' @param which_list A string describing list to extract. Values may include
#'   "desired", "max", "min", "pick_y", "pick_n", "pick_a", "clean", "distinct",
#'   "pivots", "max_pairs", "min_pairs", "pick_Y_pairs", "pick_N_pairs",
#'   or "pick_A_pairs"
#'
#' @param list_type "SNV", "indel", "dbnsfp", or "all"
#'
#' @return list (or tibble for "pivots") containing fields matching desired
#'   which_list and list_type
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
#' snv_parse_max <- .getListFromConfig(freeze_5_config, "max", "SNV")
#' }
#'
#' @importFrom magrittr "%>%"
#' @noRd

.get_list_from_config <- function(config_df, which_list, list_type){
  # check arguments
  validate_config(config_df)
  if (!any(which_list == c("desired", "max", "min", "pick_Y", "pick_N",
                           "pick_A", "clean", "distinct", "pivots",
                           "max_pairs", "min_pairs", "pick_Y_pairs",
                           "pick_N_pairs", "pick_A_pairs"))
      ) {
    msg <- paste0('which_list must be one of: "desired", "max", "min", ',
                 '"pick_Y", "pick_N", "pick_A", "clean", "distinct", ',
                 '"pivots", "max_pairs", "min_pairs", "pick_Y_pairs", ',
                 '"pick_N_pairs", or "pick_A_pairs"')
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
    fields_by_list_type <-
      config_df %>%
      dplyr::filter(as.logical(UQ(list_type)))
  }

  if (which_list == "desired") {
    # returns list ## PERHAPS ORDER HERE? - not needed b/c clean config ordered
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
  } else if (which_list == "pivots") {
    # returns tibble # TEST THIS ONE OUT CAREFULLY!
    pivot_groups <- fields_by_list_type %>%
      dplyr::filter(!is.na(.data$pivotGroup)) %>% #nolint
      dplyr::select(.data$field,
                    .data$pivotGroup, #nolint
                    .data$pivotChar) %>% #nolint
      dplyr::arrange(.data$pivotGroup, .data$pivotChar) %>% #nolint
      split(.$pivotGroup) %>% #nolint
      purrr::map(~dplyr::select(., -pivotGroup)) #nolint
    return(pivot_groups)
  } else if (which_list == "max_pairs") {
    # returns list # TEST THIS ONE OUT CAREFULLY!
    group_tibble <- fields_by_list_type %>%
      dplyr::filter(!is.na(.data$parseGroup)) %>% #nolint
      dplyr::select(.data$field,
                    .data$parseGroup, #nolint
                    .data$transformation) %>%
      dplyr::arrange(.data$parseGroup, .data$transformation) %>% #nolint
      dplyr::group_by(parseGroup) %>% #nolint
      dplyr::filter(any(.data$transformation == "max")) %>%
      dplyr::ungroup()
    parse_groups <- split(group_tibble$field, group_tibble$parseGroup) #nolint
    return(parse_groups)
  } else if (which_list == "min_pairs") {
    # returns list # TEST THIS ONE OUT CAREFULLY!
    group_tibble <- fields_by_list_type %>%
      dplyr::filter(!is.na(.data$parseGroup)) %>% #nolint
      dplyr::select(.data$field,
                    .data$parseGroup, #nolint
                    .data$transformation) %>%
      dplyr::arrange(.data$parseGroup, .data$transformation) %>% #nolint
      dplyr::group_by(parseGroup) %>% #nolint
      dplyr::filter(any(.data$transformation == "min")) %>%
      dplyr::ungroup()
    parse_groups <- split(group_tibble$field, group_tibble$parseGroup) #nolint
    return(parse_groups)
  } else if (which_list == "pick_Y_pairs") {
    # returns list # TEST THIS ONE OUT CAREFULLY!
    group_tibble <- fields_by_list_type %>%
      dplyr::filter(!is.na(.data$parseGroup)) %>% #nolint
      dplyr::select(.data$field,
                    .data$parseGroup, #nolint
                    .data$transformation) %>%
      dplyr::arrange(.data$parseGroup, .data$transformation) %>% #nolint
      dplyr::group_by(parseGroup) %>% #nolint
      dplyr::filter(any(.data$transformation == "pick_Y")) %>%
      dplyr::ungroup()
    parse_groups <- split(group_tibble$field, group_tibble$parseGroup) #nolint
    return(parse_groups)
  } else if (which_list == "pick_N_pairs") {
    # returns list # TEST THIS ONE OUT CAREFULLY!
    group_tibble <- fields_by_list_type %>%
      dplyr::filter(!is.na(.data$parseGroup)) %>% #nolint
      dplyr::select(.data$field,
                    .data$parseGroup, #nolint
                    .data$transformation) %>%
      dplyr::arrange(.data$parseGroup, .data$transformation) %>% #nolint
      dplyr::group_by(parseGroup) %>% #nolint
      dplyr::filter(any(.data$transformation == "pick_N")) %>%
      dplyr::ungroup()
    parse_groups <- split(group_tibble$field, group_tibble$parseGroup) #nolint
    return(parse_groups)
  } else if (which_list == "pick_A_pairs") {
    # returns list # TEST THIS ONE OUT CAREFULLY!
    group_tibble <- fields_by_list_type %>%
      dplyr::filter(!is.na(.data$parseGroup)) %>% #nolint
      dplyr::select(.data$field,
                    .data$parseGroup, #nolint
                    .data$transformation) %>%
      dplyr::arrange(.data$parseGroup, .data$transformation) %>% #nolint
      dplyr::group_by(parseGroup) %>% #nolint
      dplyr::filter(any(.data$transformation == "pick_A")) %>%
      dplyr::ungroup()
    parse_groups <- split(group_tibble$field, group_tibble$parseGroup) #nolint
    return(parse_groups)
  } else {
    stop("Unknown list.")
  }
}
