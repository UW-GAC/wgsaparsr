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
#' @param configPath Path to the WGSAParsr configuration file to load and
#' validate
#'
#' @return a tibble that can be used for building field lists for parsing
#'
#' @examples
#' \dontrun{
#' local_config <- load_config("config.tsv")
#'
#' freeze_5_config <- load_config(system.file("extdata",
#'                                            path = "fr_5_config.tsv",
#'                                            package = "wgsaparsr",
#'                                            mustWork = TRUE))
#' }
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

#' extract the appropriate fields from a configuration tibble (such as
#' produced by .load_config())
#' @noRd
.get_list_from_config <- function(config_df, which_list){
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

  if (which_list == "snv_desired"){
    return(config_df$field[config_df$SNV])
  } else if (which_list == "snv_to_split_VEP"){

  } else if (which_list == "snv_to_split_TFBS"){

  } else if (which_list == "snv_to_split_GTEx_V6"){

  } else if (which_list == "snv_post_processing"){

  } else if (which_list == "indel_desired"){
    return(config_df$field[config_df$indel])
  } else if (which_list == "indel_to_split"){

  } else if (which_list == "indel_max_columns"){

  } else if (which_list == "indel_yes_columns"){

  } else if (which_list == "indel_no_columns"){

  } else if (which_list == "indel_post_processing"){

  } else if (which_list == "dbnsfp_desired"){
    return(config_df$field[config_df$dbnsfp])
  } else if (which_list == "dbnsfp_to_split"){

  } else if (which_list == "dbnsfp_low_pairs"){

  } else if (which_list == "dbnsfp_high_pairs"){

  } else if (which_list == "dbnsfp_post_processing"){

  } else {
    stop("Unknown list.")
  }
}
