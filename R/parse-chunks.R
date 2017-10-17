#' Parse a chunk tibble from a SNV annotation file
#' @importFrom dplyr select mutate mutate_at distinct one_of vars funs "%>%"
#' @importFrom tidyr separate_rows extract
#' @importFrom stringr str_replace
#' @noRd
.parse_chunk_snv <- function(all_fields, freeze) {
  # set variables by freeze-----------------------------------------------------
  if (freeze == 4){
    WGSA_version <- "WGSA065"
    desired_columns <- .get_list("fr_4_snv_desired")
    to_split <- .get_list("fr_4_snv_to_split")
  }

  # pick desired columns--------------------------------------------------------
  selected_columns <- all_fields %>%
    select(one_of(desired_columns)) %>% # select fields of interest
    mutate(wgsa_version = WGSA_version) # add wgsa version

  # pivot the VEP_* fields------------------------------------------------------
  expanded <- selected_columns %>%
    separate_rows(one_of(to_split), sep = "\\|")

  # split the VEP_ensembl_Codon_Change_or_Distance field------------------------
  # if number, put in VEP_ensembl_Distance field
  # if string, put in VEP_ensembl_Codon_Change field
  if ("VEP_ensembl_Codon_Change_or_Distance" %in% desired_columns) {
    expanded <- expanded %>%
      extract(
        col = VEP_ensembl_Codon_Change_or_Distance, #nolint
        into = c("VEP_ensembl_Distance", "VEP_ensembl_Codon_Change"),
        regex = "(\\d*)(\\D*)"
      ) %>%
      mutate_at(vars(one_of(
        c("VEP_ensembl_Distance",
          "VEP_ensembl_Codon_Change")
      )),
      funs(str_replace(
        ., pattern = "^$", replacement = "."
      ))) # fill blanks with "."
  }
  expanded <- distinct(expanded)
}

#' Parse a chunk tibble from an indel annotation file -- PIVOT FIRST?
#' @importFrom dplyr select mutate mutate_at distinct one_of vars funs "%>%"
#' @importFrom tidyr separate_rows extract
#' @importFrom stringr str_replace
#' @noRd
.parse_chunk_indel <- function(all_fields, freeze) {
  # set variables by freeze-----------------------------------------------------
  if (freeze == 4){
    desired_columns <- .get_list("fr_4_indel_desired")
    to_split <- .get_list("fr_4_indel_to_split")
    WGSA_version <- "WGSA065"

    max_columns <- .get_list("fr_4_indel_max_columns")
    no_columns <- .get_list("fr_4_indel_no_columns")
    yes_columns <- .get_list("fr_4_indel_yes_columns")
    pair_columns <- .get_list("fr_4_indel_pair_columns")
    triple_columns <- .get_list("fr_4_indel_triple_columns")
  }

  # pick desired columns--------------------------------------------------------
  selected_columns <- all_fields %>%
    select(one_of(desired_columns)) %>% # select fields of interest
    mutate(wgsa_version = WGSA_version) # add wgsa version

  # pivot the VEP_* fields------------------------------------------------------
  expanded <- selected_columns %>%
    separate_rows(one_of(to_split), sep = "\\|")

  # split the VEP_ensembl_Codon_Change_or_Distance field------------------------
  # if number, put in VEP_ensembl_Distance field
  # if string, put in VEP_ensembl_Codon_Change field
  if ("VEP_ensembl_Codon_Change_or_Distance" %in% desired_columns) {
    expanded <- expanded %>%
      extract(
        col = VEP_ensembl_Codon_Change_or_Distance, #nolint
        into = c("VEP_ensembl_Distance", "VEP_ensembl_Codon_Change"),
        regex = "(\\d*)(\\D*)"
      ) %>%
      mutate_at(vars(one_of(
        c("VEP_ensembl_Distance",
          "VEP_ensembl_Codon_Change")
      )),
      funs(str_replace(
        ., pattern = "^$", replacement = "."
      ))) # fill blanks with "."
  }

  # parse get max columns #SLOW-------------------------------------------------
  expanded <- .parse_indel_max_columns(expanded, max_columns)

  # parse default No columns----------------------------------------------------
  expanded <- .parse_indel_no_columns(expanded, no_columns)

  # parse default Yes columns---------------------------------------------------
  expanded <- .parse_indel_yes_columns(expanded, yes_columns)

  # parse pair-columns----------------------------------------------------------
  expanded <- .parse_indel_column_pairs(expanded, pair_columns)

  # parse triple-columns--------------------------------------------------------
  expanded <- .parse_indel_column_triples(expanded, triple_columns)

  # change column names from old-version WGSA fields----------------------------
  if (freeze == 4){
    expanded <- .fix_names(expanded)
  }

  expanded <- distinct(expanded)
}


#' Parse a chunk tibble from a SNV annotation file for dbNSFP annotation
#' @importFrom dplyr select one_of filter distinct "%>%"
#' @importFrom tidyr separate_rows
#' @noRd
.parse_chunk_dbnsfp <- function(all_fields, freeze) {
  # set variables by freeze-----------------------------------------------------
  if (freeze == 4){
    desired_columns <- .get_list("fr_4_dbnsfp_desired")
    to_split <- .get_list("fr_4_dbnsfp_to_split")
    low_pairs <- .get_list("fr_4_dbnsfp_low_pairs")
    high_pairs <- .get_list("fr_4_dbnsfp_high_pairs")
    mutation_pairs <- .get_list("fr_4_dbnsfp_mutation_pairs")
  }

  # pick desired columns and rows-----------------------------------------------
  filtered_selected_columns <- all_fields %>%
    select(one_of(desired_columns)) %>% # select fields of interest
    filter(aaref != ".") %>% # select rows with dbNSFP annotation
    distinct() # trim redundant rows before expanding

    # IF NO ROWS, RETURN EMPTY TIBBLE
  if (nrow(filtered_selected_columns) == 0) {
    return(filtered_selected_columns)
  }

  # pivot the aaref, aaalt, and ensembl_geneid fields (and most others)---------
  filtered_selected_columns <-
    filtered_selected_columns %>%
    separate_rows(one_of(to_split), sep = "\\|") %>%
    separate_rows(one_of(c("Ensembl_geneid")), sep = ";") %>% # freeze 5 ok?
    distinct()

  # parse db_nsfp_low_pairs-----------------------------------------------------
  filtered_selected_columns <-
    .parse_dbnsfp_low(
      filtered_selected_columns,
      low_pairs)

  # parse db_nsfp_high_pairs----------------------------------------------------
  filtered_selected_columns <-
    .parse_dbnsfp_high(
      filtered_selected_columns,
      high_pairs)

  # parse db_nsfp_mutation_pairs------------------------------------------------
  filtered_selected_columns <-
    .parse_dbnsfp_mutation(
      filtered_selected_columns,
      mutation_pairs)

  # add aachange column---------------------------------------------------------
  filtered_selected_columns <-
    filtered_selected_columns %>%
    mutate(aachange = paste0(aaref, "/", aaalt))

  filtered_selected_columns <- distinct(filtered_selected_columns)
}
