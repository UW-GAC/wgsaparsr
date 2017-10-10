#' Parse a chunk tibble from a SNV annotation file
#' @importFrom dplyr select mutate mutate_at distinct one_of vars funs "%>%"
#' @importFrom tidyr separate_rows extract
#' @importFrom stringr str_replace
#' @noRd
.parse_chunk_snv <- function(all_fields,
                             desired_columns,
                             to_split,
                             WGSA_version) {

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

  #rename columns from old-version WGSA fields---------------------------------
  if (.check_names(names(expanded))) {
    names(expanded) <- .fix_names(names(expanded))
  }

  expanded <- distinct(expanded)
}

#' Parse a chunk tibble from an indel annotation file -- PIVOT FIRST?
#' @importFrom dplyr select mutate mutate_at distinct one_of vars funs "%>%"
#' @importFrom tidyr separate_rows extract
#' @importFrom stringr str_replace
#' @noRd
.parse_chunk_indel <- function(all_fields,
                               desired_columns,
                               to_split,
                               WGSA_version) {

  # pick desired columns--------------------------------------------------------
  selected_columns <- all_fields %>%
    select(one_of(desired_columns)) %>% # select fields of interest
    mutate(wgsa_version = WGSA_version) # add wgsa version

  # check whether desired fields require parsing; parse if so
  if (.check_for_parseable(desired_columns)){
    # get max columns #SLOW-----------------------------------------------------
    selected_columns <- .parse_max_columns(selected_columns)

    # default No columns--------------------------------------------------------
    selected_columns <- .parse_no_columns(selected_columns)

    # default Yes columns-------------------------------------------------------
    selected_columns <- .parse_yes_columns(selected_columns)

    # pair-columns--------------------------------------------------------------
    selected_columns <- .parse_column_pairs(selected_columns)

    # triple-columns------------------------------------------------------------
    selected_columns <- .parse_column_triples(selected_columns)
  }

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

  # rename columns from old-version WGSA fields---------------------------------
  if (.check_names(names(expanded))) {
    names(expanded) <- .fix_names(names(expanded))
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
    desired_columns <- .get_list("dbnsfp_desired_fr_4")
    to_split <- .get_list("dbnsfp_to_split_fr_4")
    low_pairs <- .get_list("dbnsfp_low_pairs_fr_4")
    high_pairs <- .get_list("dbnsfp_high_pairs_fr_4")
    mutation_pairs <- .get_list("dbnsfp_mutation_pairs_fr_4")
  }

  # pick out the desired columns for further operation--------------------------
  filtered_selected_columns <- all_fields %>%
    select(one_of(desired_columns)) %>% # select fields of interest
    filter(aaref != ".") %>% # select rows with dbNSFP annotation
    distinct() # trim redundant rows before expanding

  # pivot the aaref, aaalt, and ensembl_geneid fields---------------------------
  filtered_selected_columns <-
    filtered_selected_columns %>%
    separate_rows(one_of(to_split), sep = "\\|")

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

  filtered_selected_columns <- distinct(filtered_selected_columns)
}
