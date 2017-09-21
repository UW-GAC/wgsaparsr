# helper functions -------------------------------------------------------------

#' Check if the to_split columns are listed in desired_columns. Stop if not, and
#' return a message of columns not in desired_columns
.check_to_split <- function(desired_columns, to_split) {
  if (all(to_split %in% desired_columns)) {
    return(TRUE)
  } else {
    msg <- paste0(
      "to_split columns missing in desired_columns: ",
      paste(to_split[!to_split %in% desired_columns], collapse = ", ")
    )
    stop(msg)
  }
}

#' Check if the desired columns exist in the source file. Stop if not, and
#' return a message of columns not in source file
.check_desired <- function(source_file, desired_columns) {
  all_fields <- get_fields(source_file)
  if (all(desired_columns %in% all_fields)) {
    return(TRUE)
  } else {
    msg <- paste0(
      "Desired columns missing in source file: ",
      paste(desired_columns[!desired_columns %in% all_fields], collapse = ", ")
    )
    stop(msg)
  }
}

#' Check if the current chunk includes a header row describing the fields
#' @importFrom stringr str_detect
.has_header <- function(raw_chunk){
  any(str_detect(raw_chunk, "^#?chr\\tpos\\tref\\talt")) #nolint
}

#' Get header row from raw chunk
#' @importFrom stringr str_detect
.get_header <- function(raw_chunk){
  raw_chunk[str_detect(raw_chunk, "^#?chr\\tpos\\tref\\talt")]
}

#' Check whether the source_file is WGSA indel annotation
#' @importFrom stringr str_detect
.is_indel <- function(header){
  any(str_detect(header, "indel_focal_length"))
}

#' use read_tsv to read fields from raw chunk of TSV from readLines()
#' @importFrom readr read_tsv cols col_character
.get_fields_from_chunk <- function(raw_chunk) {
  read_tsv(paste0(raw_chunk, collapse = "\n"),
           col_types = cols(.default = col_character()))
}

#' check whether field names include parseable fields
.check_for_parseable <- function(field_names){
  any(.get_list("parseable_fields") %in% field_names)
}

#' parse string (as from the columns in get_max) and return maximum numeric 
#' value as a string
#' @importFrom stringr str_split
.get_max_value <- function(a_string){
  if (length(a_string) > 1) {
    stop(".get_max_value isn't vectorized")
  }
  numbers <-
    suppressWarnings(
      a_string %>%
        str_split(pattern = "\\{\\d+\\}|;") %>%
        unlist() %>%
        as.numeric())

  if (all(is.na(numbers))) {
    return(".")
  } else {
    numbers %>%
      max(., na.rm = TRUE) %>%
      toString()
  }
}

#' parse columns from tibble for which we want to select maximum value
#' @importFrom dplyr mutate_at rename_at ungroup rowwise vars ends_with
.parse_max_columns <- function(selected_columns){
  columns_to_max <- .get_list("parse_max")[.get_list("parse_max") %in%
                                             names(selected_columns)]

  selected_columns <- selected_columns %>%
    rowwise() %>%
    mutate_at(vars(columns_to_max), .funs =
                funs(test = suppressWarnings(.get_max_value(.)))) %>%
    rename_at(vars(columns_to_max), funs(paste(., "unparsed", sep = "_"))) %>%
    rename_at(vars(ends_with("_test")), funs(gsub("_test", "", .))) %>%
    ungroup()
}

#' check string for
#' value="N" if there is aleast one "N",
#' else if "Y" present then value="Y" ,
#' else if only "." present value="."
#' @importFrom stringr str_detect
.check_default_no <- function(a_string) {
  if (str_detect(a_string, pattern = "N")) {
    return("N")
  } else if (str_detect(a_string, pattern = "Y")) {
    return("Y")
  } else {
    return(".")
  }
}

#' parse columns from tibble for which we want to parse to N if there is an N 
#' present
#' CAUTION - ASSUMES THAT THERE IS ONLY ONE DEFAULT NO COLUMN FOR NAMING
#' @importFrom dplyr mutate_at rename_at rowwise ungroup vars
.parse_no_columns <- function(selected_columns){
  columns_to_no <-
    .get_list("parse_string_no")[.get_list("parse_string_no") %in%
                                   names(selected_columns)]

  selected_columns <- selected_columns %>%
    rowwise() %>%
    mutate_at(vars(columns_to_no),
              .funs = funs(wacky_no_column = .check_default_no(.))) %>%
    rename_at(vars(columns_to_no),
              funs(paste(., "unparsed", sep = "_"))) %>%
    rename_at(vars(wacky_no_column),
              funs(paste(columns_to_no))) %>%
    ungroup()
}

#' check string for
#' value="Y" if there is aleast one "Y",
#' else if "N" present then value="N" ,
#' else if only "." present value="."
#' @importFrom stringr str_detect
.check_default_yes <- function(a_string) {
  if (str_detect(a_string, pattern = "Y")) {
    return("Y")
  } else if (str_detect(a_string, pattern = "N")) {
    return("N")
  } else {
    return(".")
  }
}

#' parse columns from tibble for which we want to parse to Y if there is a Y 
#' present
#' @importFrom dplyr mutate_at rename_at ungroup rowwise vars ends_with
.parse_yes_columns <- function(selected_columns){
  columns_to_yes <-
    .get_list("parse_string_yes")[.get_list("parse_string_yes") %in%
                                    names(selected_columns)]

  selected_columns <- selected_columns %>%
    rowwise() %>%
    mutate_at(vars(columns_to_yes),
              .funs = funs(test = .check_default_yes(.))) %>%
    rename_at(vars(columns_to_yes),
              funs(paste(., "unparsed", sep = "_"))) %>%
    rename_at(vars(ends_with("_test")),
              funs(gsub("_test", "", .))) %>%
    ungroup()
}

#' parse column pairs from tibble for which we want to get logical mask from 
#' first column and apply to second column
#' @importFrom dplyr mutate select rename
#' @importFrom purrr map map_dbl map2 map2_chr
.parse_column_pairs <- function(selected_columns) {
  column_pairs <-
    .get_list("parse_pairs")

  if (!all(unlist(column_pairs) %in% names(selected_columns))) {
    stop("not all pair columns are in selected_columns")
  }

  for (pair in column_pairs) {
    current_pair <- syms(pair)

    score_name <- pair[[1]]
    rankscore_name <- pair[[2]]
    unparsed_score_name <- paste0(score_name, "_unparsed")
    unparsed_rankscore_name <- paste0(rankscore_name, "_unparsed")

    selected_columns <-
      suppressWarnings(
        selected_columns %>% ungroup() %>% # thanks James!
          mutate(
            p_clean = gsub("(?:\\{.*?\\})|;", " ", x = !!current_pair[[1]]),
            p_list = strsplit(p_clean, "\\s+"),
            p_list = map(p_list, as.numeric),
            p_max = map_dbl(p_list, max, na.rm = TRUE),
            p_max = as.character(p_max),
            p_max = ifelse( (p_max == "-Inf"), ".", p_max),
            match_mask = map2(p_list, p_max, str_detect),
            match_mask = replace(match_mask, is.na(match_mask), TRUE),
            match_mask = map(match_mask,
                             function(x)
                               x & !duplicated(x)), # thanks Adrienne!
            r_clean = gsub("(?:\\{.*?\\})|;", " ", x = !!current_pair[[2]]),
            r_list =  strsplit(r_clean, "\\s+"),
            r_corresponding = map2_chr(match_mask, r_list,
                                       function(logical, string)
                                         subset(string, logical))
          ) %>%
          select(-p_clean,
                 -p_list,
                 -match_mask,
                 -r_clean,
                 -r_list) %>%
          rename(
            !!unparsed_score_name := !!current_pair[[1]],
            !!current_pair[[1]] := p_max,
            !!unparsed_rankscore_name := !!current_pair[[2]],
            !!current_pair[[2]] := r_corresponding
          )
      )
  }
  return(selected_columns)
}


#' parse column triples from tibble for which we want to get logical mask from 
#' first column and apply to second and third columns. Assumes 2nd column is 
#' rankscore.
#' @importFrom dplyr mutate select rename
#' @importFrom purrr map map_dbl map2 map2_chr
.parse_column_triples <- function(selected_columns) {
  column_triples <-
    .get_list("parse_triples")

  if (!all(unlist(column_triples) %in% names(selected_columns))) {
    stop("not all column triples are in selected_columns")
  }

  for (triple in column_triples) {
    current_triple <- syms(triple)

    score_name <- triple[[1]]
    rankscore_name <- triple[[2]]
    value_name <- triple[[3]]
    unparsed_score_name <- paste0(score_name, "_unparsed")
    unparsed_rankscore_name <- paste0(rankscore_name, "_unparsed")
    unparsed_value_name <- paste0(value_name, "_unparsed")

    selected_columns <-
      suppressWarnings(
        selected_columns %>% ungroup() %>%
          mutate(
            p_clean = gsub("(?:\\{.*?\\})|;", " ", x = !!current_triple[[1]]),
            p_list = strsplit(p_clean, "\\s+"),
            p_list = map(p_list, as.numeric),
            p_max = map_dbl(p_list, max, na.rm = TRUE),
            p_max = as.character(p_max),
            p_max = ifelse( (p_max == "-Inf"), ".", p_max),
            match_mask = map2(p_list, p_max, str_detect),
            match_mask = replace(match_mask, is.na(match_mask), TRUE),
            match_mask = map(match_mask,
                             function(x)
                               x & !duplicated(x)), # thanks Adrienne!
            r_clean = gsub("(?:\\{.*?\\})|;", " ", x = !!current_triple[[2]]),
            r_list =  strsplit(r_clean, "\\s+"),
            r_corresponding = map2_chr(match_mask, r_list,
                                       function(logical, string)
                                         subset(string, logical)),
            v_clean = gsub("(?:\\{.*?\\})|;", " ", x = !!current_triple[[3]]),
            v_list =  strsplit(v_clean, "\\s+"),
            v_corresponding = map2_chr(match_mask, v_list,
                                       function(logical, string)
                                         subset(string, logical))
          ) %>%
          select(-p_clean,
                 -p_list,
                 -r_clean,
                 -r_list,
                 -match_mask,
                 -v_clean,
                 -v_list
          ) %>%
          rename(
            !!unparsed_score_name := !!current_triple[[1]],
            !!current_triple[[1]] := p_max,
            !!unparsed_rankscore_name := !!current_triple[[2]],
            !!current_triple[[2]] := r_corresponding,
            !!unparsed_value_name := !!current_triple[[3]],
            !!current_triple[[3]] := v_corresponding
          )
      )
  }
  return(selected_columns)
}

#' Parse a chunk tibble from a SNV annotation file
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate mutate_at rename distinct one_of vars funs
#' @importFrom tidyr separate_rows extract
#' @importFrom stringr str_replace
.parse_snv_chunk <- function(all_fields,
                             desired_columns,
                             to_split,
                             WGSA_version) {

  # pick out the desired columns for further operation
  selected_columns <- all_fields %>%
    select(one_of(desired_columns)) %>% # select fields of interest
    mutate(wgsa_version = WGSA_version) # add wgsa version

  # pivot the VEP_* fields
  expanded <- selected_columns %>%
    separate_rows(one_of(to_split), sep = "\\|")

  # split the VEP_ensembl_Codon_Change_or_Distance field as follows:
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

  # if it's an older version annotation file, rename columns from WGSA fields
  # with weird characters to database column names
  if (.check_names(names(expanded))) {
    names(expanded) <- .fix_names(names(expanded))
  }

  expanded <- distinct(expanded)
}

#' Parse a chunk tibble from an indel annotation file
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate mutate_at rename distinct one_of vars funs
#' @importFrom tidyr separate_rows extract
#' @importFrom stringr str_replace
.parse_indel_chunk <- function(all_fields,
                             desired_columns,
                             to_split,
                             WGSA_version) {

  # pick out the desired columns for further operation
  selected_columns <- all_fields %>%
    select(one_of(desired_columns)) %>% # select fields of interest
    mutate(wgsa_version = WGSA_version) # add wgsa version

  # check whether desired fields require parsing; parse if so
  if (.check_for_parseable(desired_columns)){
    # parse columns for which we want max value
    selected_columns <- .parse_max_columns(selected_columns)

    # parse columns for which we want No string default
    selected_columns <- .parse_no_columns(selected_columns)

    # parse columns for which we want Yes string default
    selected_columns <- .parse_yes_columns(selected_columns)

    # parse pair-columns
    selected_columns <- .parse_column_pairs(selected_columns)

    # parse triple-columns
    selected_columns <- .parse_column_triples(selected_columns)
  }

  # pivot the VEP_* fields
  expanded <- selected_columns %>%
    separate_rows(one_of(to_split), sep = "\\|")

  # split the VEP_ensembl_Codon_Change_or_Distance field as follows:
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

  # if it's an older version annotation file, rename columns from WGSA fields
  # with weird characters to database column names
  if (.check_names(names(expanded))) {
    names(expanded) <- .fix_names(names(expanded))
  }

  expanded <- distinct(expanded)
}

#' check whether field names are the old style
.check_names <- function(field_names){
  old_names <- c(
    "`#chr`",
    "`MAP20(+-149bp)`",
    "`MAP35(+-149bp)`",
    "`GMS_single-end`",
    "`GMS_paired-end`",
    "`H1-hESC_fitCons_score`", #nolint
    "`H1-hESC_fitCons_rankscore`", #nolint
    "`H1-hESC_confidence_value`", #nolint
    "`1000G_strict_masked`",
    "`1000Gp3_AC`",
    "`1000Gp3_AF`",
    "`1000Gp3_AFR_AC`",
    "`1000Gp3_AFR_AF`",
    "`1000Gp3_EUR_AC`",
    "`1000Gp3_EUR_AF`",
    "`1000Gp3_AMR_AC`",
    "`1000Gp3_AMR_AF`",
    "`1000Gp3_EAS_AC`",
    "`1000Gp3_EAS_AF`",
    "`1000Gp3_SAS_AC`",
    "`1000Gp3_SAS_AF`",
    "`fathmm-MKL_non-coding_score`",
    "`fathmm-MKL_non-coding_rankscore`", #nolint
    "`fathmm-MKL_non-coding_group`",
    "`fathmm-MKL_coding_score`",
    "`fathmm-MKL_coding_rankscore`",
    "`fathmm-MKL_coding_pred`",
    "`fathmm-MKL_coding_group`",
    "`Eigen-raw`",
    "`Eigen-phred`",
    "`Eigen-raw_rankscore`",
    "`Eigen-PC-raw`",
    "`Eigen-PC-raw_rankscore`"
  )
  any(old_names %in% field_names)
}

#' change any old names to new style names
.fix_names <- function(name_vector) {
  old_names <- c(
    "`#chr`",
    "`MAP20(+-149bp)`",
    "`MAP35(+-149bp)`",
    "`GMS_single-end`",
    "`GMS_paired-end`",
    "`H1-hESC_fitCons_score`", #nolint
    "`H1-hESC_fitCons_rankscore`", #nolint
    "`H1-hESC_confidence_value`", #nolint
    "`1000G_strict_masked`",
    "`1000Gp3_AC`",
    "`1000Gp3_AF`",
    "`1000Gp3_AFR_AC`",
    "`1000Gp3_AFR_AF`",
    "`1000Gp3_EUR_AC`",
    "`1000Gp3_EUR_AF`",
    "`1000Gp3_AMR_AC`",
    "`1000Gp3_AMR_AF`",
    "`1000Gp3_EAS_AC`",
    "`1000Gp3_EAS_AF`",
    "`1000Gp3_SAS_AC`",
    "`1000Gp3_SAS_AF`",
    "`fathmm-MKL_non-coding_score`",
    "`fathmm-MKL_non-coding_rankscore`", #nolint
    "`fathmm-MKL_non-coding_group`",
    "`fathmm-MKL_coding_score`",
    "`fathmm-MKL_coding_rankscore`",
    "`fathmm-MKL_coding_pred`",
    "`fathmm-MKL_coding_group`",
    "`Eigen-raw`",
    "`Eigen-phred`",
    "`Eigen-raw_rankscore`",
    "`Eigen-PC-raw`",
    "`Eigen-PC-raw_rankscore`",
    "`MAP20(+-149bp)_unparsed`",
    "`MAP35(+-149bp)_unparsed`",
    "`GMS_single-end_unparsed`",
    "`GMS_paired-end_unparsed`",
    "`1000G_strict_masked_unparsed`",
    "`fathmm-MKL_non-coding_score_unparsed`",
    "`fathmm-MKL_non-coding_rankscore_unparsed`", #nolint
    "`fathmm-MKL_non-coding_group_unparsed`",
    "`fathmm-MKL_coding_score_unparsed`",
    "`fathmm-MKL_coding_rankscore_unparsed`",
    "`fathmm-MKL_coding_pred_unparsed`",
    "`fathmm-MKL_coding_group_unparsed`",
    "`Eigen-raw_unparsed`",
    "`Eigen-phred_unparsed`",
    "`Eigen-raw_rankscore_unparsed`",
    "`Eigen-PC-raw_unparsed`",
    "`Eigen-PC-raw_rankscore_unparsed`"
  )

  new_names <- c(
    "chr",
    "MAP20_149bp",
    "MAP35_149bp",
    "GMS_single_end",
    "GMS_paired_end",
    "H1_hESC_fitCons_score", #nolint
    "H1_hESC_fitCons_rankscore", #nolint
    "H1_hESC_confidence_value", #nolint
    "KGP_strict_masked",
    "KGP3_AC",
    "KGP3_AF",
    "KGP3_AFR_AC",
    "KGP3_AFR_AF",
    "KGP3_EUR_AC",
    "KGP3_EUR_AF",
    "KGP3_AMR_AC",
    "KGP3_AMR_AF",
    "KGP3_EAS_AC",
    "KGP3_EAS_AF",
    "KGP3_SAS_AC",
    "KGP3_SAS_AF",
    "fathmm_MKL_non_coding_score",
    "fathmm_MKL_non_coding_rankscore", #nolint
    "fathmm_MKL_non_coding_group",
    "fathmm_MKL_coding_score",
    "fathmm_MKL_coding_rankscore",
    "fathmm_MKL_coding_pred",
    "fathmm_MKL_coding_group",
    "Eigen_raw",
    "Eigen_phred",
    "Eigen_raw_rankscore",
    "Eigen_PC_raw",
    "Eigen_PC_raw_rankscore",
    "MAP20_149bp_unparsed",
    "MAP35_149bp_unparsed",
    "GMS_single_end_unparsed",
    "GMS_paired_end_unparsed",
    "KGP_strict_masked_unparsed",
    "fathmm_MKL_non_coding_score_unparsed",
    "fathmm_MKL_non_coding_rankscore_unparsed", #nolint
    "fathmm_MKL_non_coding_group_unparsed",
    "fathmm_MKL_coding_score_unparsed",
    "fathmm_MKL_coding_rankscore_unparsed",
    "fathmm_MKL_coding_pred_unparsed",
    "fathmm_MKL_coding_group_unparsed",
    "Eigen_raw_unparsed",
    "Eigen_phred_unparsed",
    "Eigen_raw_rankscore_unparsed",
    "Eigen_PC_raw_unparsed",
    "Eigen_PC_raw_rankscore_unparsed"
  )

  name_vector[name_vector %in% old_names] <-
    new_names[old_names %in% name_vector]
}

#' select columns to set order and write_tsv
#' @importFrom readr write_tsv
#' @importFrom dplyr select one_of
.write_to_file <- function(parsed_lines,
                           destination,
                           desired_columns,
                           header_flag) {
  # desired_columns may have VEP_ensembl_Codon_Change_or_Distance. Fix.
  if ("VEP_ensembl_Codon_Change_or_Distance" %in% desired_columns) {
    desired_columns <- c(setdiff(desired_columns,
                                 "VEP_ensembl_Codon_Change_or_Distance"),
                         "VEP_ensembl_Distance", "VEP_ensembl_Codon_Change")
  }

  # use select statement to write column headers and make sure of column order.
  if (header_flag) {
    parsed_lines %>%
      select(one_of(c(desired_columns, "wgsa_version"))) %>%
      write_tsv(path = destination, append = FALSE)
  } else {
    parsed_lines %>%
      select(one_of(c(desired_columns, "wgsa_version"))) %>%
      write_tsv(path = destination, append = TRUE)
  }
}
