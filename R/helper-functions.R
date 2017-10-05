# helper functions -------------------------------------------------------------

#' Check if the to_split columns are listed in desired_columns. Stop if not, and
#' return a message of columns not in desired_columns
#' @noRd
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
#' @noRd
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
#' @noRd
.has_header <- function(raw_chunk){
  any(str_detect(raw_chunk, "^#?chr\\tpos\\tref\\talt")) #nolint
}

#' Get header row from raw chunk
#' @importFrom stringr str_detect
#' @noRd
.get_header <- function(raw_chunk){
  raw_chunk[str_detect(raw_chunk, "^#?chr\\tpos\\tref\\talt")]
}

#' Check whether the source_file is WGSA indel annotation
#' @importFrom stringr str_detect
#' @noRd
.is_indel <- function(header){
  any(str_detect(header, "indel_focal_length"))
}

#' use read_tsv to read fields from raw chunk of TSV from readLines()
#' @importFrom readr read_tsv cols col_character
#' @noRd
.get_fields_from_chunk <- function(raw_chunk) {
  read_tsv(paste0(raw_chunk, collapse = "\n"),
           col_types = cols(.default = col_character()))
}

#' check whether field names include parseable fields
#' @noRd
.check_for_parseable <- function(field_names){
  any(.get_list("parseable_fields") %in% field_names)
}

#' parse columns from tibble for which we want to select maximum value
#' @importFrom dplyr mutate select
#' @importFrom rlang sym
#' @importFrom stringr str_replace_all
#' @importFrom purrr map map_dbl
#' @noRd
.parse_max_columns <- function(selected_columns){
  columns_to_max <- .get_list("parse_max")[.get_list("parse_max") %in%
                                             names(selected_columns)]

  for (to_max in columns_to_max) {
    col_name <- to_max
    unparsed_col_name <- paste0(col_name, "_unparsed")

    selected_columns <-
      suppressWarnings(
        selected_columns %>%
          mutate(
            p_clean = str_replace_all(!!sym(col_name),
                                      "(?:\\{.*?\\})|;", " "),
            p_list = strsplit(p_clean, "\\s+"),
            p_list = map(p_list, as.numeric),
            p_max = map_dbl(p_list, max, na.rm = TRUE),
            p_max = as.character(p_max),
            p_max = ifelse( (p_max == "-Inf"), ".", p_max)
          ) %>%
          select(
            -p_clean,
            -p_list,
            !!unparsed_col_name := !!col_name,
            !!col_name := p_max
          )
      )
  }
  return(selected_columns)
}

#' check string for
#' value="N" if there is aleast one "N",
#' else if "Y" present then value="Y" ,
#' else if only "." present value="."
#' @importFrom stringr str_detect
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @importFrom rlang syms
#' @noRd
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
#' @importFrom rlang syms
#' @noRd
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
#' @noRd
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
#' @noRd
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
    # parse columns for which we want max value #SLOW
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
#' @noRd
.check_names <- function(field_names){
  old_names <- .get_list("old_names")
  any(old_names %in% field_names)
}

#' change any old names to new style names
#' @noRd
.fix_names <- function(name_vector) {
  old_names <- .get_list("old_names")
  new_names <- .get_list("new_names")

  name_vector[name_vector %in% old_names] <-
    new_names[old_names %in% name_vector]

  return(name_vector)
}

#' select columns to set order and write_tsv
#' @importFrom readr write_tsv
#' @importFrom dplyr select one_of
#' @noRd
.write_to_file <- function(parsed_lines,
                           destination,
                           desired_columns,
                           header_flag,
                           indel_flag) {
  # desired_columns may have VEP_ensembl_Codon_Change_or_Distance. Fix.
  if ("VEP_ensembl_Codon_Change_or_Distance" %in% desired_columns) {
    desired_columns <- c(
      setdiff(desired_columns,
              "VEP_ensembl_Codon_Change_or_Distance"),
      "VEP_ensembl_Distance",
      "VEP_ensembl_Codon_Change"
    )
  }

  # add _unparsed columns to desired_columns for parsed indel chunk
  if (indel_flag) {
    desired_columns <- names(parsed_lines)[names(parsed_lines) %in%
                                             .get_list("all_fields")]
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
