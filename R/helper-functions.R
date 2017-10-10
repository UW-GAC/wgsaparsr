# helper functions -------------------------------------------------------------

#' do hack to pass devtools::check() https://stackoverflow.com/questions/9439256/
#' @importFrom utils globalVariables
#' @noRd
globalVariables(c(".", ":=", "VEP_ensembl_Codon_Change_or_Distance", "aaref",
                  "match_mask", "new_p", "p_clean", "p_list", "p_max", "p_min",
                  "r_clean", "r_corresponding", "r_list", "v_clean",
                  "v_corresponding", "v_list", "wacky_no_column"))
  


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
#' @importFrom dplyr mutate select "%>%"
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
#' @importFrom dplyr mutate_at rename_at rowwise ungroup vars "%>%"
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
#' @importFrom dplyr mutate_at rename_at ungroup rowwise vars ends_with "%>%"
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
#' @importFrom dplyr mutate select rename "%>%"
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
#' @importFrom dplyr mutate select rename "%>%"
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
#' @importFrom dplyr select one_of "%>%"
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

#' parse db_nsfp_low_pairs - select low value from pair[[1]] and corresponding
#' value from pair[[2]]
#' @importFrom dplyr mutate select rename "%>%"
#' @importFrom stringr str_split str_detect
#' @importFrom purrr map map_dbl map2 map2_chr
#' @importFrom rlang syms
#' @noRd
.parse_dbnsfp_low <- function(filtered_selected_columns, low_pairs) {
  for (pair in low_pairs) {
    current_pair <- syms(pair)
    score_name <- pair[[1]]
    pred_name <- pair[[2]]
    unparsed_score_name <- paste0(score_name, "_unparsed")
    unparsed_pred_name <- paste0(pred_name, "_unparsed")

    expanded <-
      suppressWarnings(
        filtered_selected_columns %>%
          mutate(
            p_list = str_split(!!current_pair[[1]], ";"),
            p_list = map(p_list, as.numeric),
            p_min = map_dbl(p_list, min, na.rm = TRUE),
            p_min = as.character(p_min),
            p_min = ifelse( (p_min == "Inf"), ".", p_min),
            match_mask = map2(p_list, p_min, str_detect),
            # replace NA with false
            match_mask = map(match_mask,
                             function(x)
                               replace(x, is.na(x), FALSE)),
            # if all FALSE, change all to TRUE, then keep only first
            match_mask = map(match_mask,
                             function(x)
                               if (all(x == FALSE))
                                 ! x
                             else
                               x),
            # if match_mask has more than one TRUE, keep only first TRUE
            # -- thanks Adrienne!
            match_mask = map(match_mask,
                             function(x)
                               x & !duplicated(x)),
            r_list =  str_split(!!current_pair[[2]], ";"),
            r_corresponding = map2_chr(match_mask, r_list,
                                       function(logical, string)
                                         subset(string, logical))
          ) %>%
          select(-p_list,
                 -match_mask,
                 -r_list) %>%
          rename(
            !!unparsed_score_name := !!current_pair[[1]],
            !!current_pair[[1]] := p_min,
            !!unparsed_pred_name := !!current_pair[[2]],
            !!current_pair[[2]] := r_corresponding
          )
      )
  }
  return(expanded)
}

#' parse db_nsfp_high_pairs - select high value from pair[[1]] and corresponding
#' value from pair[[2]]
#' @importFrom dplyr mutate select rename "%>%"
#' @importFrom stringr str_split str_detect
#' @importFrom purrr map map_dbl map2 map2_chr
#' @importFrom rlang syms
#' @noRd
.parse_dbnsfp_high <- function(filtered_selected_columns, high_pairs) {
  for (pair in high_pairs) {
    current_pair <- syms(pair)
    score_name <- pair[[1]]
    pred_name <- pair[[2]]
    unparsed_score_name <- paste0(score_name, "_unparsed")
    unparsed_pred_name <- paste0(pred_name, "_unparsed")

    expanded <-
      suppressWarnings(
        expanded %>%
          mutate(
            p_list = str_split(!!current_pair[[1]], ";"),
            p_list = map(p_list, as.numeric),
            p_max = map_dbl(p_list, max, na.rm = TRUE),
            p_max = as.character(p_max),
            p_max = ifelse( (p_max == "-Inf"), ".", p_max),
            match_mask = map2(p_list, p_max, str_detect),
            # replace NA with false
            match_mask = map(match_mask,
                             function(x)
                               replace(x, is.na(x), FALSE)),
            # if all FALSE, change all to TRUE, then keep only first
            match_mask = map(match_mask,
                             function(x)
                               if (all(x == FALSE))
                                 ! x
                             else
                               x),
            # if match_mask has more than one TRUE, keep only first TRUE
            # -- thanks Adrienne!
            match_mask = map(match_mask,
                             function(x)
                               x & !duplicated(x)),
            r_list =  str_split(!!current_pair[[2]], ";"),
            r_corresponding = map2_chr(match_mask, r_list,
                                       function(logical, string)
                                         subset(string, logical))
          ) %>%
          select(-p_list,
                 -match_mask,
                 -r_list) %>%
          rename(
            !!unparsed_score_name := !!current_pair[[1]],
            !!current_pair[[1]] := p_max,
            !!unparsed_pred_name := !!current_pair[[2]],
            !!current_pair[[2]] := r_corresponding
          )
      )
  }
  return(expanded)
}

#' parse db_nsfp_mutation_pairs - select character from pair[[2]] and 
#' corresponding value from pair[[1]]
#' @importFrom dplyr mutate select rename "%>%"
#' @importFrom stringr str_split str_detect
#' @importFrom purrr map map_dbl map2 map2_chr
#' @importFrom rlang syms
#' @noRd
#' 
.parse_dbnsfp_mutation <- function(filtered_selected_columns,
                                   mutation_pairs) {
  for (pair in mutation_pairs) {
    current_pair <- syms(pair)
    score_name <- pair[[1]]
    pred_name <- pair[[2]]
    unparsed_score_name <- paste0(score_name, "_unparsed")
    unparsed_pred_name <- paste0(pred_name, "_unparsed")

    expanded <-
      suppressWarnings(
        expanded %>%
          mutate(
            # If A present keep A,
            # else if D present keep D,
            # else if P present keep P,
            # else if N present keep N,
            # else .
            new_p = ifelse(
              str_detect(!!current_pair[[2]], "A"),
              "A",
              ifelse(
                str_detect(!!current_pair[[2]], "D"),
                "D",
                ifelse(
                  str_detect(!!current_pair[[2]], "P"),
                  "P",
                  ifelse(str_detect(!!current_pair[[2]], "N"), "N",
                         ".")
                )
              )
            ),
            p_list = str_split(!!current_pair[[2]], ";"),
            match_mask = map2(p_list, new_p, str_detect),
            # if match_mask has more than one TRUE, keep only first TRUE
            # -- thanks Adrienne!
            match_mask = map(match_mask,
                             function(x)
                               x & !duplicated(x)),
            r_list =  str_split(!!current_pair[[1]], ";"),
            r_corresponding = map2_chr(match_mask, r_list,
                                       function(logical, string)
                                         subset(string, logical))
          ) %>%
          select(-p_list,
                 -match_mask,
                 -r_list) %>%
          rename(
            !!unparsed_score_name := !!current_pair[[1]],
            !!current_pair[[2]] := new_p,
            !!unparsed_pred_name := !!current_pair[[2]],
            !!current_pair[[1]] := r_corresponding
          )
      )
  }
  return(expanded)
}
