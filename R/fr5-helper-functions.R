# new internal helper functions from freeze 5 refactoring ----------------------

#' hack to pass devtools::check()
#' see: https://stackoverflow.com/questions/9439256/
#' @noRd
utils::globalVariables(c("MAP35_140bp"))

#' Check if the current chunk includes a header row describing the fields
#' @noRd
.fr5_has_header <- function(raw_chunk){
  any(stringr::str_detect(raw_chunk, "^CHROM\\tPOS\\tREF\\tALT")) #nolint
}

#' add column_name_unparsed column to tibble prior to parsing (for debugging,
#' mostly)
#' @importFrom magrittr "%>%"
#' @noRd
.preserve_raw <- function(selected_columns, to_parse) {
  if (length(to_parse) == 0) {
    return(selected_columns)
  }
  selected_columns <- selected_columns %>%
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
    dplyr::mutate_at(.vars = dplyr::vars(to_clean),
                     .funs = dplyr::funs(
                       stringr::str_replace_all(., "\\{[^\\}]+\\}$", "")
                       )
                     ) %>%
    # .{n}; -> .;
    dplyr::mutate_at(.vars = dplyr::vars(to_clean),
                     .funs = dplyr::funs(
                       stringr::str_replace_all(., "\\{[^\\}]+\\};", ";")
                       )
                     ) %>%
    # .{n}. -> .,. (or should it be .{n}. -> . ? or .{n}. -> .;. ?)
    dplyr::mutate_at(.vars = dplyr::vars(to_clean),
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
          .vars = dplyr::vars(max_columns),
          .funs = dplyr::funs(
            stringr::str_replace_all(., "(?:\\{.*?\\})|;", " "))
        ) %>%
        # trim white space padding to be safe
        dplyr::mutate_at(
          .vars = dplyr::vars(max_columns),
          .funs = dplyr::funs(stringr::str_trim(., side = "both"))
        ) %>%
        # also trim multiple spaces to be safe
        dplyr::mutate_at(
          .vars = dplyr::vars(max_columns),
          .funs = dplyr::funs(stringr::str_replace(., "\\s{2,}", " ")) #nolint
        ) %>%
        # split the string at the space
        dplyr::mutate_at(
          .vars = dplyr::vars(max_columns),
          .funs = dplyr::funs(stringr::str_split(., "\\s+"))
        ) %>%
        # make values numeric
        dplyr::mutate_at(
          .vars = dplyr::vars(max_columns),
          .funs = dplyr::funs(purrr::map(., as.numeric))
        ) %>%
        # get the max values
        dplyr::mutate_at(
          .vars = dplyr::vars(max_columns),
          .funs = dplyr::funs(purrr::map_dbl(., max, na.rm = TRUE))
        ) %>%
        # change to character
        dplyr::mutate_at(
          .vars = dplyr::vars(max_columns),
          .funs = dplyr::funs(as.character)
        ) %>%
        # change "-Inf" to "."
        dplyr::mutate_at(
          .vars = dplyr::vars(max_columns),
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
          .vars = dplyr::vars(min_columns),
          .funs = dplyr::funs(
            stringr::str_replace_all(., "(?:\\{.*?\\})|;", " "))
        ) %>%
        # trim white space padding to be safe
        dplyr::mutate_at(
          .vars = dplyr::vars(min_columns),
          .funs = dplyr::funs(stringr::str_trim(., side = "both"))
        ) %>%
        # also trim multiple spaces to be safe
        dplyr::mutate_at(
          .vars = dplyr::vars(min_columns),
          .funs = dplyr::funs(stringr::str_replace(., "\\s{2,}", " ")) #nolint
        ) %>%
        # split the string at the space
        dplyr::mutate_at(
          .vars = dplyr::vars(min_columns),
          .funs = dplyr::funs(stringr::str_split(., "\\s+"))
        ) %>%
        # make values numeric
        dplyr::mutate_at(
          .vars = dplyr::vars(min_columns),
          .funs = dplyr::funs(purrr::map(., as.numeric))
        ) %>%
        # get the min values
        dplyr::mutate_at(
          .vars = dplyr::vars(min_columns),
          .funs = dplyr::funs(purrr::map_dbl(., min, na.rm = TRUE))
        ) %>%
        # change to character
        dplyr::mutate_at(
          .vars = dplyr::vars(min_columns),
          .funs = dplyr::funs(as.character)
        ) %>%
        # change "Inf" to "."
        dplyr::mutate_at(
          .vars = dplyr::vars(min_columns),
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
        dplyr::mutate_at(.vars = dplyr::vars(yes_columns),
                         .funs = dplyr::funs(ifelse(
                           stringr::str_detect(., "Y"),
                           "Y",
                           ifelse(stringr::str_detect(., "N"),
                                  "N", ".")
                         )))
    )
  return(selected_columns)
}

# TODO: is there a way to short-circuit parsing on trivial case?
#' @importFrom magrittr "%>%"
#' @noRd
.parse_no_columns <- function(selected_columns, no_columns){
  if (length(no_columns) == 0){
    return(selected_columns)
  }
  selected_columns <-
    suppressWarnings(
      selected_columns %>%
        # parse:  N if N present, else Y if Y present, else .
        dplyr::mutate_at(.vars = dplyr::vars(no_columns),
                         .funs = dplyr::funs(ifelse(
                           stringr::str_detect(., "N"),
                           "N",
                           ifelse(stringr::str_detect(., "Y"),
                                  "Y", ".")
                         )))
    )
  return(selected_columns)
}

# TODO: is there a way to short-circuit parsing on trivial case?
#' @importFrom magrittr "%>%"
#' @noRd
.parse_a_columns <- function(selected_columns, a_columns){
  if (length(a_columns) == 0){
    return(selected_columns)
  }
  selected_columns <-
    suppressWarnings(
      selected_columns %>%
        # parse: A if A present, then D, P, N, else .
        dplyr::mutate_at(.vars = dplyr::vars(a_columns),
                  .funs = dplyr::funs(
                    ifelse(stringr::str_detect(., "A"), "A",
                           ifelse(stringr::str_detect(., "D"), "D",
                                  ifelse(stringr::str_detect(., "P"), "P",
                                         ifelse(stringr::str_detect(., "N"),
                                                "N", ".")
                                  )
                           )
                    )
                  )
        )
    )
  return(selected_columns)
}

# helper for .parseDistinct() - returns |-separated unique values from
# character vector
#' @importFrom magrittr "%>%"
#' @noRd
.collapse_unique <- function(x) {
  unique(x) %>%
    stringr::str_c(collapse = "|")
}

# helper for .parseDistinct()
# takes complicated string and simplifies to a ;-separated string, then
# calls .collapse_unique() to return a string of |-separated unique values

#' @importFrom magrittr "%>%"
#' @noRd
.unique_values <- function(a_string) {
  a_string %>%
    # replace {n} with ;
    stringr::str_replace_all("\\{.*?\\}", ";") %>%
    # trim any padding spaces to be safe
    stringr::str_trim(side = "both") %>%
    # replace ".;" at the end of the line with "."
    stringr::str_replace_all("\\.;$", ".") %>%
    # remove ".;" within the string
    stringr::str_replace_all("\\.;", "") %>%
    # remove ";;"
    stringr::str_replace_all(";;", ";") %>%
    # remove ";" if it's the beginning or end of the string
    stringr::str_replace_all("^;|;$", "") %>%
    # split the string at the semicolon (makes list of character vectors)
    stringr::str_split(";") %>%
    # collapse_unique returns |-separated unique values from character vector
    purrr::map_chr(.collapse_unique)
}

# distinct_example:
# Before
# chr pos Ensembl_Regulatory_Build_TFBS
# 1  100  .{1}Tr4;Egr1;Egr1{4}
# 1  200  .{4}Egr1{3}Gabp{5}Gabp;Egr1{1}Gabp;Gabp{7}Gabp;Gabp;Egr1{4}
#
# After
# chr pos Ensembl_Regulatory_Build_TFBS
# 1  100  Tr4
# 1  100  Egr1
# 1  200  Egr1
# 1  200  Gabp

#' @importFrom magrittr "%>%"
#' @noRd
.parse_distinct <- function(selected_columns, distinct_columns){
  # trivial case
  if (length(distinct_columns) == 0){
    return(selected_columns)
  }

  # if no {*} or ;, no parsing needed.
  if (!any(
    selected_columns %>%
    dplyr::select(distinct_columns) %>%
    stringr::str_detect("(?:\\{.*?\\})|;"))
  ){
    return(selected_columns)
  }

  # now parse column using new functions
  selected_columns <-
    selected_columns %>%
    purrr::map_at(distinct_columns, .unique_values) %>% # may need vars()?
    dplyr::as_tibble()

  return(selected_columns)
  # NOTE: selected_columns still needs to be pivoted on the distinct field(s)
  # after this function call
}

#' @importFrom magrittr "%>%"
#' @importFrom rlang ":="
#' @noRd
.parse_pairs_max <- function(selected_columns, pair_columns) {
  if (typeof(pair_columns) != "list") {
    stop("pair_columns must be a list")
  }
  # perhaps map()?
  parsed_columns <- selected_columns
  for (pair in pair_columns) {
    # if a single, pass it along.
    if (length(pair) == 1){
      parsed_columns <- .preserve_raw(parsed_columns, unlist(pair))
      parsed_columns <- .parse_max_columns(parsed_columns, unlist(pair))
      next
    }
    if (length(pair) != 2){
      stop("pair columns not length 1 or 2")
    }
    # if we've really got a pair, parse them.
    current_pair <- rlang::syms(pair)
    score_name <- pair[[1]]
    pred_name <- pair[[2]]
    unparsed_score_name <- paste0(score_name, "_unparsed")
    unparsed_pred_name <- paste0(pred_name, "_unparsed")

    parsed_columns <-
      suppressWarnings(
        parsed_columns %>%
          dplyr::mutate(
            p_list = stringr::str_split(rlang::UQ(current_pair[[1]]), ";"),
            p_list = purrr::map(p_list, as.numeric),
            p_max = purrr::map_dbl(p_list, max, na.rm = TRUE),
            p_max = as.character(p_max),
            p_max = ifelse( (p_max == "-Inf"), ".", p_max),
            match_mask = purrr::map2(p_list, p_max, stringr::str_detect),
            # replace NA with false
            match_mask = purrr::map(match_mask,
                                    function(x)
                                      replace(x, is.na(x), FALSE)),
            # if all FALSE, change all to TRUE, then keep only first
            match_mask = purrr::map(match_mask,
                                    function(x)
                                      if (all(x == FALSE))
                                        ! x
                                    else
                                      x),
            # if match_mask has more than one TRUE, keep only first TRUE
            # -- thanks Adrienne!
            match_mask = purrr::map(match_mask,
                                    function(x)
                                      x & !duplicated(x)),
            r_list =  stringr::str_split(rlang::UQ(current_pair[[2]]), ";"),
            r_corresponding = purrr::map2_chr(match_mask, r_list,
                                              function(logical, string)
                                                ifelse(length(string) == 1,
                                                       string,
                                                       subset(string, logical)))
          ) %>%
          dplyr::select(-p_list,
                        -match_mask,
                        -r_list) %>%
          dplyr::rename(
            rlang::UQ(unparsed_score_name) := rlang::UQ(current_pair[[1]]),
            rlang::UQ(current_pair[[1]]) := p_max,
            rlang::UQ(unparsed_pred_name) := rlang::UQ(current_pair[[2]]),
            rlang::UQ(current_pair[[2]]) := r_corresponding
          )
      )
  }
  return(parsed_columns)
}

#' @importFrom magrittr "%>%"
#' @noRd
.parse_pairs_min <- function(selected_columns, pair_columns) {
  if (typeof(pair_columns) != "list") {
    stop("pair_columns must be a list")
  }
  # perhaps map()?
  parsed_columns <- selected_columns
  for (pair in pair_columns) {
    # if a single, pass it along.
    if (length(pair) == 1){
      parsed_columns <- .preserve_raw(parsed_columns, unlist(pair))
      parsed_columns <- .parse_min_columns(parsed_columns, unlist(pair))
      next
    }
    if (length(pair) != 2){
      stop("pair columns not length 1 or 2")
    }
    # if we've really got a pair, parse them.
    current_pair <- rlang::syms(pair)
    score_name <- pair[[1]]
    pred_name <- pair[[2]]
    unparsed_score_name <- paste0(score_name, "_unparsed")
    unparsed_pred_name <- paste0(pred_name, "_unparsed")

    parsed_columns <-
      suppressWarnings(
        parsed_columns %>%
          dplyr::mutate(
            p_list = stringr::str_split(rlang::UQ(current_pair[[1]]), ";"),
            p_list = purrr::map(p_list, as.numeric),
            p_min = purrr::map_dbl(p_list, min, na.rm = TRUE),
            p_min = as.character(p_min),
            p_min = ifelse( (p_min == "Inf"), ".", p_min),
            match_mask = purrr::map2(p_list, p_min, stringr::str_detect),
            # replace NA with false
            match_mask = purrr::map(match_mask,
                                    function(x)
                                      replace(x, is.na(x), FALSE)),
            # if all FALSE, change all to TRUE, then keep only first
            match_mask = purrr::map(match_mask,
                                    function(x)
                                      if (all(x == FALSE))
                                        ! x
                                    else
                                      x),
            # if match_mask has more than one TRUE, keep only first TRUE
            # -- thanks Adrienne!
            match_mask = purrr::map(match_mask,
                                    function(x)
                                      x & !duplicated(x)),
            r_list =  stringr::str_split(rlang::UQ(current_pair[[2]]), ";"),
            r_corresponding = purrr::map2_chr(match_mask, r_list,
                                              function(logical, string)
                                                ifelse(length(string) == 1,
                                                       string,
                                                       subset(string, logical)))
          ) %>%
          dplyr::select(-p_list,
                        -match_mask,
                        -r_list) %>%
          dplyr::rename(
            rlang::UQ(unparsed_score_name) := rlang::UQ(current_pair[[1]]),
            rlang::UQ(current_pair[[1]]) := p_min,
            rlang::UQ(unparsed_pred_name) := rlang::UQ(current_pair[[2]]),
            rlang::UQ(current_pair[[2]]) := r_corresponding
          )
      )
  }
  return(parsed_columns)
}

#' @importFrom magrittr "%>%"
#' @noRd
.parse_pairs_pick_y <- function(selected_columns, pair_columns) {
  if (typeof(pair_columns) != "list") {
    stop("pair_columns must be a list")
  }
  # perhaps map()?
  parsed_columns <- selected_columns
  for (pair in pair_columns) {
    if (length(pair) == 1){
      parsed_columns <- .preserve_raw(parsed_columns, unlist(pair))
      parsed_columns <- .parse_yes_columns(parsed_columns, unlist(pair))
      next
    }
    if (length(pair) != 2){
      stop("pair columns not length 1 or 2")
    }
    stop(".parse_pairs_pick_y() not implemented yet")
  }
  return(parsed_columns)
}

#' @importFrom magrittr "%>%"
#' @noRd
.parse_pairs_pick_n <- function(selected_columns, pair_columns) {
  if (typeof(pair_columns) != "list") {
    stop("pair_columns must be a list")
  }
  # perhaps map()?
  parsed_columns <- selected_columns
  for (pair in pair_columns) {
    if (length(pair) == 1){
      parsed_columns <- .preserve_raw(parsed_columns, unlist(pair))
      parsed_columns <- .parse_no_columns(parsed_columns, unlist(pair))
      next
    }
    if (length(pair) != 2){
      stop("pair columns not length 1 or 2")
    }
    stop(".parse_pairs_pick_n() not implemented yet")
  }
  return(parsed_columns)
}

#' @importFrom magrittr "%>%"
#' @importFrom rlang ":="
#' @noRd
#'
.parse_pairs_a <- function(selected_columns, pair_columns) {
  if (typeof(pair_columns) != "list") {
    stop("pair_columns must be a list")
  }
  # perhaps map()?
  parsed_columns <- selected_columns
  for (pair in pair_columns) {
    if (length(pair) == 1){
      parsed_columns <- .preserve_raw(parsed_columns, unlist(pair))
      parsed_columns <- .parse_a_columns(parsed_columns, unlist(pair))
      next
    }
    if (length(pair) != 2){
      stop("pair columns not length 1 or 2")
    }

    # if we've really got a pair, parse them.
    current_pair <- rlang::syms(pair)
    score_name <- pair[[2]]
    pred_name <- pair[[1]]
    unparsed_score_name <- paste0(score_name, "_unparsed")
    unparsed_pred_name <- paste0(pred_name, "_unparsed")

    parsed_columns <-
      suppressWarnings(
        parsed_columns %>%
          dplyr::mutate(
            # If A present keep A,
            # else if D present keep D,
            # else if P present keep P,
            # else if N present keep N,
            # else .
            new_p = ifelse(
              stringr::str_detect(rlang::UQ(current_pair[[1]]), "A"),
              "A",
              ifelse(
                stringr::str_detect(rlang::UQ(current_pair[[1]]), "D"),
                "D",
                ifelse(
                  stringr::str_detect(rlang::UQ(current_pair[[1]]), "P"),
                  "P",
                  ifelse(stringr::str_detect(rlang::UQ(current_pair[[1]]), "N"),
                         "N",
                         ".")
                )
              )
            ),
            p_list = stringr::str_split(rlang::UQ(current_pair[[1]]), ";"),
            match_mask = purrr::map2(p_list, new_p, stringr::str_detect),
            # if match_mask has more than one TRUE, keep only first TRUE
            # -- thanks Adrienne!
            match_mask = purrr::map(match_mask,
                                    function(x)
                                      x & !duplicated(x)),
            r_list =  stringr::str_split(rlang::UQ(current_pair[[2]]), ";"),
            r_corresponding = purrr::map2_chr(match_mask, r_list,
                                       function(logical, string)
                                         ifelse(length(string) == 1,
                                                string,
                                                subset(string, logical)))
          ) %>%
          dplyr::select(-p_list,
                        -match_mask,
                        -r_list) %>%
          dplyr::rename(
            rlang::UQ(unparsed_score_name) := rlang::UQ(current_pair[[2]]),
            rlang::UQ(current_pair[[2]]) := r_corresponding,
            rlang::UQ(unparsed_pred_name) := rlang::UQ(current_pair[[1]]),
            rlang::UQ(current_pair[[1]]) := new_p
          )
      )
  }
  return(parsed_columns)
}

#' @importFrom magrittr "%>%"
#' @noRd
.pivot_fields <- function(selected_columns, pivot_columns) {
  if (typeof(pivot_columns) != "list") {
    stop("pivot_columns must be a list")
  }
  # perhaps map()?
  pivoted_columns <- selected_columns
  for (pivot_set in pivot_columns) {
    regexp <- paste0("\\", pivot_set$pivotChar[[1]]) #nolint
    pivoted_columns <- pivoted_columns %>%
      tidyr::separate_rows(dplyr::one_of(pivot_set$field), sep = regexp)
  }
  pivoted_columns <- dplyr::distinct(pivoted_columns)
  return(pivoted_columns)
}

#' chunk = with colnames, as from wgsaparsr:::.get_fields_from_chunk()
#' config = tibble as from load_config()
#' type = "SNV"|"indel"
#' @importFrom magrittr "%>%"
#' @noRd
.parse_then_pivot <- function(chunk, config, type) {
  # check args---------------------------
  if (!(type %in% c("SNV", "indel"))) {
    stop('type must be one of "SNV" or "indel"')
  }

  validate_config(config)

  # get desired fields from config to validate
  desired <- .get_list_from_config(config, "desired", type)

  # validate the config against chunk
  if (!all(unlist(desired) %in% names(chunk))) {
    stop("not all desired fields are in sourcefile")
  }

  # build lists from config file---------
  # fields that are transformed by themselves:
  parse_max <- .get_list_from_config(config, "max", type)
  parse_min <- .get_list_from_config(config, "min", type)
  pick_y <- .get_list_from_config(config, "pick_Y", type)
  pick_n <- .get_list_from_config(config, "pick_N", type)
  pick_a <- .get_list_from_config(config, "pick_A", type)
  parse_clean <- .get_list_from_config(config, "clean", type)
  parse_distinct <- .get_list_from_config(config, "distinct", type)

  # fields that are transformed as pairs:
  parse_pairs_max <- .get_list_from_config(config, "max_pairs", type)
  parse_pairs_min <- .get_list_from_config(config, "min_pairs", type)
  parse_pairs_pick_y <- .get_list_from_config(config, "pick_Y_pairs", type)
  parse_pairs_pick_n <- .get_list_from_config(config, "pick_N_pairs", type)
  parse_pairs_pick_a <- .get_list_from_config(config, "pick_A_pairs", type)

  # pivoting
  to_pivot <- .get_list_from_config(config, "pivots", type)

  # select the variables from chunk----------
  selected <- chunk %>% dplyr::select(unlist(desired))

  # parse the chunk single fields-------------
  # preserve unparsed, first (maybe with flag?)
  parsed <- .preserve_raw(selected, unlist(parse_max))
  parsed <- .parse_max_columns(parsed, unlist(parse_max))

  parsed <- .preserve_raw(parsed, unlist(parse_min))
  parsed <- .parse_min_columns(parsed, unlist(parse_min))

  parsed <- .preserve_raw(parsed, unlist(pick_y))
  parsed <- .parse_yes_columns(parsed, unlist(pick_y))

  parsed <- .preserve_raw(parsed, unlist(pick_n))
  parsed <- .parse_no_columns(parsed, unlist(pick_n))

  parsed <- .preserve_raw(parsed, unlist(pick_a))
  parsed <- .parse_a_columns(parsed, unlist(pick_a))

  parsed <- .preserve_raw(parsed, unlist(parse_clean))
  parsed <- .parse_clean(parsed, unlist(parse_clean))

  parsed <- .preserve_raw(parsed, unlist(parse_distinct))
  parsed <- .parse_distinct(parsed, unlist(parse_distinct))

  # parse the chunk pair fields-------------
  parsed <- .parse_pairs_max(parsed, parse_pairs_max)
  parsed <- .parse_pairs_min(parsed, parse_pairs_min)
  parsed <- .parse_pairs_pick_y(parsed, parse_pairs_pick_y)
  parsed <- .parse_pairs_pick_n(parsed, parse_pairs_pick_n)
  parsed <- .parse_pairs_a(parsed, parse_pairs_pick_a)

  # pivot chunk on pivot fields--------------
  pivoted <- .pivot_fields(parsed, to_pivot)

  return(pivoted)
}

#' chunk = with colnames, as from wgsaparsr:::.get_fields_from_chunk()
#' config = tibble as from load_config()
#' type = "dbnsfp"
#' @importFrom magrittr "%>%"
#' @noRd
.pivot_then_parse <- function(chunk, config, type = "dbnsfp") {
  # check args---------------------------
  if (!(type %in% c("dbnsfp"))) {
    stop('type must be "dbnsfp"')
  }

  validate_config(config)

  # get desired fields from config to validate
  desired <- .get_list_from_config(config, "desired", type)

  # validate the config against chunk-----
  if (!all(unlist(desired) %in% names(chunk))) {
    stop("not all desired fields are in sourcefile")
  }

  # build lists from config file---------
  # fields that are transformed by themselves:
  parse_max <- .get_list_from_config(config, "max", type)
  parse_min <- .get_list_from_config(config, "min", type)
  pick_y <- .get_list_from_config(config, "pick_Y", type)
  pick_n <- .get_list_from_config(config, "pick_N", type)
  pick_a <- .get_list_from_config(config, "pick_A", type)
  parse_clean <- .get_list_from_config(config, "clean", type)
  parse_distinct <- .get_list_from_config(config, "distinct", type)

  # fields that are transformed as pairs:
  parse_pairs_max <- .get_list_from_config(config, "max_pairs", type)
  parse_pairs_min <- .get_list_from_config(config, "min_pairs", type)
  parse_pairs_pick_y <- .get_list_from_config(config, "pick_Y_pairs", type)
  parse_pairs_pick_n <- .get_list_from_config(config, "pick_N_pairs", type)
  parse_pairs_pick_a <- .get_list_from_config(config, "pick_A_pairs", type)

  # pivoting
  to_pivot <- .get_list_from_config(config, "pivots", type)

  # select the variables from chunk----------
  selected <- chunk %>% dplyr::select(unlist(desired))

  # pivot chunk on pivot fields--------------
  pivoted <- .pivot_fields(selected, to_pivot)

  # parse the chunk single fields-------------
  # preserve unparsed, first (maybe with flag?)
  parsed <- .preserve_raw(pivoted, unlist(parse_max))
  parsed <- .parse_max_columns(parsed, unlist(parse_max))

  parsed <- .preserve_raw(parsed, unlist(parse_min))
  parsed <- .parse_min_columns(parsed, unlist(parse_min))

  parsed <- .preserve_raw(parsed, unlist(pick_y))
  parsed <- .parse_yes_columns(parsed, unlist(pick_y))

  parsed <- .preserve_raw(parsed, unlist(pick_n))
  parsed <- .parse_no_columns(parsed, unlist(pick_n))

  parsed <- .preserve_raw(parsed, unlist(pick_a))
  parsed <- .parse_a_columns(parsed, unlist(pick_a))

  parsed <- .preserve_raw(parsed, unlist(parse_clean))
  parsed <- .parse_clean(parsed, unlist(parse_clean))

  parsed <- .preserve_raw(parsed, unlist(parse_distinct))
  parsed <- .parse_distinct(parsed, unlist(parse_distinct))

  # parse the chunk pair fields-------------
  parsed <- .parse_pairs_max(parsed, parse_pairs_max)
  parsed <- .parse_pairs_min(parsed, parse_pairs_min)
  parsed <- .parse_pairs_pick_y(parsed, parse_pairs_pick_y)
  parsed <- .parse_pairs_pick_n(parsed, parse_pairs_pick_n)
  parsed <- .parse_pairs_a(parsed, parse_pairs_pick_a)

  return(parsed)
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
