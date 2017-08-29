#' Parse the WGSA output file to tidy and select columns of interest
#' 
#' \href{https://sites.google.com/site/jpopgen/wgsa}{WGSA} output files can 
#' contain thousands of fields, including fields with lists of entries. This 
#' function reads a WGSA field in chunks, parses the chunks to select desired 
#' fields and unnests specified list fields, and writes to an output file.
#' 
#' List fields are tidied by separating so that each entry in the field is its 
#' own row in the output file (other fields are duplicated as necessary)
#' 
#' @param source_file Path to the WGSA output file to parse
#' @param destination Path to the desired output file
#' @param desired_columns a character vector with the names of fields to extract
#'   from the WGSA output (names must match names in WGSA output file).
#' @param to_split a character vector with the names of list-fields to be tidied
#' @param WGSA_version The version of WGSA used to generate output
#' @param chunk_size Number of lines to parse each iteration (default 10,000)
#' @param verbose more output to screen (default FALSE)
#'
#' @examples 
#' \dontrun{
#'  target_columns <- c("#chr", 
#'    "pos", 
#'    "ref", 
#'    "alt",
#'    "VEP_ensembl_Transcript_ID", 
#'    "VEP_ensembl_Gene_ID")
#'    
#'  columns_to_split <- c("VEP_ensembl_Transcript_ID", 
#'    "VEP_ensembl_Gene_ID")
#'    
#' parse_to_file(source_file = "WGSA_chr_1.gz", 
#'  destination = "parsed_chr_1.csv", 
#'  desired_columns = target_columns, 
#'  to_split = columns_to_split, 
#'  chunk_size = 1000)
#' }
#' 
#' @export

parse_to_file <- function(source_file,
                          destination,
                          desired_columns,
                          to_split,
                          WGSA_version = "WGSA065",
                          chunk_size = 10000,
                          verbose = TRUE) {

  # check that desired_columns and to_split are possible
  .check_to_split(desired_columns, to_split)
  .check_desired(source_file, desired_columns)

  # main loop - read file by chunk, process chunk, write chunk
  readfile_con <- gzfile(source_file, "r")
  index <- 0L
  while (TRUE) {
    # read a raw chunk
    raw_chunk <- suppressWarnings(readLines(readfile_con, n = chunk_size))

    # check for header line and read raw chunk to all_fields tibble
    if (.has_header(raw_chunk)) {
      found_header <- TRUE
      header_flag <- TRUE
      raw_header <- .get_header(raw_chunk)
      indel_flag <- .is_indel(raw_header)
      all_fields <- .get_fields_from_chunk(raw_chunk)
    } else {
      # header line should have been read in a previous chunk
      if (!found_header){
        stop("Didn't find header line in source_file!")
      }
      header_flag <- FALSE
      modified_chunk <- paste0(raw_header, "\n",
                               raw_chunk, collapse = "\n")
      all_fields <- .get_fields_from_chunk(modified_chunk)
    }

    # end iteration if all_fields has 0 observations
    # (to avoid dplyr error arising from empty tibble)
    if (dim(all_fields)[1] == 0) {
      break
    }

    # parse the all_fields tibble
    if (indel_flag) {
      parsed_lines <- .parse_indel_chunk(all_fields,
                                         desired_columns,
                                         to_split,
                                         WGSA_version)
    } else {
      parsed_lines <- .parse_snv_chunk(all_fields,
                                       desired_columns,
                                       to_split,
                                       WGSA_version)
    }

    # write tibble to tsv file
    .write_to_file(parsed_lines, destination, desired_columns, header_flag)

    # update progress if desired
    if (verbose) {
      msg <- paste0(
        "Chunks completed: ", index,
        "\n Sourcefile lines processed <= ", chunk_size * index,
        "\n Records in current import: ", dim(parsed_lines)[1]
      )
      message(msg)
    }

    # and on to the next chunk!
    index <- index + 1L
  }
  close(readfile_con)
}

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
#' @importFrom readr read_tsv
.get_fields_from_chunk <- function(raw_chunk) {
  read_tsv(paste0(raw_chunk, collapse = "\n"),
           col_types = cols(.default = col_character()))
}

#' Parse a chunk tibble from a SNV annotation file
#' @importFrom dplyr select mutate mutate_at rename distinct one_of
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
  if (names(expanded)[[1]] == "#chr") {
    expanded <- expanded %>%
      rename(
        chr = `#chr`,
        MAP20_149bp = `MAP20(+-149bp)`,
        MAP35_149bp = `MAP35(+-149bp)`,
        GMS_single_end = `GMS_single-end`,
        GMS_paired_end = `GMS_paired-end`,
        H1_hESC_fitCons_score = `H1-hESC_fitCons_score`, #nolint
        H1_hESC_fitCons_rankscore = `H1-hESC_fitCons_rankscore`, #nolint
        H1_hESC_confidence_value = `H1-hESC_confidence_value`, #nolint
        KGP_strict_masked = `1000G_strict_masked`,
        KGP3_AC = `1000Gp3_AC`,
        KGP3_AF = `1000Gp3_AF`,
        KGP3_AFR_AC = `1000Gp3_AFR_AC`,
        KGP3_AFR_AF = `1000Gp3_AFR_AF`,
        KGP3_EUR_AC = `1000Gp3_EUR_AC`,
        KGP3_EUR_AF = `1000Gp3_EUR_AF`,
        KGP3_AMR_AC = `1000Gp3_AMR_AC`,
        KGP3_AMR_AF = `1000Gp3_AMR_AF`,
        KGP3_EAS_AC = `1000Gp3_EAS_AC`,
        KGP3_EAS_AF = `1000Gp3_EAS_AF`,
        KGP3_SAS_AC = `1000Gp3_SAS_AC`,
        KGP3_SAS_AF = `1000Gp3_SAS_AF`,
        fathmm_MKL_non_coding_score = `fathmm-MKL_non-coding_score`,
        fathmm_MKL_non_coding_rankscore = `fathmm-MKL_non-coding_rankscore`, #nolint
        fathmm_MKL_non_coding_group = `fathmm-MKL_non-coding_group`,
        fathmm_MKL_coding_score = `fathmm-MKL_coding_score`,
        fathmm_MKL_coding_rankscore = `fathmm-MKL_coding_rankscore`,
        fathmm_MKL_coding_pred = `fathmm-MKL_coding_pred`,
        fathmm_MKL_coding_group = `fathmm-MKL_coding_group`,
        Eigen_raw = `Eigen-raw`,
        Eigen_phred = `Eigen-phred`,
        Eigen_raw_rankscore = `Eigen-raw_rankscore`,
        Eigen_PC_raw = `Eigen-PC-raw`,
        Eigen_PC_raw_rankscore = `Eigen-PC-raw_rankscore`
      )
  }

  expanded <- distinct(expanded)
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
