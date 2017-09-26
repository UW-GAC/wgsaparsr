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
#'   from the WGSA output (names must match names in WGSA output file). For
#'   indel annotation, unparsed columns will be retained in addition to parsed 
#'   columns.
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
  if (!.check_to_split(desired_columns, to_split)) {
    stop("all to_split fields must be in desired_columns")
  }
  if (!.check_desired(source_file, desired_columns)) {
    stop("not all desired_columns are in source_file")
  }

  # main loop - read file by chunk, process chunk, write chunk
  readfile_con <- gzfile(source_file, "r")
  index <- 0L
  while (TRUE) {
    # read a raw chunk
    raw_chunk <- suppressWarnings(readLines(readfile_con, n = chunk_size))

    # readLines() returns a zero length result at EOF
    if (is.null(dim(raw_chunk))) {
      break
    }

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
      modified_chunk <- c(raw_header, raw_chunk)
      all_fields <- .get_fields_from_chunk(modified_chunk)
    }

    # end iteration if all_fields has 0 observations
    # (to avoid dplyr error arising from empty tibble)
    if (dim(all_fields)[1] == 0) {
      index <- index + 1L
      next
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
    .write_to_file(parsed_lines,
                   destination,
                   desired_columns,
                   header_flag,
                   indel_flag)

    # ready for the next chunk!
    index <- index + 1L

    # update progress if desired
    if (verbose) {
      msg <- paste0(
        "Chunks completed: ", index,
        "\n Sourcefile lines processed <= ", chunk_size * index,
        "\n Records in current import: ", dim(parsed_lines)[1]
      )
      message(msg)
    }
  }
  close(readfile_con)
}
