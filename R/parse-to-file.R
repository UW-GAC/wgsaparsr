#' Parse the WGSA output file to tidy and select columns of interest
#' 
#' \href{https://sites.google.com/site/jpopgen/wgsa}{WGSA} generates distinct 
#' annotation output files for annotating indel or SNVs. These output files
#' contain thousands of fields, including fields with lists of entries. 
#' parse_to_file() reads a TOPMed Freeze 4 WGSA file in chunks, parses the
#' chunks, writes to two output files - one for the snv or indel annotation,
#' and another for the dbnsfp annotation. These tab-separated output files can
#' then be imported to a database for aggregation, or used for obtaining
#' variant annotation.
#' 
#' @param source_file Path to the WGSA output file to parse (indel or SNV
#'   annotation)
#' @param destination Path to the desired indel or snv output file
#' @param dbnsfp_destination Path to the desired dbnsfp output file
#' @param freeze Which TOPMed freeze is being used (default 4)
#' @param chunk_size Number of lines to parse each iteration (default 10,000)
#' @param verbose more output to screen (default TRUE)
#'
#' @examples 
#' \dontrun{
#'    
#' parse_to_file(source_file = "WGSA_chr_1.gz", 
#'  destination = "parsed_chr_1_snv.tsv",
#'  dbnsfp_destination = "parsed_chr_1_dbnsfp.tsv",
#'  chunk_size = 1000)
#' }
#' 
#' @export

parse_to_file <- function(source_file,
                          destination,
                          dbnsfp_destination,
                          freeze = 4,
                          chunk_size = 10000,
                          verbose = TRUE) {


  if (freeze != 4){
    stop("This version of WGSAparsr only supports freeze 4.")
  }

  # set variables using first line of sourcefile--------------------------------
  readfile_con <- gzfile(source_file, "r")
  first_line <- suppressWarnings(readLines(readfile_con, n = 1))
  close(readfile_con)

  if (!.has_header(first_line)){
    stop("source_file doesn't have header line")
  }

  raw_header <- first_line
  indel_flag <- .is_indel(first_line)

  #todo: move this to parse-chunks.R functions
  if (freeze == 4) {
    WGSA_version <- "WGSA065"
    if (indel_flag) {
      desired_columns <- .get_list("fr_4_indel_desired")
      to_split <- .get_list("fr_4_indel_to_split")
    } else {
      desired_columns <- .get_list("fr_4_snv_desired")
      to_split <- .get_list("fr_4_snv_to_split")
    }
  }

  # main loop - read file by chunk, process chunk, write chunk------------------
  readfile_con <- gzfile(source_file, "r")
  index <- 0L
  while (TRUE) {
    # read chunk
    raw_chunk <- suppressWarnings(readLines(readfile_con, n = chunk_size))

    # readLines() returns a zero length result at EOF (which SHOULD end loop...)
    if (length(raw_chunk) == 0) {
      break
    }

    # check if header line in this chunk, read raw chunk to all_fields tibble
    if (.has_header(raw_chunk)) {
      header_flag <- TRUE # for .write_to_file()
      all_fields <- .get_fields_from_chunk(raw_chunk)
    } else {
      header_flag <- FALSE # for .write_to_file()
      modified_chunk <- c(raw_header, raw_chunk) # add the raw header
      all_fields <- .get_fields_from_chunk(modified_chunk)
    }

    # end current iteration if all_fields has 0 observations
    # (to avoid dplyr error arising from empty tibble)
    if (dim(all_fields)[1] == 0) {
      index <- index + 1L
      next
    }

    # parse the all_fields tibble for snv or indel annotation
    # TODO: clean up so call more like .parse_chunk_dbnsfp
    if (indel_flag) {
      parsed_lines <-
        .parse_chunk_indel(
          all_fields,
          desired_columns,
          to_split,
          WGSA_version)
    } else {
      parsed_lines <-
        .parse_chunk_snv(
          all_fields,
          desired_columns,
          to_split,
          WGSA_version)
    }

    # write snv or indel tibble to tsv file
    .write_to_file(parsed_lines,
                   destination,
                   desired_columns,
                   header_flag,
                   indel_flag)

    # parse the dbnsfp fields
    parsed_lines_dbnsfp <-
      .parse_chunk_dbnsfp(
        all_fields,
        freeze)

    # write dbnsfp chunk to tsv file
    # TODO

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
