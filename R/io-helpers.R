# helpers related to file io

#' Check if the current chunk includes a header row describing the fields
#' @noRd
.get_first_line <- function(source_file){
  readfile_con <- gzfile(source_file, "r")
  first_line <- suppressWarnings(readLines(readfile_con, n = 1))
  close(readfile_con)
  return(first_line)
}

#' get header line from source file or header file
#' @noRd
.get_header <- function(source_file, header_file){
  first_line <- .get_first_line(source_file)
  source_header_flag <- .has_header(first_line)
  if (is.na(header_file) & !source_header_flag) {
    stop("no header in source_file or header_file")
  }
  if (!is.na(header_file) & source_header_flag) {
    header_line <- .get_first_line(header_file)
    if (header_line != first_line) {
      stop("headers in header_file and source_file don't match")
    } else {
      return(header_line)
    }
  }
  if (is.na(header_file) & source_header_flag) {
    raw_header <- first_line
  }
  if (!is.na(header_file) & !source_header_flag) {
    raw_header <- .get_first_line(header_file)
  }
  return(raw_header)
}

#' initialize output files by writing header
#' @noRd
.write_output_header <-
  function(config, destination, dbnsfp_destination, indel_flag){
    # check inputs
    if (is.na(destination) & is.na(dbnsfp_destination)){
      stop("must provide outupt destination")
    }

    if (indel_flag){
      type <- "indel"
    } else {
      type <- "SNV"
    }

    # get fields for SNV or indel
    desired_fields <- .get_list_from_config(config, "desired", type)

    # freeze 5 has some different field names between indel and SNV. Fix that.
    desired_fields <-
      stringr::str_replace(desired_fields, "MAP35_149bp", "MAP35_149")
    desired_fields <-
      stringr::str_replace(desired_fields, "VEP_refseq_ProteinID(ENSP)",
                           "VEP_refseq_ProteinID")
    # str_replace returns a character vector. Make it back into a list.
    desired_fields <- as.list(desired_fields)

    # get fields for dbnsfp
    if (!is.na(dbnsfp_destination) & type == "SNV") {
      dbnsfp_fields <- .get_list_from_config(config, "desired", "dbnsfp")
    }

    # initialize snv or indel file
    readr::write_tsv(as.data.frame(desired_fields), destination, append = TRUE)

    # initialse dbnsfp file if present
    if (exists("dbnsfp_fields")){
      readr::write_tsv(
        as.data.frame(dbnsfp_fields), dbnsfp_destination, append = TRUE)
    }
    invisible(TRUE)
  }

#' select columns to set order and write_tsv
#' @importFrom magrittr "%>%"
#' @noRd
.write_to_file <- function(parsed_lines,
                           destination,
                           processed_fields) {
  parsed_lines %>%
      dplyr::select(dplyr::one_of(processed_fields)) %>%
      readr::write_tsv(path = destination, append = TRUE)
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
