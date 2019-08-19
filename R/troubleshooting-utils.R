# functions to help with bug hunting and troubleshooting------------------------

#' Get problem chunks from WGSA output file
#'
#' @param source_file Path to the WGSA output file to parse (indel or SNV
#'   annotation)
#' @param chunk_size Number of lines to parse each iteration (default 10,000)
#' @param problem_index the index of desired chunk to get. Likely one more than
#'   the last output from a call to wgsaparsr::parse_to_file()
#'
#' @examples
#' \dontrun{
#'
#' problem_chunk <-
#'   get_problem_chunk(source_file = "./path/to/source.gz",
#'                     chunk_size = 1000,
#'                     problem_index = 7)
#' }
#'
#' @export
get_problem_chunk <- function(source_file,
                              chunk_size,
                              problem_index){
  # get header and check if indel file----------------------------------------
  first_line <- .get_first_line(source_file)

  if (!.has_header(first_line)) {
    stop("source_file doesn't have header line")
  }

  raw_header <- first_line

  # main loop - read file by chunk-----------------------------------------
  readfile_con <- gzfile(source_file, "r")
  index <- 0L
  while (TRUE) {
    # read chunk
    raw_chunk <- suppressWarnings(readLines(readfile_con, n = chunk_size))

    # readLines() returns a zero length result at EOF (which SHOULD end loop)
    if (length(raw_chunk) == 0) {
      break
    }

    if (index < problem_index){
      message(paste0("Chunk skipped: ", index))
      index <- index + 1L
      next
    }

    if (index > problem_index){
      message("Index greater than problem; breaking.")
      break
    }

    # if header line in this chunk, read raw chunk to all_fields tibble
    header_flag <- .has_header(raw_chunk)

    if (header_flag) {
      all_fields <- .get_fields_from_chunk(raw_chunk)
    } else {
      modified_chunk <- c(raw_header, raw_chunk) # add the raw header
      all_fields <- .get_fields_from_chunk(modified_chunk)

      # end current iteration if all_fields has 0 observations
      # (to avoid dplyr error arising from empty tibble)
      if (nrow(all_fields) == 0) {
        index <- index + 1L
        next
      }
#      break
    }
    close(readfile_con)
    return(all_fields)
  }
}

#' Select the variables of interest from a problem chunk
#'
#' @param problem_chunk A problem chunk for troubleshooting. Usually from
#'     get_problem_chunk()
#' @param config Path to config file or a dataframe that passes
#'   validate_config()
#' @param type the subset of interest: "SNV", "indel", or "dbnsfp"
#'
#' @examples
#' \dontrun{
#'
#' snv_selected <- subset_problem_chunk(problem_chunk, config, "SNV")
#' }
#'
#' @export
subset_problem_chunk <- function(problem_chunk, config, type){
  if ("data.frame" %in% class(config)) {
    validate_config(config)
  } else {
    config <- load_config(config)
  }

  # get desired fields from config to validate
  desired <- .get_list_from_config(config, "desired", type)

  # validate the config against chunk
  if (!all(unlist(desired) %in% names(problem_chunk))) {
    stop("not all desired fields are in sourcefile")
  }

  # select the variables from chunk----------
  selected <- problem_chunk %>% dplyr::select(unlist(desired))
  return(selected)
}
