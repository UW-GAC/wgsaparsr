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

  # create variable in function execution environment
  res <- dplyr::tibble()

  # Define the callback function for read_tsv_chunked():
  # this function will return FALSE after reading the specified chunk, but does
  # some environment trickery to assign the specified chunk to the res variable
  # in its calling environment (or it will create res in the global environment
  # if it doesn't exist. In this case, the get_problem_chunk function
  # execution environment includes res, so that is where the chunk goes). This
  # way, get_problem_chunk() can returns res to its calling environment, rather
  # than creating res in the global environment.
  get_chunk <- function(num) {
    i <- 1
    function(x, pos) {
      if (i == num) {
        res <<- x
        return(FALSE)
      }
      i <<- i + 1
    }
  }

  # call read_tsv_chunked() with a SideEffectChunkCallback call to get_chunk()
  readr::read_tsv_chunked(source_file,
                          get_chunk(problem_index),
                          chunk_size = chunk_size,
                          col_types =
                            readr::cols(.default = readr::col_character()))

  # return res to the calling environment
  return(res)
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
