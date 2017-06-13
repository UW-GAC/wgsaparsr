#' Get column names from the WGSA output file
#' 
#' \href{https://sites.google.com/site/jpopgen/wgsa}{WGSA} output files can 
#' contain thousands of fields, including fields with lists of entries. This 
#' function reads the header of a WGSA file and returns a list of fields in the file.
#' 
#' @param source Path to the WGSA output file to parse
#' @return a character vector with the names of the WGSA fields
#'
#' @examples 
#' \dontrun{
#' all_fields <- get_fields(soure = "WGSA_chr_1.gz")
#' }
#' @importFrom stringr str_split
#' @export

get_fields <- function(source) {
  readfile_con <- gzfile(source, "r")
  header <- suppressWarnings(readLines(readfile_con, n = 1))
  close(readfile_con)
  fields <- str_split(header, "\t")[[1]]
  if (str_split(header, "\t")[[1]][[1]] != "#chr") {
    stop("First line of source doesn't look like a WGSA header")
  }
  return(fields)
}
