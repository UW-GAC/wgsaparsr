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
#'   from the WGSA output. Column names with unusual characters should be
#'   wrapped in backticks (e.g. `#chr`).
#' @param to_split list-fields to be tidied
#' @param chunk_size Number of lines to parse each iteration (default 10,000)
#'
#' @examples 
#' \dontrun{
#'  target_columns <- c("`#chr`", 
#'    "pos", 
#'    "ref", 
#'    "alt",
#'    "VEP_ensembl_Transcript_ID", 
#'    "VEP_ensembl_Gene_ID")
#'    
#'  columns_to_split <- c("VEP_ensembl_Transcript_ID", 
#'    "VEP_ensembl_Gene_ID")
#'    
#' parse_to_file(soure_file = "WGSA_chr_1.gz", 
#'  destination = "parsed_chr_1.csv", 
#'  desired_columns = target_columns, 
#'  to_split = columns_to_split, 
#'  chunk_size = 1000)
#' }
#' @import readr
#' @import dplyr
#' @importFrom tidyr separate_rows_
#' @importFrom tidyr unite
#' @importFrom digest digest
#' @importFrom purrr map_chr
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_sub
#' @export

parse_to_file <- function(source_file, destination
                          , desired_columns, to_split, chunk_size = 10000) {
  readfile_con <- gzfile(source_file, "r")

  # check that desired_columns are in source file
  all_fields <- get_fields(source_file)
  cleaned_desired <- str_replace_all(desired_columns, "`", "")
  if (!all(cleaned_desired %in% all_fields)){
    close(readfile_con)
    stop("Not all desired columns are in source file")
  }

  index <- 0L
  while (TRUE) {
    # read a chunk
    raw_chunk <- suppressWarnings(readLines(readfile_con, n = chunk_size))

    # check for header and grab if in chunk
    if (.has_header(raw_chunk)) {
      raw_header <- raw_chunk[str_sub(raw_chunk, 1, 4) == "#chr"]
      all_fields <-
        read_tsv(paste0(raw_chunk, collapse = "\n"),
                 #paste all lines to a single string
                 col_types = cols(.default = col_character()))
      header_flag <- TRUE
    } else {
      all_fields <-
        read_tsv(paste0(raw_header, "\n", # add header to chunk
                        paste0(raw_chunk, collapse = "\n")),
                 col_types = cols(.default = col_character()))
      header_flag <- FALSE
    }

    # end iteration if all_fields has 0 observations
    # (to avoid dplyr error arising from empty tibble)
    if (dim(all_fields)[1] == 0) {
      break
    }

    # pick out the desired columns for further operation
    selected_columns <- all_fields %>%
      select_(.dots = desired_columns) %>% # select fields of interest
      select(chr = `#chr`, everything()) %>% # rename #chr to chr
      mutate(wgsa_version = "WGSA065") %>% # add wgsa version
      distinct() # hopefully a minor pre-filter

    # parse the complex columns to unpack them
    expanded <- selected_columns %>%
      separate_rows_(to_split, sep = "\\|")

    # add a hash of each line as a unique key e.g.
    # digest(paste(data.frame(letters[1:10], letters[11:20])[1,], collapse =
    # ""), algo = "md5", serialize = FALSE)

    # first, combine columns by row for hashing
    lines <- expanded %>% unite(foo, everything()) #nolint

    # add hash of each string and save resulting tibble
    parsed_lines <-
      mutate(expanded, hash = map_chr(lines$foo, function(x)
        digest(
          x, algo = "md5", serialize = FALSE
        ))) %>%
      distinct() # hopefully a minor pre-filter

    # write tibble to tsv file
    if (header_flag) {
      write_tsv(parsed_lines, destination)
    } else {
      write_tsv(parsed_lines, destination, append = TRUE)
    }

    index <- index + 1L
    print(
      paste0(
        "Chunks: ",
        index,
        " Lines: <= ",
        chunk_size * index,
        " Records in current import: ",
        dim(parsed_lines)[1]
      )
    )
  }
  close(readfile_con)
}

# ----------------------------------------------------------------------------------------
# helper functions
# ----------------------------------------------------------------------------------------
#
#' Check if the current chunk includes a header row describing the fields
#' @importFrom stringr str_sub

.has_header <- function(raw_chunk){
  any(str_sub(raw_chunk, 1, 4) == "#chr")
}
