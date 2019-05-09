#' Parse the WGSA output file to tidy and select columns of interest
#'
#' \href{https://sites.google.com/site/jpopgen/wgsa}{WGSA} generates distinct
#' annotation output files for annotating indel or SNVs. These output files
#' contain thousands of fields, including fields with lists of entries.
#' parse_to_file() reads a WGSA output file in chunks, parses the chunks, writes
#' to two output files - one for the snv or indel annotation, and another for
#' the dbnsfp annotation. These tab-separated output files can then be imported
#' to a database for aggregation, or used for obtaining variant annotation.
#'
#' @param source_file Path to the WGSA output file to parse (indel or SNV
#'   annotation)
#' @param config Path to config file or a dataframe that passes
#'   validate_config()
#' @param destination Path to the desired indel or snv output file
#' @param dbnsfp_destination Path to the desired dbnsfp output file
#' @param chunk_size Number of lines to parse each iteration (default 10,000)
#' @param header_file Path to a header file (optional)
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
                          config,
                          destination = NA,
                          dbnsfp_destination = NA,
                          chunk_size = 10000,
                          header_file = NA,
                          verbose = TRUE) {

  # there must be a destination-------------------
  if (all(is.na(c(destination, dbnsfp_destination)))) {
    stop("no outfile destination(s) specified")
  }

  # don't accidentally overwrite existing files---------
  if (file.exists(destination)) {
    stop("destination outfile already exists.")
  }
  if (!is.na(dbnsfp_destination) && file.exists(dbnsfp_destination)) {
    stop("dbnsfp_destination outfile already exists.")
  }

  # check config-------------------
  if ("data.frame" %in% class(config)) {
    validate_config(config)
  } else {
    config <- load_config(config)
  }

  # get header----------------------------------------
  raw_header <- .get_header(source_file, header_file)

  # check if parsing indel file------------
  indel_flag <- .is_indel(raw_header)

  # initialize output file(s) by writing header(s)----------------
  .write_output_header(config, destination, dbnsfp_destination, indel_flag)

  # main loop - read file by chunk, process chunk, write chunk----------------
  readfile_con <- gzfile(source_file, "r")
  index <- 0L
  while (TRUE) {
    # read chunk
    raw_chunk <- suppressWarnings(readLines(readfile_con, n = chunk_size))

    # readLines() returns a zero length result at EOF (which SHOULD end loop)
    if (length(raw_chunk) == 0) {
      break
    }

    # if header line in this chunk, read raw chunk to all_fields tibble
    header_flag <- .has_header(raw_chunk)

    if (header_flag) {
      all_fields <- .get_fields_from_chunk(raw_chunk)
    } else {
      modified_chunk <- c(raw_header, raw_chunk) # add the raw header
      all_fields <- .get_fields_from_chunk(modified_chunk)
    }

    # end current iteration if all_fields has 0 observations
    # (to avoid dplyr error arising from empty tibble)
    if (nrow(all_fields) == 0) {
      index <- index + 1L
      next
    }

    # don't need to get dbnsfp fields from indel chunk
    if (indel_flag == TRUE) {
      type <- "indel"
    } else {
      type <- "SNV"
    }

    # parse chunk of indel or SNV annotation
    parsed_lines <- .parse_then_pivot(all_fields, config, type)

    # get list of desired fields from config to ensure outfile column order
    parsed_fields <- .get_list_from_config(config, "desired", type)

    if (type == "SNV") {
      # parse dbnsfp fields from snv chunk
      dbnsfp_parsed_lines <- .pivot_then_parse(all_fields, config, "dbnsfp")

      # get list of desired fields from config to ensure outfile column order
      dbnsfp_parsed_fields <- .get_list_from_config(config, "desired", "dbnsfp")

      # if present, write dbnsfp data to tsv file
      if (nrow(dbnsfp_parsed_lines) > 0 & ncol(dbnsfp_parsed_lines) > 0) {
        # rename dbnsfp fields if needed
        if ("outputName" %in% colnames(config)) {
          dbnsfp_parsed_fields <- .rename_fields(config, dbnsfp_parsed_fields)
          dbnsfp_parsed_lines <-
            .rename_chunk_variables(config, dbnsfp_parsed_lines)
        }
        .write_to_file(
          dbnsfp_parsed_lines,
          dbnsfp_destination,
          dbnsfp_parsed_fields)
      }
    }

    # rename indel/snv fields if needed
    if ("outputName" %in% colnames(config)) {
      parsed_fields <- .rename_fields(config, parsed_fields)
      parsed_lines <- .rename_chunk_variables(config, parsed_lines)
    }

    # write processed indel or snv chunk to tsv file
    .write_to_file(parsed_lines,
                   destination,
                   parsed_fields)

    # update progress if desired
    if (verbose) {
      msg <- paste0(
        "Finished chunk ", index,
        "\n Sourcefile lines processed <= ",
        chunk_size * (index + 1),
        "\n Records in current import: ",
        dim(parsed_lines)[1]
      )
      message(msg)
    }

    # ready for the next chunk!
    index <- index + 1L

  }
  close(readfile_con)
  msg <- paste0(index, " chunks parsed.\n")
  message(msg)
  .last()
}
