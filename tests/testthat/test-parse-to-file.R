context("test_parse-to-file - unit tests")

test_that("parse_to_file() selects SNV fields", {
  snv_tmp <- tempfile()
  on.exit(unlink(snv_tmp))

  source_file <- "unparsed_simple.tsv"
  config <-
    tibble::tibble(
      field = c("Header 1", "Header 3"),
      SNV = c(TRUE, TRUE),
      indel = c(FALSE, FALSE),
      dbnsfp = c(FALSE, FALSE),
      pivotGroup = c(NA, NA),
      pivotChar = c(NA, NA),
      parseGroup = c(NA, NA),
      transformation = c(NA, NA),
      toRemove = c(NA, NA)
    )
  destination = snv_tmp

  parse_to_file(source_file = source_file,
                 config = config,
                 destination = snv_tmp,
                 dbnsfp_destination = NA,
                 chunk_size = 10000,
                 header_file = NA,
                 verbose = FALSE)

  written <- readr::read_lines(snv_tmp, progress = FALSE)
  expect_equal(written, c("Header 1\tHeader 3", "foo\tbat"))
})
