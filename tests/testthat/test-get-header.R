context("test_.get_header - unit tests")

test_that(
  ".get_header stops with error if no header given", {
    msg <- "no header in source_file or header_file"
    expect_error(
      .get_header(source_file = "snv_10_lines.tsv",
                  header_file = NA), msg)
  }
)

test_that(
  ".get_header returns expected header if header given twice", {
    msg <- "headers in both header_file and source_file"
    expected <- .get_first_line("fr_5_header.tsv")
    result <- .get_header(source_file = "1k_annotation.gz",
                          header_file = "fr_5_header.tsv")
    expect_equal(expected, result)
  }
)

test_that(
  ".get_header returns error if two different headers", {
    msg <- "headers in header_file and source_file don't match"
    expect_error(
      .get_header(source_file = "1k_annotation.gz",
                  header_file = "bad_config.tsv"), msg)
  }
)

test_that(
  ".get_header returns expected header from header_file", {
    result <- .get_header(source_file = "snv_10_lines.tsv",
                          header_file = "snv_header.tsv")
    expect_true(any(stringr::str_detect(result, "^CHROM\\tPOS\\tREF\\tALT")))
  }
)

test_that(
  ".get_header returns expected header from source_file", {
    result <- .get_header(source_file = "1k_annotation.gz",
                          header_file = NA)
    expect_true(any(stringr::str_detect(result, "^#?chr\\tpos\\tref\\talt")))
  }
)
