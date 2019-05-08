context("test_.rename_chunk_variables() - unit tests")

test_that(".rename_fields() returns expected 1-length list", {
  chunk <- dplyr::tibble(
    a = c("1;2", "3", "4;5"),
    b = c("waa", "bim", "bam"),
    c = c("foo", "bar", "baz;bat")
  )

  config <- dplyr::tibble(
    field = c("a", "b", "c"),
    SNV = c(TRUE, FALSE, TRUE),
    indel = c(FALSE, FALSE, FALSE),
    dbnsfp = c(FALSE, TRUE, FALSE),
    sourceGroup = c("1", "1", "2"),
    pivotGroup = c(NA, NA, 1),
    pivotChar = c(NA, NA, ";"),
    parseGroup = c("1", NA, NA),
    transformation = c("max", NA, NA),
    outputName = c("new_a", "new_b", "new_c")
  )

  target <- dplyr::tibble(
    new_a = c("2", "3", "5", "5"),
    new_c = c("foo", "bar", "baz", "bat"),
    a_unparsed = c("1;2", "3", "4;5", "4;5")
  )
  #for SNV and indel
  parsed_lines <- .parse_then_pivot(chunk, config, type = "SNV")
  result <- .rename_chunk_variables(config, parsed_lines)

  expect_identical(result, target)
})
