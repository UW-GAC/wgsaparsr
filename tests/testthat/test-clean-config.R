context("test_.clean_config() - unit tests")

test_that(".clean_config() returns expected tibble", {
  example <- dplyr::tibble(
    parseGroup = c("1", "1", "2", NA),
    field = c("Header 1", NA, "Header 3", "Header 4"),
    pivotGroup = c(NA, "1", "1", NA),
    indel = c(TRUE, NA, TRUE, NA),
    sourceGroup = c("1", "1", "2", "2"),
    dbnsfp = c(NA, FALSE, FALSE, NA),
    SNV = c(TRUE, FALSE, NA, NA),
    transformation = c(NA, "max", "min", NA),
    pivotChar = c("|", NA, ";", NA),
    notes = c("a note", "another", "foo", "bar")
  )
  
  target <- dplyr::tibble(
    field = c("Header 1", "Header 3"),
    SNV = c(TRUE, FALSE),
    indel = c(TRUE, TRUE),
    dbnsfp = c(FALSE, FALSE),
    sourceGroup = c("1", "2"),
    pivotGroup = c(NA, "1"),
    pivotChar = c("|", ";"),
    parseGroup = c("1", "2"),
    transformation = c(NA, "min")
  )
  result <- .clean_config(example)
  expect_identical(result, target)
})