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
    pivotGroup = c(NA, "1"),
    pivotChar = c("|", ";"),
    parseGroup = c("1", "2"),
    transformation = c(NA, "min"),
    sourceGroup = c("1", "2")
  )
  result <- .clean_config(example)
  expect_identical(result, target)
})

test_that(".clean_config() returns expected ordered tibble", {
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
    notes = c("a note", "another", "foo", "bar"),
    order = c(NA, "3", "2", "1")
  )

  target <- dplyr::tibble(
    field = c("Header 3", "Header 1"),
    SNV = c(FALSE, TRUE),
    indel = c(TRUE, TRUE),
    dbnsfp = c(FALSE, FALSE),
    pivotGroup = c("1", NA),
    pivotChar = c(";", "|"),
    parseGroup = c("2", "1"),
    transformation = c("min", NA),
    order = c("2", NA),
    sourceGroup = c("2", "1")
  )
  result <- .clean_config(example)
  expect_identical(result, target)
})

test_that(".clean_config() returns expected tibble with toRemove", {
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
    notes = c("a note", "another", "foo", "bar"),
    toRemove = c(".", "NA", "NULL", ".")
  )

  target <- dplyr::tibble(
    field = c("Header 1", "Header 3"),
    SNV = c(TRUE, FALSE),
    indel = c(TRUE, TRUE),
    dbnsfp = c(FALSE, FALSE),
    pivotGroup = c(NA, "1"),
    pivotChar = c("|", ";"),
    parseGroup = c("1", "2"),
    transformation = c(NA, "min"),
    sourceGroup = c("1", "2"),
    toRemove = c("^\\.$", "^NULL$")
  )
  result <- .clean_config(example)
  expect_identical(result, target)
})
