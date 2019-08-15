context("test_.clean_config() - unit tests")

test_that(".clean_config() returns expected tibble", {
  example <- tibble::tibble(
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

  target <- tibble::tibble(
    field = c("Header 1", "Header 3"),
    SNV = c(TRUE, FALSE),
    indel = c(TRUE, TRUE),
    dbnsfp = c(FALSE, FALSE),
    pivotGroup = c(NA, "1"),
    pivotChar = c("|", ";"),
    parseGroup = c("1", "2"),
    transformation = c(NA, "min")
  )
  result <- .clean_config(example)
  expect_identical(result, target)
})

test_that(".clean_config() returns expected ordered tibble", {
  example <- tibble::tibble(
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
    outputOrder = c(NA, "3", "2", "1")
  )

  target <- tibble::tibble(
    field = c("Header 3", "Header 1"),
    SNV = c(FALSE, TRUE),
    indel = c(TRUE, TRUE),
    dbnsfp = c(FALSE, FALSE),
    pivotGroup = c("1", NA),
    pivotChar = c(";", "|"),
    parseGroup = c("2", "1"),
    transformation = c("min", NA),
    outputOrder = c("2", NA)
  )
  result <- .clean_config(example)
  expect_identical(result, target)
})

test_that(".clean_config() returns expected tibble with toRemove", {
  example <- tibble::tibble(
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

  target <- tibble::tibble(
    field = c("Header 1", "Header 3"),
    SNV = c(TRUE, FALSE),
    indel = c(TRUE, TRUE),
    dbnsfp = c(FALSE, FALSE),
    pivotGroup = c(NA, "1"),
    pivotChar = c("|", ";"),
    parseGroup = c("1", "2"),
    transformation = c(NA, "min"),
    toRemove = c("^\\.$", "^NULL$")
  )
  result <- .clean_config(example)
  expect_identical(result, target)
})

test_that(".clean_config() returns expected tibble with outputName", {
  example <- tibble::tibble(
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
    outputName = c("Header A", NA, "Header B", "Header C")
  )

  target <- tibble::tibble(
    field = c("Header 1", "Header 3"),
    SNV = c(TRUE, FALSE),
    indel = c(TRUE, TRUE),
    dbnsfp = c(FALSE, FALSE),
    pivotGroup = c(NA, "1"),
    pivotChar = c("|", ";"),
    parseGroup = c("1", "2"),
    transformation = c(NA, "min"),
    outputName = c("Header A", "Header B")
  )
  result <- .clean_config(example)
  expect_identical(result, target)
})

test_that(".clean_config() returns expected tibble with NA in outputName", {
  example <- tibble::tibble(
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
    outputName = c("Header A", NA, NA, "Header C")
  )

  target <- tibble::tibble(
    field = c("Header 1", "Header 3"),
    SNV = c(TRUE, FALSE),
    indel = c(TRUE, TRUE),
    dbnsfp = c(FALSE, FALSE),
    pivotGroup = c(NA, "1"),
    pivotChar = c("|", ";"),
    parseGroup = c("1", "2"),
    transformation = c(NA, "min"),
    outputName = c("Header A", "Header 3")
  )
  result <- .clean_config(example)
  expect_identical(result, target)
})

test_that(".clean_config() returns expected tibble with pivotChar2", {
  example <- tibble::tibble(
    parseGroup = c("1", "1", "2", NA),
    field = c("Header 1", NA, "Header 3", "Header 4"),
    pivotGroup = c(NA, "1", "1", "1"),
    indel = c(TRUE, NA, TRUE, TRUE),
    sourceGroup = c("1", "1", "2", "2"),
    dbnsfp = c(NA, FALSE, FALSE, FALSE),
    SNV = c(TRUE, FALSE, NA, NA),
    transformation = c(NA, "max", "min", NA),
    pivotChar = c("|", NA, ";", ";"),
    pivotChar2 = c(NA, NA, "|", NA),
    notes = c("a note", "another", "foo", "bar"),
    outputName = c("Header A", NA, NA, "Header C")
  )

  target <- tibble::tibble(
    field = c("Header 1", "Header 3", "Header 4"),
    SNV = c(TRUE, FALSE, FALSE),
    indel = c(TRUE, TRUE, TRUE),
    dbnsfp = c(FALSE, FALSE, FALSE),
    pivotGroup = c(NA, "1", "1"),
    pivotChar = c("|", ";", ";"),
    parseGroup = c("1", "2", NA),
    transformation = c(NA, "min", NA),
    pivotChar2 = c(NA, "|", NA),
    outputName = c("Header A", "Header 3", "Header C")
  )
  result <- .clean_config(example)
  expect_identical(result, target)
})
