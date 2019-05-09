context("test_.rename_fields() - unit tests")

test_that(".rename_fields() returns expected 1-length list", {
  config <- tibble::tibble(
    field = c("Header 1", "Header 3"),
    SNV = c(TRUE, FALSE),
    indel = c(TRUE, TRUE),
    dbnsfp = c(FALSE, FALSE),
    pivotGroup = c(NA, "1"),
    pivotChar = c("|", ";"),
    parseGroup = c("1", "2"),
    transformation = c(NA, "min"),
    outputName = c("Header A", "Header B"),
    toRemove = c("^\\.$", "^NULL$")
  )

  snv_ex <- list(field = "Header 1")

  result <- .rename_fields(config, snv_ex)

  target <- list("Header A")

  expect_identical(result, target)
})

test_that(".rename_fields() returns expected 2-length list", {
  config <- tibble::tibble(
    field = c("Header 1", "Header 3"),
    SNV = c(TRUE, FALSE),
    indel = c(TRUE, TRUE),
    dbnsfp = c(FALSE, FALSE),
    pivotGroup = c(NA, "1"),
    pivotChar = c("|", ";"),
    parseGroup = c("1", "2"),
    transformation = c(NA, "min"),
    outputName = c("Header A", "Header B"),
    toRemove = c("^\\.$", "^NULL$")
  )

  indel_ex <- list("Header 1", "Header 3")

  result <- .rename_fields(config, indel_ex)

  target <- list("Header A", "Header B")

  expect_identical(result, target)
})

test_that(".rename_fields() returns expected 0-length list", {
  config <- tibble::tibble(
    field = c("Header 1", "Header 3"),
    SNV = c(TRUE, FALSE),
    indel = c(TRUE, TRUE),
    dbnsfp = c(FALSE, FALSE),
    pivotGroup = c(NA, "1"),
    pivotChar = c("|", ";"),
    parseGroup = c("1", "2"),
    transformation = c(NA, "min"),
    outputName = c("Header A", "Header B"),
    toRemove = c("^\\.$", "^NULL$")
  )

  dbnsfp_ex <- list()

  result <- .rename_fields(config, dbnsfp_ex)

  target <- list()

  expect_identical(result, target)
})

test_that(".rename_fields() returns error if config doesn't have outputName", {
  config <- tibble::tibble(
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

  dbnsfp_ex <- list()

  expect_error(
    .rename_fields(config, dbnsfp_ex),
    "Config filed doesn't have required 'outputName' and 'field' columns.")
})
