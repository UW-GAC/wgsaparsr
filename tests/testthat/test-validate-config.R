context("test_validate_config() - unit tests")

test_that("validate_config() gives error when required column is missing", {
  example <- dplyr::tibble(
    #field = "a", #required field now missing
    SNV = "a",
    indel = "a",
    dbnsfp = "a",
    sourceGroup = "a",
    pivotGroup = "a",
    pivotChar = "a",
    parseGroup = "a",
    transformation = "a"
  )
  expect_error(validate_config(example),
               "Required columns missing")
})

test_that("validate_config() gives error when SNV has bad value", {
  example <- dplyr::tibble(
    field = "a",
    SNV = "a",
    indel = "TRUE",
    dbnsfp = "TRUE",
    sourceGroup = "1",
    pivotGroup = "1",
    pivotChar = ";",
    parseGroup = "1",
    transformation = "max"
  )
  expect_error(validate_config(example),
               "SNV field has values other than TRUE, FALSE, or NA")
})

test_that("validate_config() gives error when indel has bad value", {
  example <- dplyr::tibble(
    field = "a",
    SNV = "TRUE",
    indel = "a",
    dbnsfp = "TRUE",
    sourceGroup = "1",
    pivotGroup = "1",
    pivotChar = ";",
    parseGroup = "1",
    transformation = "max"
  )
  expect_error(validate_config(example),
               "indel field has values other than TRUE, FALSE, or NA")
})

test_that("validate_config() gives error when dbnsfp has bad value", {
  example <- dplyr::tibble(
    field = "a",
    SNV = "TRUE",
    indel = "TRUE",
    dbnsfp = "a",
    sourceGroup = "1",
    pivotGroup = "1",
    pivotChar = ";",
    parseGroup = "1",
    transformation = "max"
  )
  expect_error(validate_config(example),
               "dbnsfp field has values other than TRUE, FALSE, or NA")
})

test_that("validate_config() gives error when transformation has bad value", {
  example <- dplyr::tibble(
    field = "a",
    SNV = "TRUE",
    indel = "TRUE",
    dbnsfp = "TRUE",
    sourceGroup = "1",
    pivotGroup = "1",
    pivotChar = ";",
    parseGroup = "1",
    transformation = "foo"
  )
  expect_error(validate_config(example),
               "transformation field has unrecognized values")
})

test_that("validate_config() gives error when pivot char wrong", {
  example <- dplyr::tibble(
    field = c("a", "b"),
    SNV = c("TRUE", "TRUE"),
    indel = c("TRUE", "TRUE"),
    dbnsfp = c("TRUE", "TRUE"),
    sourceGroup = c("1", "1"),
    pivotGroup = c("1", "1"),
    pivotChar = c(";", "|"),
    parseGroup = c("1", "1"),
    transformation = c("max", "max")
  )
  expect_error(validate_config(example),
               "all pivotChar values must be the same withinin a pivotGroup")
})

test_that("validate_config() gives error when transformation groups wrong", {
  example <- dplyr::tibble(
    field = c("a", "b"),
    SNV = c("TRUE", "TRUE"),
    indel = c("TRUE", "TRUE"),
    dbnsfp = c("TRUE", "TRUE"),
    sourceGroup = c("1", "1"),
    pivotGroup = c("1", "1"),
    pivotChar = c(";", ";"),
    parseGroup = c("1", "1"),
    transformation = c("max", "min")
  )
  msg <- paste0("all transformation values must be the same withinin a ",
               "parseGroup")
  expect_error(validate_config(example), msg)
})

test_that("validate_config() gives error when not in order", {
  example <- dplyr::tibble(
    field = c("a", "b"),
    SNV = c("TRUE", "TRUE"),
    indel = c("TRUE", "TRUE"),
    dbnsfp = c("TRUE", "TRUE"),
    sourceGroup = c("1", "1"),
    pivotGroup = c("1", "1"),
    pivotChar = c(";", ";"),
    parseGroup = c("1", "1"),
    transformation = c("max", "max"),
    order = c("2", "1")
  )
  msg <- "configuration rows not arranged by order"
  expect_error(validate_config(example), msg)
})
