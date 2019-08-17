context("test_validate_config() - unit tests")

test_that("validate_config() gives error when required column is missing", {
  example <- tibble::tibble(
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
  example <- tibble::tibble(
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
  example <- tibble::tibble(
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
  example <- tibble::tibble(
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
  example <- tibble::tibble(
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
  example <- tibble::tibble(
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
               "all pivotChar values must be the same within a pivotGroup")
})

test_that("validate_config() gives error when transformation groups wrong", {
  example <- tibble::tibble(
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
  example <- tibble::tibble(
    field = c("a", "b"),
    SNV = c("TRUE", "TRUE"),
    indel = c("TRUE", "TRUE"),
    dbnsfp = c("TRUE", "TRUE"),
    sourceGroup = c("1", "1"),
    pivotGroup = c("1", "1"),
    pivotChar = c(";", ";"),
    parseGroup = c("1", "1"),
    transformation = c("max", "max"),
    outputOrder = c("2", "1")
  )
  msg <- "configuration rows not arranged by outputOrder"
  expect_error(validate_config(example), msg)
})

test_that("validate_config() gives error when outputName has NAs", {
  example <- tibble::tibble(
    field = c("a", "b"),
    SNV = c("TRUE", "TRUE"),
    indel = c("TRUE", "TRUE"),
    dbnsfp = c("TRUE", "TRUE"),
    sourceGroup = c("1", "1"),
    pivotGroup = c("1", "1"),
    pivotChar = c(";", ";"),
    parseGroup = c("1", "1"),
    transformation = c("max", "max"),
    outputOrder = c("1", "2"),
    outputName = c("d", NA)
  )
  msg <- "outputName has NA values"
  expect_error(validate_config(example), msg)
})

test_that("validate_config() warns on empty config", {
  #empty config made with dput(.clean_config(example)) where example has all NAs
  example <-
    structure(
      list(field = logical(0), SNV = logical(0), indel = logical(0),
           dbnsfp = logical(0), pivotGroup = logical(0), pivotChar = logical(0),
           parseGroup = logical(0), transformation = logical(0),
           outputOrder = logical(0)),
      row.names = c(NA, 0L),
      class = c("tbl_df", "tbl", "data.frame"))

  expect_warning(validate_config(example), "configuration has zero rows")
})

test_that("validate_config() works with 1-length config", {
  example <- tibble::tibble(
    field = c("a"),
    SNV = c("TRUE"),
    indel = c("TRUE"),
    dbnsfp = c("TRUE"),
    sourceGroup = c("1"),
    pivotGroup = c("1"),
    pivotChar = c(";"),
    parseGroup = c("1"),
    transformation = c("max"),
    outputOrder = c("1"),
    outputName = c("d")
  )

  expect_true(validate_config(example))
})

test_that("validate_config() works if SNV is all NAs", {
  example <- tibble::tibble(
    field = c("a", "b"),
    SNV = c(NA, NA),
    indel = c("TRUE", "TRUE"),
    dbnsfp = c("TRUE", "TRUE"),
    sourceGroup = c("1", "1"),
    pivotGroup = c("1", "1"),
    pivotChar = c(";", ";"),
    parseGroup = c("1", "1"),
    transformation = c("max", "max"),
    outputOrder = c("1", "2"),
    outputName = c("d", "e")
  )

  expect_true(validate_config(example))
})

test_that("validate_config() works if indel is all NAs", {
  example <- tibble::tibble(
    field = c("a", "b"),
    SNV = c("TRUE", "TRUE"),
    indel = c(NA, NA),
    dbnsfp = c("TRUE", "TRUE"),
    sourceGroup = c("1", "1"),
    pivotGroup = c("1", "1"),
    pivotChar = c(";", ";"),
    parseGroup = c("1", "1"),
    transformation = c("max", "max"),
    outputOrder = c("1", "2"),
    outputName = c("d", "e")
  )

  expect_true(validate_config(example))
})

test_that("validate_config() works with dbnsfp NA", {
  example <- tibble::tibble(
    field = c("a"),
    SNV = c("TRUE"),
    indel = c("TRUE"),
    dbnsfp = c(NA),
    sourceGroup = c("1"),
    pivotGroup = c("1"),
    pivotChar = c(";"),
    parseGroup = c("1"),
    transformation = c("max"),
    outputOrder = c("1"),
    outputName = c("d")
  )
  expect_true(validate_config(example))
})

test_that("validate_config() works with sourceGroup NA", {
  example <- tibble::tibble(
    field = c("a"),
    SNV = c("TRUE"),
    indel = c("TRUE"),
    dbnsfp = c("TRUE"),
    sourceGroup = c(NA),
    pivotGroup = c("1"),
    pivotChar = c(";"),
    parseGroup = c("1"),
    transformation = c("max"),
    outputOrder = c("1"),
    outputName = c("d")
  )
  expect_true(validate_config(example))
})

test_that("validate_config() works with pivotChar NA", {
  example <- tibble::tibble(
    field = c("a"),
    SNV = c("TRUE"),
    indel = c("TRUE"),
    dbnsfp = c("TRUE"),
    sourceGroup = c("1"),
    pivotGroup = c("1"),
    pivotChar = c(NA),
    parseGroup = c("1"),
    transformation = c("max"),
    outputOrder = c("1"),
    outputName = c("d")
  )
  expect_true(validate_config(example))
})

test_that("validate_config() works with parseGroup NA", {
  example <- tibble::tibble(
    field = c("a"),
    SNV = c("TRUE"),
    indel = c("TRUE"),
    dbnsfp = c("TRUE"),
    sourceGroup = c("1"),
    pivotGroup = c("1"),
    pivotChar = c(";"),
    parseGroup = c(NA),
    transformation = c("max"),
    outputOrder = c("1"),
    outputName = c("d")
  )
  expect_true(validate_config(example))
})

test_that("validate_config() works with transformation NA", {
  example <- tibble::tibble(
    field = c("a"),
    SNV = c("TRUE"),
    indel = c("TRUE"),
    dbnsfp = c("TRUE"),
    sourceGroup = c("1"),
    pivotGroup = c("1"),
    pivotChar = c(";"),
    parseGroup = c("1"),
    transformation = c(NA),
    outputOrder = c("1"),
    outputName = c("d")
  )
  expect_true(validate_config(example))
})

test_that("validate_config() works with outputOrder NA", {
  example <- tibble::tibble(
    field = c("a"),
    SNV = c("TRUE"),
    indel = c("TRUE"),
    dbnsfp = c("TRUE"),
    sourceGroup = c("1"),
    pivotGroup = c("1"),
    pivotChar = c(";"),
    parseGroup = c("1"),
    transformation = c("max"),
    outputOrder = c(NA),
    outputName = c("d")
  )
  expect_true(validate_config(example))
})

test_that("validate_config() gives error when pivotChar2 wrong", {
  example <- tibble::tibble(
    field = c("a", "b"),
    SNV = c("TRUE", "TRUE"),
    indel = c("TRUE", "TRUE"),
    dbnsfp = c("TRUE", "TRUE"),
    sourceGroup = c("1", "1"),
    pivotGroup = c("1", "1"),
    pivotChar = c("|", "|"),
    pivotChar2 = c(";", "|"),
    parseGroup = c("1", "1"),
    transformation = c("max", "max")
  )
  expect_error(validate_config(example),
               "all pivotChar2 values must be the same within a pivotGroup")
})

test_that("validate_config() gives error if pivotChar2 w/o pivotChar", {
  example <- tibble::tibble(
    field = c("a", "b"),
    SNV = c("TRUE", "TRUE"),
    indel = c("TRUE", "TRUE"),
    dbnsfp = c("TRUE", "TRUE"),
    sourceGroup = c("1", "1"),
    pivotGroup = c("1", "1"),
    pivotChar = c(NA, "|"),
    pivotChar2 = c("|", "|"),
    parseGroup = c("1", "1"),
    transformation = c("max", "max")
  )
  expect_error(validate_config(example),
               "Fields with pivotChar2 values must have pivotChar values")
})

test_that("validate_config() gives error if pivotChar without pivotGroup", {
  example <- tibble::tibble(
    field = c("a", "b"),
    SNV = c("TRUE", "TRUE"),
    indel = c("TRUE", "TRUE"),
    dbnsfp = c("TRUE", "TRUE"),
    sourceGroup = c("1", "1"),
    pivotGroup = c(NA, "1"),
    pivotChar = c("|", "|"),
    pivotChar2 = c(NA, NA),
    parseGroup = c("1", "1"),
    transformation = c("max", "max")
  )
  expect_error(validate_config(example),
               "Fields with pivotChar values must have pivotGroup")
})

test_that("validate_config() works if pivotChar2 in 2nd pivot group has NA", {
  example <- tibble::tibble(
    field = c("a", "b", "c"),
    SNV = c("TRUE", "TRUE", "TRUE"),
    indel = c("TRUE", "TRUE", "TRUE"),
    dbnsfp = c("TRUE", "TRUE", "TRUE"),
    sourceGroup = c("1", "1", "1"),
    pivotGroup = c("1", "1", "2"),
    pivotChar = c("|", "|", "|"),
    pivotChar2 = c(NA, ";", NA),
    parseGroup = c("1", "1", "1"),
    transformation = c("max", "max", "max")
  )
  expect_error(validate_config(example), NA)
})
