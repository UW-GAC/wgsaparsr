context("test_.validate_config() - unit tests")

test_that(".validate_config() gives error when required column is missing", {
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
  expect_error(.validate_config(example),
               "Required columns are not in config tibble")
})

test_that(".validate_config() gives error when SNV has bad value", {
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
  expect_error(.validate_config(example),
               "SNV field has values other than TRUE, FALSE, or NA")
})

test_that(".validate_config() gives error when indel has bad value", {
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
  expect_error(.validate_config(example),
               "indel field has values other than TRUE, FALSE, or NA")
})

test_that(".validate_config() gives error when dbnsfp has bad value", {
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
  expect_error(.validate_config(example),
               "dbnsfp field has values other than TRUE, FALSE, or NA")
})

test_that(".validate_config() gives error when dbnsfp has bad value", {
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
  expect_error(.validate_config(example),
               "transformation field has unrecognized values")
})
