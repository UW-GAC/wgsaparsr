context("test_.pivot_then_parse() - unit tests")

test_that(".pivot_then_parse() gives error when wrong type argument given", {
  expect_error(.pivot_then_parse(chunk = "a", config = "b", type = "badtype"),
               'type must be "dbnsfp"')
})

test_that(".pivot_then_parse() gives desired field not in chunk", {
  chunk <- dplyr::tibble(
    a = c("foo")
  )
  config <- dplyr::tibble(
    field = c("a", "b"),
    SNV = c("TRUE", "TRUE"), # or could be logical instead of string
    indel = c("TRUE", "TRUE"),
    dbnsfp = c("TRUE", "TRUE"),
    sourceGroup = c("1", "1"),
    pivotGroup = c("1", "1"),
    pivotChar = c(";", ";"),
    parseGroup = c("1", "1"),
    transformation = c("max", "max")
  )
  expect_error(.pivot_then_parse(chunk, config, type = "dbnsfp"),
               "not all desired fields are in sourcefile")
})

test_that(".pivot_then_parse() returns expected tibble", {
  chunk <- dplyr::tibble(
    a = c("2|2;3", "3", "1;4"),
    b = c("waa", "bim", "bam"),
    c = c("foo", "bar", "baz;bat")

  )
  config <- dplyr::tibble(
    field = c("a", "b", "c"),
    SNV = c("FALSE", "TRUE", "FALSE"),
    indel = c("FALSE", "FALSE", "FALSE"),
    dbnsfp = c("TRUE", "FALSE", "TRUE"),
    sourceGroup = c("1", "1", "2"),
    pivotGroup = c(1, NA, NA),
    pivotChar = c("|", NA, NA),
    parseGroup = c("1", NA, NA),
    transformation = c("max", NA, NA)
  )
  target <- dplyr::tibble(
    a = c("2", "3", "3", "4"),
    c = c("foo", "foo", "bar", "baz;bat"),
    a_unparsed = c("2", "2;3", "3", "1;4")
  )
  result <- .pivot_then_parse(chunk, config, type = "dbnsfp")
  expect_identical(target, result)
})
