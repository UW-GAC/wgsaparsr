context("test_.parse_then_pivot() - unit tests")

test_that(".parse_then_pivot() gives error when wrong type argument given", {
  expect_error(.parse_then_pivot(chunk = "a", config = "b", type = "badtype"),
               'type must be one of "SNV" or "indel"')
})

test_that(".parse_then_pivot() gives desired field not in chunk", {
  chunk <- tibble::tibble(
    a = c("foo")
  )
  config <- tibble::tibble(
    field = c("a", "b"),
    SNV = c(TRUE, TRUE),
    indel = c(TRUE, TRUE),
    dbnsfp = c(TRUE, TRUE),
    sourceGroup = c("1", "1"),
    pivotGroup = c("1", "1"),
    pivotChar = c(";", ";"),
    parseGroup = c("1", "1"),
    transformation = c("max", "max")
  )
  expect_error(.parse_then_pivot(chunk, config, type = "SNV"),
               "not all desired fields are in sourcefile")
})

test_that(".parse_then_pivot() returns expected tibble", {
  chunk <- tibble::tibble(
    a = c("1;2", "3", "4;5"),
    b = c("waa", "bim", "bam"),
    c = c("foo", "bar", "baz;bat")
  )
  config <- tibble::tibble(
    field = c("a", "b", "c"),
    SNV = c(TRUE, FALSE, TRUE),
    indel = c(FALSE, FALSE, FALSE),
    dbnsfp = c(FALSE, TRUE, FALSE),
    sourceGroup = c("1", "1", "2"),
    pivotGroup = c(NA, NA, 1),
    pivotChar = c(NA, NA, ";"),
    parseGroup = c("1", NA, NA),
    transformation = c("max", NA, NA)
  )
  target <- tibble::tibble(
    a = c("2", "3", "5", "5"),
    c = c("foo", "bar", "baz", "bat"),
    a_unparsed = c("1;2", "3", "4;5", "4;5")
  )
  result <- .parse_then_pivot(chunk, config, type = "SNV")
  expect_identical(target, result)
})
