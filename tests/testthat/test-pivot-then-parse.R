context("test_.pivot_then_parse() - unit tests")

test_that(".pivot_then_parse() gives error when wrong type argument given", {
  expect_error(.pivot_then_parse(chunk = "a", config = "b", type = "badtype"),
               'type must be "dbnsfp"')
})

test_that(".pivot_then_parse() gives desired field not in chunk", {
  chunk <- tibble::tibble(
    a = c("foo")
  )
  config <- tibble::tibble(
    field = c("a", "b"),
    SNV = c(TRUE, TRUE), # or could be logical instead of string
    indel = c(TRUE, TRUE),
    dbnsfp = c(TRUE, TRUE),
    sourceGroup = c("1", "1"),
    pivotGroup = c("1", "1"),
    pivotChar = c(";", ";"),
    parseGroup = c("1", "1"),
    transformation = c("max", "max")
  )
  expect_error(.pivot_then_parse(chunk, config, type = "dbnsfp"),
               "not all desired fields are in sourcefile")
})

test_that(".pivot_then_parse() warns if no aaref/aaalt", {
  chunk <- tibble::tibble(
    a = c("2|2;3", "3", "1;4"),
    b = c("waa", "bim", "bam"),
    c = c("foo", "bar", "baz;bat")

  )
  config <- tibble::tibble(
    field = c("a", "b", "c"),
    SNV = c(FALSE, TRUE, FALSE),
    indel = c(FALSE, FALSE, FALSE),
    dbnsfp = c(TRUE, FALSE, TRUE),
    sourceGroup = c("1", "1", "2"),
    pivotGroup = c(1, NA, NA),
    pivotChar = c("|", NA, NA),
    parseGroup = c("1", NA, NA),
    transformation = c("max", NA, NA)
  )
  target <- tibble::tibble(
    a = c("2", "3", "3", "4"),
    c = c("foo", "foo", "bar", "baz;bat"),
    a_unparsed = c("2", "2;3", "3", "1;4")
  )
  msg <- paste0("'aaref' and 'aaalt' not in desired dbnsfp fields. Can't ",
                "filter variants not annotated by dbnsfp.")
  expect_warning(
    .pivot_then_parse(chunk, config, type = "dbnsfp"),
    msg)
})

test_that(".pivot_then_parse() returns expected tibble (no filter)", {
  chunk <- tibble::tibble(
    a = c("2|2;3", "3", "1;4"),
    b = c("waa", "bim", "bam"),
    c = c("foo", "bar", "baz;bat"),
    aaref = c("A", "A", "A"),
    aaalt = c("G", "G", "G")
  )
  config <- tibble::tibble(
    field = c("a", "b", "c", "aaref", "aaalt"),
    SNV = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    indel = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    dbnsfp = c(TRUE, FALSE, TRUE, TRUE, TRUE),
    sourceGroup = c("1", "1", "2", "", ""),
    pivotGroup = c(1, NA, NA, NA, NA),
    pivotChar = c("|", NA, NA, NA, NA),
    parseGroup = c("1", NA, NA, NA, NA),
    transformation = c("max", NA, NA, NA, NA)
  )
  target <- tibble::tibble(
    a = c("2", "3", "3", "4"),
    c = c("foo", "foo", "bar", "baz;bat"),
    aaref = c("A", "A", "A", "A"),
    aaalt = c("G", "G", "G", "G"),
    a_unparsed = c("2", "2;3", "3", "1;4")
  )
  result <- .pivot_then_parse(chunk, config, type = "dbnsfp")
  expect_identical(target, result)
})

test_that(".pivot_then_parse() returns expected tibble (filtered)", {
  chunk <- tibble::tibble(
    a = c("2|2;3", "3", "1;4"),
    b = c("waa", "bim", "bam"),
    c = c("foo", "bar", "baz;bat"),
    aaref = c("A", ".", "A"),
    aaalt = c("G", ".", "G")
  )
  config <- tibble::tibble(
    field = c("a", "b", "c", "aaref", "aaalt"),
    SNV = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    indel = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    dbnsfp = c(TRUE, FALSE, TRUE, TRUE, TRUE),
    sourceGroup = c("1", "1", "2", "", ""),
    pivotGroup = c(1, NA, NA, NA, NA),
    pivotChar = c("|", NA, NA, NA, NA),
    parseGroup = c("1", NA, NA, NA, NA),
    transformation = c("max", NA, NA, NA, NA)
  )
  target <- tibble::tibble(
    a = c("2", "3", "4"),
    c = c("foo", "foo", "baz;bat"),
    aaref = c("A", "A", "A"),
    aaalt = c("G", "G", "G"),
    a_unparsed = c("2", "2;3", "1;4")
  )
  result <- .pivot_then_parse(chunk, config, type = "dbnsfp")
  expect_identical(target, result)
})
