context("test_.fix_nulls() - unit tests")

test_that(".fix_nulls() returns expected tibble", {
  chunk <- tibble::tibble(
    a = c("1.0", ".", "4.3"),
    b = c("waa", "bim", "bam"),
    c = c("foo", "bar", "baz"),
    d = c("null", "3.1", "null")
  )
  config <- tibble::tibble(
    field = c("a", "b", "c", "d"),
    SNV = c(TRUE, FALSE, TRUE, TRUE),
    indel = c(FALSE, FALSE, FALSE, TRUE),
    dbnsfp = c(FALSE, TRUE, FALSE, TRUE),
    sourceGroup = c("1", "1", "2", "2"),
    pivotGroup = c(NA, NA, 1, NA),
    pivotChar = c(NA, NA, ";", NA),
    parseGroup = c("1", NA, NA, NA),
    transformation = c("max", NA, NA, NA),
    toRemove = c(".", NA, ".", "null")
  )
  target <- tibble::tibble(
    a = c("1.0", "", "4.3"),
    b = c("waa", "bim", "bam"),
    c = c("foo", "bar", "baz"),
    d = c("", "3.1", "")
  )
  result <- .fix_nulls(chunk, config)
  expect_identical(target, result)
})
