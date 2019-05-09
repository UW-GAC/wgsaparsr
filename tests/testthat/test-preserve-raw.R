context("test_.preserve_raw() - unit tests")

test_that(".preserve_raw() returns expected tibble", {
  example <- tibble::tibble(
    col1 = c("a", "b"),
    col2 = c(1, 2))
  target <- tibble::tibble(
    col1 = c("a", "b"),
    col2 = c(1, 2),
    col1_unparsed = c("a", "b")
  )
  result <- .preserve_raw(example, "col1")
  expect_identical(result, target)
})
