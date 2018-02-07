context("test_.pivot_fields() - unit tests")

test_that(".pivot_fields() gives error if pivot_columns is not a list", {
  example <- dplyr::tibble(a = "a")
  expect_error(.pivot_fields(example, "not a list"),
               "pivot_columns must be a list")
})

test_that(".pivot_fields() returns expected tibble when parsing a single", {
  example <- dplyr::tibble(
    a = c("1;2"),
    b = c("3|4")
  )
  target <- dplyr::tibble(
    a = c("1", "1", "2", "2"),
    b = c("3", "4", "3", "4")
  )
  to_pivot <- list(
    "1" = dplyr::tibble(
      field = "a",
      pivotChar = ";"),
    "2" = dplyr::tibble(
      field = "b",
      pivotChar = "|")
    )
  result <- .pivot_fields(example, to_pivot)
  expect_identical(result, target)
})
