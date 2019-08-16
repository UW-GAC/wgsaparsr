context("test_.pivot_fields() - unit tests")

test_that(".pivot_fields() gives error if pivot_columns is not a list", {
  example <- tibble::tibble(a = "a")
  expect_error(.pivot_fields(example, "not a list"),
               "pivot_columns must be a list")
})

test_that(".pivot_fields() returns expected tibble when parsing a single", {
  example <- tibble::tibble(
    a = c("1;2"),
    b = c("3|4")
  )
  target <- tibble::tibble(
    a = c("1", "1", "2", "2"),
    b = c("3", "4", "3", "4")
  )
  to_pivot <- list(
    "1" = tibble::tibble(
      field = "a",
      pivotChar = ";"),
    "2" = tibble::tibble(
      field = "b",
      pivotChar = "|")
    )
  result <- .pivot_fields(example, to_pivot)
  expect_identical(result, target)
})

test_that(".pivot_fields() returns expected tibble with pivotChar2", {
  example <- tibble::tibble(
    a = c("1;2|3;4"),
    b = c("5|6")
  )
  target <- tibble::tibble(
    a = c("1", "2", "3", "4"),
    b = c("5", "5", "6", "6")
  )
  to_pivot <- list(
    "1" = tibble::tibble(
      field = c("a", "b"),
      pivotChar = c("|", "|"),
      pivotChar2 = c(";", NA)
    )
  )
  result <- .pivot_fields(example, to_pivot)
  expect_identical(result, target)
})
