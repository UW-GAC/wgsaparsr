context("test_.parse_a_columns - unit tests")

test_that(".parse_a_columns returns expected tibble", {
  example <- dplyr::tibble(
    "a_col" = c("1;2",
                "42",
                "37",
                "waa",
                "Y",
                "N",
                ".",
                "8"),
    "col_to_a" = c("A;D;P;N;foo",
                   "D;P;N;foo",
                   "P;N;foo",
                   "waa",
                   "N",
                   "N;D",
                   "P;A",
                   "8")
  )
  target <- dplyr::tibble(
    "a_col" = c("1;2",
                "42",
                "37",
                "waa",
                "Y",
                "N",
                ".",
                "8"),
    "col_to_a" = c("A",
                   "D",
                   "P",
                   ".",
                   "N",
                   "D",
                   "A",
                   ".")
  )
  result <- .parse_a_columns(example, "col_to_a")
  expect_identical(result, target)
})
