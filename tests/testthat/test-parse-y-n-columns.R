context("test_.parse_yes_columns - unit tests")

test_that(".parse_yes_columns returns expected tibble", {
  example <- dplyr::tibble(
    "a_col" = c("1;2",
                "42",
                "37",
                "waa",
                "Y",
                "N",
                ".",
                "8"),
    "col_to_yes" = c("Y;N",
                     "N;N",
                     "abc",
                     ".",
                     "N{23}",
                     "N",
                     "N{23}Y{2}",
                     "N;Y{23}")
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
    "col_to_yes" = c("Y",
                     "N",
                     ".",
                     ".",
                     "N",
                     "N",
                     "Y",
                     "Y")
  )
  result <- .parse_yes_columns(example, "col_to_yes")
  expect_identical(result, target)
})
