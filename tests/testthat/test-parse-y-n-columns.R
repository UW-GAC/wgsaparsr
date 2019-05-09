context("test_.parse_y_n_columns - unit tests")

test_that(".parse_y_n_columns returns expected yestibble", {
  example <- tibble::tibble(
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
  target <- tibble::tibble(
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
  result <- .parse_y_n_columns(example, "col_to_yes", "yes")
  expect_identical(result, target)
})

test_that(".parse_y_n_columns returns expected no tibble", {
  example <- tibble::tibble(
    "a_col" = c("1;2",
                "42",
                "37",
                "waa",
                "Y",
                "N",
                ".",
                "8"),
    "col_to_no" = c("Y;N",
                    "N;N",
                    "abc",
                    ".",
                    "N{23}",
                    "N",
                    "N{23}Y{2}",
                    "N;Y{23}")
  )
  target <- tibble::tibble(
    "a_col" = c("1;2",
                "42",
                "37",
                "waa",
                "Y",
                "N",
                ".",
                "8"),
    "col_to_no" = c("N",
                    "N",
                    ".",
                    ".",
                    "N",
                    "N",
                    "N",
                    "N")
  )
  result <- .parse_y_n_columns(example, "col_to_no", "no")
  expect_identical(result, target)
})
