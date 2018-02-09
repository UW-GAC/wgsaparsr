context("test_.parse_no_columns - unit tests")

test_that(".parse_no_columns returns expected tibble", {
  example <- dplyr::tibble(
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
  target <- dplyr::tibble(
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
  result <- .parse_no_columns(example, "col_to_no")
  expect_identical(result, target)
})
