context("test_.parse_indel_no_columns - unit tests")

test_that(".parse_indel_no_columns returns expected tibble", {
  example <- dplyr::tibble(
    "a_col" = c("1;2",
                "42",
                "37",
                "waa"),
    "col_to_no" = c("Y;N",
                    "Y;Y",
                    "abc",
                    ".")
  )
  target <- dplyr::tibble(
    "a_col" = c("1;2",
                "42",
                "37",
                "waa"),
    "col_to_no_unparsed" = c("Y;N",
                    "Y;Y",
                    "abc",
                    "."),
    "col_to_no" = c("N",
                    "Y",
                    ".",
                    ".")
  )
  result <- .parse_indel_no_columns(example, "col_to_no")
  expect_identical(result, target)
})
