context("test_.parse_indel_yes_columns - unit tests")

test_that(".parse_indel_yes_columns returns expected tibble", {
  example <- dplyr::tibble(
    "a_col" = c("1;2",
                "42",
                "37",
                "waa"),
    "col_to_yes" = c("Y;N",
                    "N;N",
                    "abc",
                    ".")
  )
  target <- dplyr::tibble(
    "a_col" = c("1;2",
                "42",
                "37",
                "waa"),
    "col_to_yes_unparsed" = c("Y;N",
                    "N;N",
                    "abc",
                    "."),
    "col_to_yes" = c("Y",
                    "N",
                    ".",
                    ".")
  )
  result <- .parse_indel_yes_columns(example, "col_to_yes")
  expect_identical(result, target)
})
