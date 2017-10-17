context("test_.parse_indel_max_columns - unit tests")

test_that(".parse_indel_max_columns returns expected tibble", {
  example <- dplyr::tibble(
    "a_col" = c("1;2",
                "42"),
    "col_to_max" = c("1{23};2{1}",
                     "7")
  )
  target <- dplyr::tibble(
    "a_col" = c("1;2",
                "42"),
    "col_to_max_unparsed" = c("1{23};2{1}",
                     "7"),
    "col_to_max" = c("2",
                     "7")
  )
  result <- .parse_indel_max_columns(example, "col_to_max")
  expect_identical(result, target)
})
