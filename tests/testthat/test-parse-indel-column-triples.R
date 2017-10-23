context("test_.parse_indel_column_triples - unit tests")
# need more tests - I'm not sure examples are good

test_that(".parse_indel_column_triples returns expected tibble", {
  example <- dplyr::tibble(
    "a_col" = c("1;2",
                "42",
                "37",
                "waa",
                "bar"),
    "col_1" = c("1;2",
                "7{3}1",
                "4",
                ".",
                ".{1}"),
    "col_2" = c("Y;N",
                "N;N",
                "abc",
                ".",
                ".{1}"),
    "col_3" = c("Y;N",
                "N;N",
                "abc",
                ".",
                ".{1}")
  )
  target <- dplyr::tibble(
    "a_col" = c("1;2",
                "42",
                "37",
                "waa",
                "bar"),
    "col_1_unparsed" = c("1;2",
                         "7{3}1",
                         "4",
                         ".",
                         ".{1}"),
    "col_2_unparsed" = c("Y;N",
                         "N;N",
                         "abc",
                         ".",
                         ".{1}"),
    "col_3_unparsed" = c("Y;N",
                "N;N",
                "abc",
                ".",
                ".{1}"),
    "col_1" = c("2",
                "7",
                "4",
                ".",
                "."),
    "col_2" = c("N",
                "N",
                "abc",
                ".",
                "."),
    "col_3" = c("N",
                "N",
                "abc",
                ".",
                ".")
  )
  result <- .parse_indel_column_triples(example,
                                        list(c("col_1", "col_2", "col_3")))
  expect_identical(result, target)
})
