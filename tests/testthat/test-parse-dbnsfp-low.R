context("test_.parse_dbnsfp_low - unit tests")

test_that(
  ".parse_dbnsfp_low returns expected tibble", {
    example <- dplyr::tibble("a" = c("3;4"),
                             "b" = c("a;b"))
    target <- dplyr::tibble("a_unparsed" = "3;4",
                            "b_unparsed" = "a;b",
                            "a" = c("3"),
                            "b" = c("a"))
    result <- .parse_dbnsfp_low(example, list(c("a", "b")))
    expect_identical(result, target)
  }
)
