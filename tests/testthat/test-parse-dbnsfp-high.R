context("test_.parse_dbnsfp_high - unit tests")

test_that(
  ".parse_dbnsfp_high returns expected tibble", {
    example <- dplyr::tibble("a" = c("3;4"),
                             "b" = c("a;b"))
    target <- dplyr::tibble("a_unparsed" = "3;4",
                            "b_unparsed" = "a;b",
                            "a" = c("4"),
                            "b" = c("b"))
    result <- .parse_dbnsfp_high(example, list(c("a", "b")))
    expect_identical(result, target)
  }
)
