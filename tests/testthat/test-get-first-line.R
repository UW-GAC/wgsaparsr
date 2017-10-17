context("test_.get_first_line - unit tests")

test_that(
  ".get_first_line returns a character vector from a test file", {
    result <- .get_first_line("1k_annotation.gz")
    expect_true(all(c(rlang::is_atomic(result),
                      rlang::is_vector(result),
                      rlang::is_character(result)
    )))
  }
)

test_that(
  ".get_first_line returns the expected character vector from a test file", {
    result <- .get_first_line("1k_annotation.gz")
    expect_true(any(stringr::str_detect(result, "^#?chr\\tpos\\tref\\talt"))
    )
  }
)
