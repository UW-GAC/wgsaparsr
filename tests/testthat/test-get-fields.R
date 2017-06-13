context("test_get_fields - unit tests")

test_that(
  "get_fields returns an error if the file header doesn't start with '#chr'", {
    expect_error(get_fields("bad_header.gz"),
                 "First line of source doesn't look like a WGSA header")
  }
)

test_that(
  "get_fields returns a character vector", {
    result <- get_fields("1k_annotation.gz")
    expect_true(all(c(rlang::is_atomic(result),
                      rlang::is_vector(result),
                      rlang::is_character(result)
                      )
                    )
                )
  }
)
