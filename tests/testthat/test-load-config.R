context("test_load_config - unit tests")

test_that(
  "load_config() returns an error if validation fails", {
    expect_error(load_config("bad_config.tsv"),
                 "Required columns missing")
  }
)

test_that(
  "load_config() returns a tibble", {
    expect_true(tibble::is.tibble(
      load_config(system.file("extdata",
                              path = "fr_5_config.tsv",
                              package = "wgsaparsr",
                              mustWork = TRUE))))
  }
)

test_that(
  "load_config() returns a tibble of expected size", {
    config <- load_config(system.file("extdata",
                                      path = "fr_5_config.tsv",
                                      package = "wgsaparsr",
                                      mustWork = TRUE))
    expect_true(all(dim(config) == c(289, 8)))
  }
)
