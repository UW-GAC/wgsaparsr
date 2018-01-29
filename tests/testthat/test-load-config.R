context("test_loadConfig - unit tests")

test_that(
  "loadConfig() returns an error if required columns aren't present", {
    expect_error(loadConfig("bad_config.tsv"),
                 "Required columns are not in config tibble")
  }
)

test_that(
  "loadConfig() returns a tibble", {
    expect_true(tibble::is.tibble(
      loadConfig(system.file("extdata",
                              path = "fr_5_config.tsv",
                              package = "wgsaparsr",
                              mustWork = TRUE))))
  }
)

test_that(
  "loadConfig() returns a tibble of expected size", {
    config <- loadConfig(system.file("extdata",
                                      path = "fr_5_config.tsv",
                                      package = "wgsaparsr",
                                      mustWork = TRUE))
    expect_true(all(dim(config) == c(289, 9)))
  }
)
