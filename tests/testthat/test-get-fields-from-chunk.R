context("test_.get_fields_from_chunk - unit tests")

test_that(
  ".get_fields_from_chunk returns expected result", {
    test_chunk <- c(paste("#chr", "pos", "ref", "alt", sep = "\t"),
                    paste("1", "42", "A", "G", sep = "\t"),
                    paste("2", "12", "C", "T", sep = "\t"))
    expected_result <- tibble::tibble("#chr" = c("1", "2"),
                              "pos" = c("42", "12"),
                              "ref" = c("A", "C"),
                              "alt" = c("G", "T"))
    attributes(expected_result)$spec <-
      readr::cols(
          .default = readr::col_character(),
          `#chr` = readr::col_character(),
          pos = readr::col_character(),
          ref = readr::col_character(),
          alt = readr::col_character()
        )
    expect_equal(.get_fields_from_chunk(test_chunk), expected_result)
  }
)
