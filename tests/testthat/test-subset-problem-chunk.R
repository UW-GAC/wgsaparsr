context("test_subset_problem_chunk - unit tests")

test_that(
  "get_subset_chunk returns the expected tibble", {
    load("loaded_chunk.RDa")

    expected_chunk <- loaded_chunk %>%
      dplyr::select(pos, ref, alt, unique_variant)

    test_config <- tibble::tibble(
      field = c("pos", "ref", "alt", "unique_variant"),
      SNV = c(TRUE, TRUE, TRUE, TRUE),
      indel = c(FALSE, TRUE, FALSE, FALSE),
      dbnsfp = c(FALSE, FALSE, TRUE, FALSE),
      sourceGroup = c("1", "2", "3", "4"),
      pivotGroup = c("1", "1", "1", "1"),
      pivotChar = c("|", "|", "|", "|"),
      parseGroup = c("1", "1", "2", "2"),
      transformation = c(NA, "max", "max", NA)
    )

    snv_selected <- subset_problem_chunk(loaded_chunk, test_config, "SNV")

    expect_identical(expected_chunk,
                     subset_problem_chunk(loaded_chunk, test_config, "SNV"))
  }
)
