context("test_.pad_dots() - unit tests")

test_that(".pad_dots() returns expected tibble", {
  example <-
    tibble::tibble(
      FATHMM_score = c(".", NA, "1;2"),
      FATHMM_pred = c(".", NA, "1;2"),
      Ensembl_transcriptid = c("a;b;c;d", ".", "e;f"),
      VEP_canonical = c(".;.;YES;.", "FOO", "YES;.")
    )

  target <-
    tibble::tibble(
      FATHMM_score = c(NA, "1;2", ".;.;.;."),
      FATHMM_pred = c(NA, "1;2", ".;.;.;."),
      Ensembl_transcriptid = c(".", "e;f", "a;b;c;d"),
      VEP_canonical = c("FOO", "YES;.", ".;.;YES;.")
    )

  fields <- c("FATHMM_score", "FATHMM_pred", "Ensembl_transcriptid",
              "VEP_canonical")

  result <- .pad_dots(example, fields)

  expect_identical(result, target)
})
