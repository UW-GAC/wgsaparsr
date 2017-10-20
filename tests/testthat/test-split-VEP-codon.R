context("test_.split_VEP_codon - unit tests")

test_that(".split_VEP_codon returns expected tibble", {
  example <- dplyr::tibble(
    "VEP_ensembl_Codon_Change_or_Distance" = c("123",
                "42",
                "foo",
                "waa")
  )
  target <- dplyr::tibble(
    "VEP_ensembl_Distance" = c("123",
                "42",
                ".",
                "."),
    "VEP_ensembl_Codon_Change" = c(".",
                         ".",
                         "foo",
                         "waa")
  )
  result <- .split_VEP_codon(example)
  expect_identical(result, target)
})
