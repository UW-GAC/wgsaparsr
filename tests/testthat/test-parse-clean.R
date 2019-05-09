context("test_.parse_clean() - unit tests")

test_that(".parse_clean() returns expected tibble when parsing snv", {
  example <- tibble::tibble(
    "Ensembl_Regulatory_Build_feature_type" = c(
      ".", "Enhancer"),
    "hESC_Topological_Domain" = c("1:834758-1314757", "."),
    "IMR90_Topological_Domain" = c("1:834758-1314757", ".")
  )
  target <- tibble::tibble(
    "Ensembl_Regulatory_Build_feature_type" = c(
      ".", "Enhancer"),
    "hESC_Topological_Domain" = c("1:834758-1314757", "."),
    "IMR90_Topological_Domain" = c("1:834758-1314757", ".")
  )
  result <- .parse_clean(example,
                         c("Ensembl_Regulatory_Build_feature_type",
                           "hESC_Topological_Domain",
                           "IMR90_Topological_Domain"))
  expect_identical(result, target)
})

test_that(".parse_clean() returns expected tibble when parsing indel", {
  example <- tibble::tibble(
    "Ensembl_Regulatory_Build_feature_type" =
      c(".{1}", ".{1}", "MA0341.1;MA0366.1{1}", "MA0341.1;MA0366.1{1}", ".{1}",
      ".{1}MA0341.1;MA0366.1{1}", "MA0341.1;MA0366.1{1}"),
    "hESC_Topological_Domain" =
      c(".{1}", "354{1}", "354{1}", "354;201{1}", "354;201{1}", "354;201{2}",
        "354;201{1}"),
    "IMR90_Topological_Domain" =
      c(".{1}", "HepG2,K562{1}", "HepG2,K562{1}", "HepG2,K562{1}",
        "HepG2,K562{1}", "HepG2,K562{2}", "HepG2,K562{2}")
  )

  target <- tibble::tibble(
    "Ensembl_Regulatory_Build_feature_type" =
      c(".", ".", "MA0341.1;MA0366.1", "MA0341.1;MA0366.1", ".",
        ".,MA0341.1;MA0366.1", "MA0341.1;MA0366.1"), # 6th entry may be wrong
    "hESC_Topological_Domain" =
      c(".", "354", "354", "354;201", "354;201", "354;201", "354;201"),
    "IMR90_Topological_Domain" =
      c(".", "HepG2,K562", "HepG2,K562", "HepG2,K562", "HepG2,K562",
        "HepG2,K562", "HepG2,K562")
  )
  result <- .parse_clean(example,
                         c("Ensembl_Regulatory_Build_feature_type",
                           "hESC_Topological_Domain",
                           "IMR90_Topological_Domain"))
  expect_identical(result, target)
})
