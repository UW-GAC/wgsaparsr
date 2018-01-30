context("test_.parse_clean() - unit tests")

test_that(".parse_clean() returns expected tibble when parsing snv", {
  example <- dplyr::tibble(
    "Ensembl_Regulatory_Build_feature_type" = c(
      ".", "Enhancer"),
    "hESC_Topological_Domain" = c("1:834758-1314757", "."),
    "IMR90_Topological_Domain" = c("1:834758-1314757", ".")
  )
  # SHOULD TARGET INCLUDE _unparsed columns?
  target <- dplyr::tibble(
    "Ensembl_Regulatory_Build_feature_type" = c(
      ".", "Enhancer"),
    "hESC_Topological_Domain" = c("1:834758-1314757", "."),
    "IMR90_Topological_Domain" = c("1:834758-1314757", ".")
  )
  result <- .parse_clean(example, "col_to_max")
  expect_identical(result, target)
})

test_that(".parse_clean() returns expected tibble when parsing indel", {
  example <- dplyr::tibble(
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
  # SHOULD TARGET INCLUDE _unparsed columns?
  target <- dplyr::tibble(
    "Ensembl_Regulatory_Build_feature_type" =
      c(
      ".", "Enhancer"),
    "hESC_Topological_Domain" =
      c("1:834758-1314757", "."),
    "IMR90_Topological_Domain" =
      c("1:834758-1314757", ".")
  )
  result <- .parse_clean(example, "col_to_max")
  expect_identical(result, target)
})