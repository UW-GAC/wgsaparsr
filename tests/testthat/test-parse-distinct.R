context("test_.parse_distinct() - unit tests")

test_that(".collapse_uniqe() returns expected string", {
  example <- c("abc", "abc", "def", "def")
  target <- c("abc|def")
  result <- .collapse_unique(example)
  expect_identical(result, target)
})

test_that(".unique_values() returns expected string", {
  example <- c("abc{n};def",
               ".{n}abc;def",
               ".{3};abc;def",
               ".{1}Tr4;Egr1;Egr1{4}",
               ".{4}Egr1{3}Gabp{5}Gabp;Egr1{1}Gabp;Gabp{7}Gabp;Gabp;Egr1{4}",
               ".;")
  target <- c("abc|def",
              "abc|def",
              "abc|def",
              "Tr4|Egr1",
              "Egr1|Gabp",
              ".")
  result <- .unique_values(example)
  expect_identical(result, target)
})

test_that(".parse_distinct() returns expected tibble", {
  example <- dplyr::tibble(
    chr = c("1", "1"),
    pos = c("100", "200"),
    Ensembl_Regulatory_Build_TFBS =
      c(".{1}Tr4;Egr1;Egr1{4}",
        ".{4}Egr1{3}Gabp{5}Gabp;Egr1{1}Gabp;Gabp{7}Gabp;Gabp;Egr1{4}"))
  target <- dplyr::tibble(
    chr = c("1", "1"),
    pos = c("100", "200"),
    Ensembl_Regulatory_Build_TFBS = c("Tr4|Egr1", "Egr1|Gabp")
  )
  result <- .parse_distinct(example, "Ensembl_Regulatory_Build_TFBS")
  expect_identical(result, target)
})
