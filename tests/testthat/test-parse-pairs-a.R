context("test_.parse_pairs_a() - unit tests")

test_that(".parse_pairs_a() gives error if pair_columns is not a list", {
  example <- tibble::tibble(
    "a_col" = c("1;2", "42", "37", "waa", "Y", "N", ".", "8"),
    "col_to_a" = c("A;D;P;N;foo",
                   "D;P;N;foo",
                   "P;N;foo",
                   "waa",
                   "N",
                   "N;D",
                   "P;A",
                   "8"),
    "b_col" = c("1;2", "42", "37", "waa", "Y", "N", ".", "8")
    )
  expect_error(.parse_pairs_a(example, "not a list"),
               "pair_columns must be a list")
})

test_that(".parse_pairs_a() gives expected tibble when parsing a pair", {
  example <- tibble::tibble(
    "MutationTaster_pred" = c("N",
                              "D;D",
                              "D;N",
                              "D;N;N",
                              "D;D;D;D;N;N;N;N",
                              "."),
    "MutationTaster_score" = c("1",
                               "0.734887;0.734887",
                               "0.876553;1",
                               "1;0.588175;0.588175",
                               paste0("0.999829;0.999829;0.999829;0.999829;",
                                      "0.990216;0.990216;0.990216;0.990216"),
                               ".")
  )
  target <- tibble::tibble(
   "MutationTaster_pred_unparsed" = c("N",
                                      "D;D",
                                      "D;N",
                                      "D;N;N",
                                      "D;D;D;D;N;N;N;N",
                                      "."),
   "MutationTaster_score_unparsed" = c("1",
                                       "0.734887;0.734887",
                                       "0.876553;1",
                                       "1;0.588175;0.588175",
                                       paste0("0.999829;0.999829;0.999829;",
                                              "0.999829;0.990216;0.990216;",
                                              "0.990216;0.990216"),
                                       "."),
   "MutationTaster_pred" = c("N",
                             "D",
                             "D",
                             "D",
                             "D",
                             "."),
   "MutationTaster_score" = c("1",
                              "0.734887",
                              "0.876553",
                              "1",
                              "0.999829",
                              ".")
  )
  result <-
    .parse_pairs_a(example, list(c(
      "MutationTaster_pred",
      "MutationTaster_score"
    )))
  expect_identical(result, target)
})

test_that(".parse_pairs_a() returns expected tibble when parsing a single", {
  example <- tibble::tibble(
    "a_col" = c("1;2", "42", "37", "waa", "Y", "N", ".", "8"),
    "col_to_a" = c("A;D;P;N;foo",
                   "D;P;N;foo",
                   "P;N;foo",
                   "waa",
                   "N",
                   "N;D",
                   "P;A",
                   "8"),
    "b_col" = c("1;2", "42", "37", "waa", "Y", "N", ".", "8")
  )
  target <- tibble::tibble(
    "a_col" = c("1;2", "42", "37", "waa", "Y", "N", ".", "8"),
    "col_to_a" = c("A", "D", "P", ".", "N", "D", "A", "."),
    "b_col" = c("1;2", "42", "37", "waa", "Y", "N", ".", "8"),
    "col_to_a_unparsed" = c("A;D;P;N;foo",
                            "D;P;N;foo",
                            "P;N;foo",
                            "waa",
                            "N",
                            "N;D",
                            "P;A",
                            "8")
    )
  result <- .parse_pairs_a(example, list("col_to_a"))
  expect_identical(result, target)
})

test_that(".parse_pairs_a() gives error when parsing a tripple", {
  example <- tibble::tibble(
    "a_col" = c("1;2", "42", "37", "waa", "Y", "N", ".", "8"),
    "col_to_a" = c("A;D;P;N;foo",
                   "D;P;N;foo",
                   "P;N;foo",
                   "waa",
                   "N",
                   "N;D",
                   "P;A",
                   "8"),
    "b_col" = c("1;2", "42", "37", "waa", "Y", "N", ".", "8")
  )
  expect_error(.parse_pairs_a(example, list(c("a_col", "col_to_a", "b_col"))),
               "pair columns not length 1 or 2")
})
