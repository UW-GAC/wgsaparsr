context("test_.parse_dbnsfp_mutation - unit tests")

test_that(".parse_dbnsfp_mutation returns expected tibble", {
  example <- dplyr::tibble(
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
  target <- dplyr::tibble(
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
    .parse_dbnsfp_mutation(example, list(c(
      "MutationTaster_pred",
      "MutationTaster_score"
    )))
  expect_identical(result, target)
})
