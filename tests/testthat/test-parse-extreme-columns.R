context("test_.parse_extreme_columns() - unit tests")

test_that(".parse_extreme_columns() returns expected tibble for snv max", {
  example <- tibble::tibble(
    Polyphen2_HDIV_score = c(".", "1"),
    Eigen_PC_raw = c("-0.178845453566383", "2"),
    fathmm_MKL_coding_score = c("0.87811", "3"),
    MAP20_149bp = c("0.0", "4")
  )
  target <- tibble::tibble(
    Polyphen2_HDIV_score = c(".", "1"),
    Eigen_PC_raw = c("-0.178845453566383", "2"),
    fathmm_MKL_coding_score = c("0.87811", "3"),
    MAP20_149bp = c("0.0", "4")
  )
  result <- .parse_extreme_columns(example,
                         c("Polyphen2_HDIV_score",
                           "Eigen_PC_raw",
                           "fathmm_MKL_coding_score",
                           "MAP20_149bp"),
                         "max")
  expect_identical(result, target)
})

test_that(".parse_extreme_columns() returns expected tibble for indel max", {
  example <- tibble::tibble(
    Eigen_PC_raw = c(".{1}", "-0.164398825586883{1}",
                     ".{5}-0.00506108216253831{4}0.0200792355286427{1}",
                     ".{5}0.636779480039485{1}0.651616016738356{2}", ".{5}",
                     ".{3}0.382030178748437{1}0.394216023865136{1}",
                     ".{3}-0.0842385276257326{1}-0.0880785370562746{1}",
                     ".;-0.0842385276257326;-0.0880785370562746{1}"
                     ),
    fathmm_MKL_coding_score = c(".{1}", "0.87811{1}", ".{5}0.90605{5}",
                                ".{5}0.90734{3}", ".{5}", ".{3}0.90912{2}",
                                ".{3}0.90863{2}", "0.90863{3};0.90863{2}"),
    MAP20_149bp = c("0.0{1}", "0.004166664{1}", "0.015773803{10}",
                    "0.01666666{8}",
                    "0.09196428{1}0.09553571{1}0.09732143{1}0.09910714{1}",
                    "0.11339285{1}0.12053571{1}0.12410714{1}0.12767857{1}",
                    "0.19464286{1}0.19821429{1}0.20178571{1}0.20535715{1}",
                    "0.19464286{1};0.19821429{1}0.20178571{1};0.20535715{1}")
  )
  target <- tibble::tibble(
    Eigen_PC_raw = c(".", "-0.164398825586883", "0.0200792355286427",
                     "0.651616016738356", ".", "0.394216023865136",
                     "-0.0842385276257326", # or do we want -0.0880785370562746?
                     "-0.0842385276257326" # or do we want -0.0880785370562746?
    ),
    fathmm_MKL_coding_score = c(".", "0.87811", "0.90605", "0.90734", ".",
                                "0.90912", "0.90863", "0.90863"),
    # note: first value of parsed MAP20_149bp is "0", not "0.0". I think that's
    # okay
    MAP20_149bp = c("0", "0.004166664", "0.015773803", "0.01666666",
                    "0.09910714", "0.12767857", "0.20535715", "0.20535715")
  )
  result <- .parse_extreme_columns(example,
                               c("Eigen_PC_raw",
                                 "fathmm_MKL_coding_score",
                                 "MAP20_149bp"),
                               "max")
  expect_identical(result, target)
})

# add test based on observed bug when max_columns is a 1-length named list
test_that(".parse_extreme_columns() returns expected tibble for indel max", {
  example <- tibble::tibble(
    VEST3_score =
      c(".",
        "0.759;0.258;0.652;0.296;0.652;0.759;0.652;0.652;0.723",
        "0.75;0.205;0.663;0.236;0.663;0.75;0.663;0.663;0.705"),
    fathmm_MKL_coding_score = c(".{1}", "0.87811{1}", ".{5}0.90605{5}")
  )
  target <- tibble::tibble(
    VEST3_score = c(".", "0.759", "0.75"),
    fathmm_MKL_coding_score = c(".{1}", "0.87811{1}", ".{5}0.90605{5}")
  )
  max_columns <- list(field = c("VEST3_score"))
  result <- .parse_extreme_columns(example,
                               unlist(max_columns),
                               "max")
  expect_identical(result, target)
})

test_that(".parse_extreme_columns() returns expected tibble for snv min", {
  example <- tibble::tibble(
    Polyphen2_HDIV_score = c(".", "1"),
    Eigen_PC_raw = c("-0.178845453566383", "2"),
    fathmm_MKL_coding_score = c("0.87811", "3"),
    MAP20_149bp = c("0.0", "4")
  )
  target <- tibble::tibble(
    Polyphen2_HDIV_score = c(".", "1"),
    Eigen_PC_raw = c("-0.178845453566383", "2"),
    fathmm_MKL_coding_score = c("0.87811", "3"),
    MAP20_149bp = c("0.0", "4")
  )
  result <- .parse_extreme_columns(example,
                               c("Polyphen2_HDIV_score",
                                 "Eigen_PC_raw",
                                 "fathmm_MKL_coding_score",
                                 "MAP20_149bp"),
                               "min")
  expect_identical(result, target)
})

test_that(".parse_min_columns() returns expected tibble for indel min", {
  example <- tibble::tibble(
    Eigen_PC_raw = c(".{1}", "-0.164398825586883{1}",
                     ".{5}-0.00506108216253831{4}0.0200792355286427{1}",
                     ".{5}0.636779480039485{1}0.651616016738356{2}", ".{5}",
                     ".{3}0.382030178748437{1}0.394216023865136{1}",
                     ".{3}-0.0842385276257326{1}-0.0880785370562746{1}",
                     ".;-0.0842385276257326;-0.0880785370562746{1}"
    ),
    fathmm_MKL_coding_score = c(".{1}", "0.87811{1}", ".{5}0.90605{5}",
                                ".{5}0.90734{3}", ".{5}", ".{3}0.90912{2}",
                                ".{3}0.90863{2}", "0.90863{3};0.90863{2}"),
    MAP20_149bp = c("0.0{1}", "0.004166664{1}", "0.015773803{10}",
                    "0.01666666{8}",
                    "0.09196428{1}0.09553571{1}0.09732143{1}0.09910714{1}",
                    "0.11339285{1}0.12053571{1}0.12410714{1}0.12767857{1}",
                    "0.19464286{1}0.19821429{1}0.20178571{1}0.20535715{1}",
                    "0.19464286{1};0.19821429{1}0.20178571{1};0.20535715{1}")
  )
  target <- tibble::tibble(
    Eigen_PC_raw = c(".", "-0.164398825586883", "-0.00506108216253831",
                     "0.636779480039485", ".", "0.382030178748437",
                     "-0.0880785370562746", "-0.0880785370562746"),
    fathmm_MKL_coding_score = c(".", "0.87811", "0.90605", "0.90734", ".",
                                "0.90912", "0.90863", "0.90863"),
    # note: first value of parsed MAP20_149bp is "0", not "0.0". I think that's
    # okay
    MAP20_149bp = c("0", "0.004166664", "0.015773803", "0.01666666",
                    "0.09196428", "0.11339285", "0.19464286", "0.19464286")
  )
  result <- .parse_extreme_columns(example,
                               c("Eigen_PC_raw",
                                 "fathmm_MKL_coding_score",
                                 "MAP20_149bp"),
                               "min")
  expect_identical(result, target)
})
