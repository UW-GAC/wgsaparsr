context("test_.get_list_from_config() - unit tests")

test_that(".get_list_from_config() gives error for bad which_list argument", {
  msg <- paste0('which_list must be one of: "desired", "max", "min", ',
                '"pick_Y", "pick_N", "pick_A", "clean", "distinct", ',
                '"pivots", "max_pairs", "min_pairs", "pick_Y_pairs", ',
                '"pick_N_pairs", or "pick_A_pairs"')
  test_df <- tibble::tibble(
    field = c("Header 1", "Header 2", "Header 3"),
    SNV = c(TRUE, FALSE, FALSE),
    indel = c(FALSE, TRUE, FALSE),
    dbnsfp = c(FALSE, FALSE, TRUE),
    sourceGroup = c("1", "2", "3"),
    pivotGroup = c("1", "1", "1"),
    pivotChar = c("|", "|", "|"),
    parseGroup = c("1", "2", "1"),
    transformation = c(NA, "min", "max")
  )

  expect_error(.get_list_from_config(test_df, "some_list", "SNV"), msg)
})

test_that(".get_list_from_config() gives error for bad type argument", {
  msg <- 'list_type must be one of: "SNV", "indel", "dbnsfp", or "all"'
  test_df <- tibble::tibble(
    field = c("Header 1", "Header 2", "Header 3"),
    SNV = c(TRUE, FALSE, FALSE),
    indel = c(FALSE, TRUE, FALSE),
    dbnsfp = c(FALSE, FALSE, TRUE),
    sourceGroup = c("1", "2", "3"),
    pivotGroup = c("1", "1", "1"),
    pivotChar = c("|", "|", "|"),
    parseGroup = c("1", "2", "1"),
    transformation = c(NA, "min", "max")
  )

  expect_error(.get_list_from_config(test_df, "max", "bad_type"), msg)
})

test_that(".get_list_from_config() returns expected 'desired' list for all", {
  test_config <- tibble::tibble(
    field = c("Header 1", "Header 2", "Header 3"),
    SNV = c(TRUE, FALSE, FALSE),
    indel = c(FALSE, TRUE, FALSE),
    dbnsfp = c(FALSE, FALSE, TRUE),
    sourceGroup = c("1", "2", "3"),
    pivotGroup = c("1", "1", "1"),
    pivotChar = c("|", "|", "|"),
    parseGroup = c("1", "2", "1"),
    transformation = c(NA, "min", "max")
  )

  target <- list("Header 1", "Header 2", "Header 3")
  result <- .get_list_from_config(test_config, "desired", "all")
  expect_identical(result, target)
})

test_that(".get_list_from_config() returns expected 'desired' list for SNV", {
  test_config <- tibble::tibble(
    field = c("Header 1", "Header 2", "Header 3", "Header 4"),
    SNV = c(TRUE, FALSE, FALSE, TRUE),
    indel = c(FALSE, TRUE, FALSE, TRUE),
    dbnsfp = c(FALSE, FALSE, TRUE, TRUE),
    sourceGroup = c("1", "2", "3", "4"),
    pivotGroup = c("1", "1", "1", "1"),
    pivotChar = c("|", "|", "|", "|"),
    parseGroup = c(NA, "2", "1", "1"),
    transformation = c(NA, "min", "max", NA)
  )

  target <- list("Header 1", "Header 4")
  result <- .get_list_from_config(test_config, "desired", "SNV")
  expect_identical(result, target)
})

test_that(".get_list_from_config() returns expected 'desired' list for indel", {
  test_config <- tibble::tibble(
    field = c("Header 1", "Header 2", "Header 3"),
    SNV = c(TRUE, FALSE, FALSE),
    indel = c(FALSE, TRUE, FALSE),
    dbnsfp = c(FALSE, FALSE, TRUE),
    sourceGroup = c("1", "2", "3"),
    pivotGroup = c("1", "1", "1"),
    pivotChar = c("|", "|", "|"),
    parseGroup = c("1", "2", "1"),
    transformation = c(NA, "min", "max")
  )

  target <- list(field = "Header 2")
  result <- .get_list_from_config(test_config, "desired", "indel")
  expect_identical(result, target)
})

test_that(".get_list_from_config() returns expected 'desired' dbnsfp list", {
  test_config <- tibble::tibble(
    field = c("Header 1", "Header 2", "Header 3"),
    SNV = c(TRUE, FALSE, FALSE),
    indel = c(FALSE, TRUE, FALSE),
    dbnsfp = c(FALSE, FALSE, TRUE),
    sourceGroup = c("1", "2", "3"),
    pivotGroup = c("1", "1", "1"),
    pivotChar = c("|", "|", "|"),
    parseGroup = c("1", "2", "1"),
    transformation = c(NA, "min", "max")
  )

  target <- list(field = "Header 3")
  result <- .get_list_from_config(test_config, "desired", "dbnsfp")
  expect_identical(result, target)
})

test_that(".get_list_from_config() returns expected 'max' list for SNV", {
  test_config <- tibble::tibble(
    field = c("Header 1", "Header 2", "Header 3"),
    SNV = c(TRUE, FALSE, TRUE),
    indel = c(FALSE, TRUE, FALSE),
    dbnsfp = c(FALSE, FALSE, TRUE),
    sourceGroup = c("1", "2", "3"),
    pivotGroup = c("1", "1", "1"),
    pivotChar = c("|", "|", "|"),
    parseGroup = c(NA, "2", NA),
    transformation = c(NA, "min", "max")
  )

  target <- list(field = "Header 3")
  result <- .get_list_from_config(test_config, "max", "SNV")
  expect_identical(result, target)
})

test_that(".get_list_from_config() returns expected 'min' list for SNV", {
  test_config <- tibble::tibble(
    field = c("Header 1", "Header 2", "Header 3"),
    SNV = c(TRUE, FALSE, FALSE),
    indel = c(FALSE, TRUE, FALSE),
    dbnsfp = c(FALSE, FALSE, TRUE),
    sourceGroup = c("1", "2", "3"),
    pivotGroup = c("1", "1", "1"),
    pivotChar = c("|", "|", "|"),
    parseGroup = c(NA, "2", "1"),
    transformation = c(NA, "min", "max")
  )

  target <- list()
  result <- .get_list_from_config(test_config, "min", "SNV")
  expect_identical(result, target)
})

test_that(".get_list_from_config() returns expected 'min' list for indel", {
  test_config <- tibble::tibble(
    field = c("Header 1", "Header 2", "Header 3"),
    SNV = c(TRUE, FALSE, FALSE),
    indel = c(FALSE, TRUE, FALSE),
    dbnsfp = c(FALSE, FALSE, TRUE),
    sourceGroup = c("1", "2", "3"),
    pivotGroup = c("1", "1", "1"),
    pivotChar = c("|", "|", "|"),
    parseGroup = c("1", NA, "1"),
    transformation = c(NA, "min", "max")
  )

  target <- list(field = "Header 2")
  result <- .get_list_from_config(test_config, "min", "indel")
  expect_identical(result, target)
})

test_that(".get_list_from_config() returns expected 'pick_Y' list for SNV", {
  test_config <- tibble::tibble(
    field = c("Header 1", "Header 2", "Header 3"),
    SNV = c(TRUE, FALSE, FALSE),
    indel = c(FALSE, TRUE, FALSE),
    dbnsfp = c(FALSE, FALSE, TRUE),
    sourceGroup = c("1", "2", "3"),
    pivotGroup = c("1", "1", "1"),
    pivotChar = c("|", "|", "|"),
    parseGroup = c(NA, NA, "1"),
    transformation = c("pick_Y", "min", "max")
  )

  target <- list(field = "Header 1")
  result <- .get_list_from_config(test_config, "pick_Y", "SNV")
  expect_identical(result, target)
})

test_that(".get_list_from_config() returns expected 'pick_N' list for SNV", {
  test_config <- tibble::tibble(
    field = c("Header 1", "Header 2", "Header 3"),
    SNV = c(TRUE, FALSE, FALSE),
    indel = c(FALSE, TRUE, FALSE),
    dbnsfp = c(FALSE, FALSE, TRUE),
    sourceGroup = c("1", "2", "3"),
    pivotGroup = c("1", "1", "1"),
    pivotChar = c("|", "|", "|"),
    parseGroup = c(NA, NA, "1"),
    transformation = c("pick_N", "min", "max")
  )

  target <- list(field = "Header 1")
  result <- .get_list_from_config(test_config, "pick_N", "SNV")
  expect_identical(result, target)
})

test_that(".get_list_from_config() returns expected 'pick_A' list for SNV", {
  test_config <- tibble::tibble(
    field = c("Header 1", "Header 2", "Header 3"),
    SNV = c(TRUE, FALSE, FALSE),
    indel = c(FALSE, TRUE, FALSE),
    dbnsfp = c(FALSE, FALSE, TRUE),
    sourceGroup = c("1", "2", "3"),
    pivotGroup = c("1", "1", "1"),
    pivotChar = c("|", "|", "|"),
    parseGroup = c(NA, NA, "1"),
    transformation = c("pick_A", "min", "max")
  )

  target <- list(field = "Header 1")
  result <- .get_list_from_config(test_config, "pick_A", "SNV")
  expect_identical(result, target)
})

test_that(".get_list_from_config() returns expected 'clean' list for SNV", {
  test_config <- tibble::tibble(
    field = c("Header 1", "Header 2", "Header 3"),
    SNV = c(TRUE, FALSE, FALSE),
    indel = c(FALSE, TRUE, FALSE),
    dbnsfp = c(FALSE, FALSE, TRUE),
    sourceGroup = c("1", "2", "3"),
    pivotGroup = c("1", "1", "1"),
    pivotChar = c("|", "|", "|"),
    parseGroup = c(NA, NA, "1"),
    transformation = c("clean", "min", "max")
  )

  target <- list(field = "Header 1")
  result <- .get_list_from_config(test_config, "clean", "SNV")
  expect_identical(result, target)
})

test_that(".get_list_from_config() returns expected 'distinct' list for SNV", {
  test_config <- tibble::tibble(
    field = c("Header 1", "Header 2", "Header 3"),
    SNV = c(TRUE, FALSE, FALSE),
    indel = c(FALSE, TRUE, FALSE),
    dbnsfp = c(FALSE, FALSE, TRUE),
    sourceGroup = c("1", "2", "3"),
    pivotGroup = c("1", "1", "1"),
    pivotChar = c("|", "|", "|"),
    parseGroup = c(NA, NA, "1"),
    transformation = c("distinct", "min", "max")
  )

  target <- list(field = "Header 1")
  result <- .get_list_from_config(test_config, "distinct", "SNV")
  expect_identical(result, target)
})

test_that(".get_list_from_config() returns expected 'pivots' list for SNV", {
  test_config <- tibble::tibble(
    field = c("Header 1", "Header 2", "Header 3", "Header 4"),
    SNV = c(TRUE, TRUE, TRUE, TRUE),
    indel = c(FALSE, TRUE, FALSE, FALSE),
    dbnsfp = c(FALSE, FALSE, TRUE, FALSE),
    sourceGroup = c("1", "2", "3", "4"),
    pivotGroup = c("1", "1", "2", "2"),
    pivotChar = c("|", "|", ";", ";"),
    parseGroup = c("1", NA, "1", NA),
    transformation = c(NA, "min", "max", NA)
  )

  target <- list(
    "1" = tibble::tibble(field = c("Header 1", "Header 2"),
                        pivotChar = c("|", "|")
    ),
    "2" = tibble::tibble(field = c("Header 3", "Header 4"),
                        pivotChar = c(";", ";")
    )
  )

  result <- .get_list_from_config(test_config, "pivots", "SNV")
  expect_identical(result, target)
})

test_that(".get_list_from_config() returns expected 'pivots' tibble for SNV", {
  test_config <- tibble::tibble(
    field = c("Header 1", "Header 2", "Header 3"),
    SNV = c(TRUE, TRUE, FALSE),
    indel = c(FALSE, TRUE, FALSE),
    dbnsfp = c(FALSE, FALSE, TRUE),
    sourceGroup = c("1", "2", "3"),
    pivotGroup = c("1", "1", "1"),
    pivotChar = c("|", "|", "|"),
    parseGroup = c("1", NA, "1"),
    transformation = c("clean", "min", NA)
  )

  target <- list(
    "1" = tibble::tibble(field = c("Header 1", "Header 2"),
                        pivotChar = c("|", "|")
    )
  )
  result <- .get_list_from_config(test_config, "pivots", "SNV")
  expect_identical(result, target)
})

test_that(".get_list_from_config() returns expected 'max_pairs' list for SNV", {
  test_config <- tibble::tibble(
    field = c("Header 1", "Header 2", "Header 3"),
    SNV = c(TRUE, TRUE, FALSE),
    indel = c(FALSE, TRUE, FALSE),
    dbnsfp = c(FALSE, FALSE, TRUE),
    sourceGroup = c("1", "2", "3"),
    pivotGroup = c("1", "1", "1"),
    pivotChar = c("|", "|", "|"),
    parseGroup = c("1", "1", "2"),
    transformation = c(NA, "max", "max")
  )

  target <- list("1" = c("Header 2", "Header 1"))
  result <- .get_list_from_config(test_config, "max_pairs", "SNV")
  expect_identical(result, target)
})

test_that(
  ".get_list_from_config() returns expected 'max_pairs' list for SNV", {
    test_config <- tibble::tibble(
      field = c("Header 1", "Header 2", "Header 3", "Header 4"),
      SNV = c(TRUE, TRUE, TRUE, TRUE),
      indel = c(FALSE, TRUE, FALSE, FALSE),
      dbnsfp = c(FALSE, FALSE, TRUE, TRUE),
      sourceGroup = c("1", "2", "3", "4"),
      pivotGroup = c("1", "1", "2", "2"),
      pivotChar = c("|", "|", ";", ";"),
      parseGroup = c("1", "1", "2", "2"),
      transformation = c("max", NA, "max", NA)
    )

    target <- list("1" = c("Header 1", "Header 2"),
                   "2" = c("Header 3", "Header 4"))
    result <- .get_list_from_config(test_config, "max_pairs", "SNV")
    expect_identical(result, target)
  })

test_that(".get_list_from_config() returns expected 'min_pairs' list for SNV", {
  test_config <- tibble::tibble(
    field = c("Header 1", "Header 2", "Header 3"),
    SNV = c(TRUE, TRUE, FALSE),
    indel = c(FALSE, TRUE, FALSE),
    dbnsfp = c(FALSE, FALSE, TRUE),
    sourceGroup = c("1", "2", "3"),
    pivotGroup = c("1", "1", "1"),
    pivotChar = c("|", "|", "|"),
    parseGroup = c("1", "1", "2"),
    transformation = c(NA, "min", "max")
  )

  target <- list("1" = c("Header 2", "Header 1"))
  result <- .get_list_from_config(test_config, "min_pairs", "SNV")
  expect_identical(result, target)
})

test_that(
  ".get_list_from_config() returns expected 'pick_Y_pairs' list for SNV", {
  test_config <- tibble::tibble(
    field = c("Header 1", "Header 2", "Header 3"),
    SNV = c(TRUE, TRUE, FALSE),
    indel = c(FALSE, TRUE, FALSE),
    dbnsfp = c(FALSE, FALSE, TRUE),
    sourceGroup = c("1", "2", "3"),
    pivotGroup = c("1", "1", "1"),
    pivotChar = c("|", "|", "|"),
    parseGroup = c("1", "1", "2"),
    transformation = c(NA, "pick_Y", "max")
  )

  target <- list("1" = c("Header 2", "Header 1"))
  result <- .get_list_from_config(test_config, "pick_Y_pairs", "SNV")
  expect_identical(result, target)
})

test_that(
  ".get_list_from_config() returns expected 'pick_N_pairs' list for SNV", {
    test_config <- tibble::tibble(
      field = c("Header 1", "Header 2", "Header 3"),
      SNV = c(TRUE, TRUE, FALSE),
      indel = c(FALSE, TRUE, FALSE),
      dbnsfp = c(FALSE, FALSE, TRUE),
      sourceGroup = c("1", "2", "3"),
      pivotGroup = c("1", "1", "1"),
      pivotChar = c("|", "|", "|"),
      parseGroup = c("1", "1", "2"),
      transformation = c(NA, "pick_N", "max")
    )

    target <- list("1" = c("Header 2", "Header 1"))
    result <- .get_list_from_config(test_config, "pick_N_pairs", "SNV")
    expect_identical(result, target)
  })

test_that(
  ".get_list_from_config() returns expected 'pick_A_pairs' list for SNV", {
    test_config <- tibble::tibble(
      field = c("Header 1", "Header 2", "Header 3"),
      SNV = c(TRUE, TRUE, FALSE),
      indel = c(FALSE, TRUE, FALSE),
      dbnsfp = c(FALSE, FALSE, TRUE),
      sourceGroup = c("1", "2", "3"),
      pivotGroup = c("1", "1", "1"),
      pivotChar = c("|", "|", "|"),
      parseGroup = c("1", "1", "2"),
      transformation = c(NA, "pick_A", "max")
    )

    target <- list("1" = c("Header 2", "Header 1"))
    result <- .get_list_from_config(test_config, "pick_A_pairs", "SNV")
    expect_identical(result, target)
  })
