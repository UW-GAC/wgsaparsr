context("test_.write_output_header - unit tests")

test_that(".write_output_header() writes expected indel header", {
  indel_tmp <- tempfile()
  on.exit(unlink(indel_tmp))

  config <-
    dplyr::tibble(
      field = c("Header 1", "Header 3"),
      SNV = c(TRUE, FALSE),
      indel = c(TRUE, TRUE),
      dbnsfp = c(FALSE, FALSE),
      pivotGroup = c(NA, "1"),
      pivotChar = c("|", ";"),
      parseGroup = c("1", "2"),
      transformation = c(NA, "min"),
      sourceGroup = c("1", "2"),
      toRemove = c("^\\.$", "^NULL$")
    )
  .write_output_header(config,
                       destination = indel_tmp,
                       dbnsfp_destination = NA,
                       indel_flag = TRUE)
  written <- readr::read_lines(indel_tmp, progress = FALSE)
  expect_equal(written, "Header 1\tHeader 3")
})


test_that(".write_output_header() writes expected snv header", {
  snv_tmp <- tempfile()
  on.exit(unlink(snv_tmp))

  config <-
    dplyr::tibble(
      field = c("Header 1", "Header 3"),
      SNV = c(TRUE, FALSE),
      indel = c(TRUE, TRUE),
      dbnsfp = c(FALSE, FALSE),
      pivotGroup = c(NA, "1"),
      pivotChar = c("|", ";"),
      parseGroup = c("1", "2"),
      transformation = c(NA, "min"),
      sourceGroup = c("1", "2"),
      toRemove = c("^\\.$", "^NULL$")
    )
  .write_output_header(config,
                       destination = snv_tmp,
                       dbnsfp_destination = NA,
                       indel_flag = FALSE)
  written <- readr::read_lines(snv_tmp, progress = FALSE)
  expect_equal(written, "Header 1")
})

test_that(".write_output_header() writes expected dbnsfp and snv header", {
  snv_tmp <- tempfile()
  on.exit(unlink(snv_tmp))

  dbnsfp_tmp <- tempfile()
  on.exit(unlink(dbnsfp_tmp))

  config <-
    dplyr::tibble(
      field = c("Header 1", "Header 3"),
      SNV = c(TRUE, FALSE),
      indel = c(TRUE, TRUE),
      dbnsfp = c(FALSE, TRUE),
      pivotGroup = c(NA, "1"),
      pivotChar = c("|", ";"),
      parseGroup = c("1", "2"),
      transformation = c(NA, "min"),
      sourceGroup = c("1", "2"),
      toRemove = c("^\\.$", "^NULL$")
    )
  .write_output_header(config,
                       destination = snv_tmp,
                       dbnsfp_destination = dbnsfp_tmp,
                       indel_flag = FALSE)
  written <- readr::read_lines(dbnsfp_tmp, progress = FALSE)
  expect_equal(written, "Header 3")
})
