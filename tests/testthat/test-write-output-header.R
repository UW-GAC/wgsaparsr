context("test_.write_output_header - unit tests")

test_that(".write_output_header() writes expected indel header", {
  indel_tmp <- tempfile()
  on.exit(unlink(indel_tmp))

  config <-
    tibble::tibble(
      field = c("Header 1", "Header 3"),
      SNV = c(TRUE, FALSE),
      indel = c(TRUE, TRUE),
      dbnsfp = c(FALSE, FALSE),
      pivotGroup = c("1", "2"),
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
    tibble::tibble(
      field = c("Header 1", "Header 3"),
      SNV = c(TRUE, FALSE),
      indel = c(TRUE, TRUE),
      dbnsfp = c(FALSE, FALSE),
      pivotGroup = c("1", "2"),
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
    tibble::tibble(
      field = c("Header 1", "Header 3"),
      SNV = c(TRUE, FALSE),
      indel = c(TRUE, TRUE),
      dbnsfp = c(FALSE, TRUE),
      pivotGroup = c("1", "2"),
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

test_that(".write_output_header() writes expected renamed header", {
  indel_tmp <- tempfile()
  on.exit(unlink(indel_tmp))

  config <-
    tibble::tibble(
      field = c("Header 1", "Header 3"),
      SNV = c(TRUE, FALSE),
      indel = c(TRUE, TRUE),
      dbnsfp = c(FALSE, FALSE),
      pivotGroup = c("1", "2"),
      pivotChar = c("|", ";"),
      parseGroup = c("1", "2"),
      transformation = c(NA, "min"),
      sourceGroup = c("1", "2"),
      toRemove = c("^\\.$", "^NULL$"),
      ouptutOrder = c(1, 2),
      outputName = c("Header A", "Header B")
    )
  .write_output_header(config,
                       destination = indel_tmp,
                       dbnsfp_destination = NA,
                       indel_flag = TRUE)
  written <- readr::read_lines(indel_tmp, progress = FALSE)
  expect_equal(written, "Header A\tHeader B")
})

test_that(".write_output_header() writes expected renamed reordered header", {
  indel_tmp <- tempfile()
  on.exit(unlink(indel_tmp))

  config <-
    tibble::tibble(
      field = c("Header 1", "Header 3"),
      SNV = c(TRUE, FALSE),
      indel = c(TRUE, TRUE),
      dbnsfp = c(FALSE, FALSE),
      pivotGroup = c("1", "2"),
      pivotChar = c("|", ";"),
      parseGroup = c("1", "2"),
      transformation = c(NA, "min"),
      sourceGroup = c("1", "2"),
      toRemove = c("^\\.$", "^NULL$"),
      outputOrder = c(2, 1),
      outputName = c("Header A", "Header B")
    )
  config <- .clean_config(config) #shouldn't do this in test
  .write_output_header(config,
                       destination = indel_tmp,
                       dbnsfp_destination = NA,
                       indel_flag = TRUE)
  written <- readr::read_lines(indel_tmp, progress = FALSE)
  expect_equal(written, "Header B\tHeader A")
})

# test_that(".write_output_header() writes expected dbnsfp header with no snv", {
#   dbnsfp_tmp <- tempfile()
#   on.exit(unlink(dbnsfp_tmp))
#
#   config <-
#     tibble::tibble(
#       field = c("Header 1", "Header 3"),
#       SNV = c(FALSE, FALSE),
#       indel = c(FALSE, TRUE),
#       dbnsfp = c(FALSE, TRUE),
#       pivotGroup = c(NA, "1"),
#       pivotChar = c("|", ";"),
#       parseGroup = c("1", "2"),
#       transformation = c(NA, "min"),
#       sourceGroup = c("1", "2"),
#       toRemove = c("^\\.$", "^NULL$")
#     )
#   .write_output_header(config,
#                        destination = NA,
#                        dbnsfp_destination = dbnsfp_tmp,
#                        indel_flag = FALSE)
#   written <- readr::read_lines(dbnsfp_tmp, progress = FALSE)
#   expect_equal(written, "Header 3")
# })
