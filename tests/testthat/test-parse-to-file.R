# note
# foo <- .get_list_from_config(config, "desired", "indel")
# write_tsv(as.data.frame(foo), indel_tmp, append = TRUE)
#
# example:
# test_that("write_lines uses UTF-8 encoding", {
#   tmp <- tempfile()
#   on.exit(unlink(tmp))
#   write_lines(c("fran\u00e7ais", "\u00e9l\u00e8ve"), tmp)
#   x <- read_lines(tmp, locale = locale(encoding = "UTF-8"), progress = FALSE)
#   expect_equal(x, c("fran\u00e7ais", "\u00e9l\u00e8ve"))
# })

context("test_parse-to-file - unit tests")
# STUB TODO
