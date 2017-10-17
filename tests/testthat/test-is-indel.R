context("test_.is_indel - unit tests")

test_that(
  ".is_indel returns TRUE when it should", {
    test_header <- paste("foo", "indel_focal_length", "bar", sep = "\t")
    expect_true(.is_indel(test_header))
  }
)

test_that(
  ".is_indel returns FALSE when it should", {
    test_header <- paste("foo", "bar", "baz", sep = "\t")
    expect_false(.is_indel(test_header))
  }
)
