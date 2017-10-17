context("test_.get_header - unit tests")

test_that(
  ".get_header returns header line from a chunk", {
    test_chunk <- c(paste("#chr", "pos", "ref", "alt", sep = "\t"),
                    "foo",
                    "bar",
                    "another_line")
    expected_result <- paste("#chr", "pos", "ref", "alt", sep = "\t")
    expect_identical(.get_header(test_chunk), expected_result)
  }
)
