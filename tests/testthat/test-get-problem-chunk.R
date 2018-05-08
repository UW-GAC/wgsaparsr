context("test_get_problem_chunk - unit tests")

test_that(
  "get_problem_chunk returns the expected first chunk", {
    expected_chunk <- load("loaded_chunk.RDa")
    expect_identical(loaded_chunk,
                     suppressMessages(
                       get_problem_chunk("1k_annotation.gz", 5, 1)))
  }
)

test_that(
  "get_problem_chunk returns the expected second chunk", {
    expected_chunk <- load("loaded_chunk_2.RDa")
    expect_identical(loaded_chunk_2,
                     suppressMessages(
                       get_problem_chunk("1k_annotation.gz", 5, 2)))
  }
)
