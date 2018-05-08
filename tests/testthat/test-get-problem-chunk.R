context("test_get_problem_chunk - unit tests")

test_that(
  "get_problem_chunk returns the expected first chunk", {
    load(file = "loaded_chunk.RDa")
    stop(dplyr::glimpse(
      get_problem_chunk(
        source_file = "1k_annotation.gz",
        chunk_size = 5,
        problem_index = 1)
    ))
    expect_equivalent(loaded_chunk,
                     get_problem_chunk(
                       source_file = "1k_annotation.gz",
                       chunk_size = 5,
                       problem_index = 1))
  }
)

test_that(
  "get_problem_chunk returns the expected second chunk", {
    load(file = "loaded_chunk_2.RDa")
    expect_equivalent(loaded_chunk_2,
                     get_problem_chunk("1k_annotation.gz", 5, 2))
  }
)
