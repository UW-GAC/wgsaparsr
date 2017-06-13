context("test_parse_to_file - unit tests")

test_that(
  "parse_to_file returns an error if ", {
    target_columns <- c("fake_field", "pos")
    columns_to_split <- "VEP_ensembl_Transcript_ID"
    expect_error(parse_to_file("1k_annotation.gz",
                               destination = "parsed_chr_1.csv",
                               desired_columns = target_columns,
                               to_split = columns_to_split,
                               chunk_size = 1000),
                 "Not all desired columns are in source file")
  }
)
