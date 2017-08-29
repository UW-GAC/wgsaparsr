context("test_parse_to_file - unit tests")

test_that(
  "parse_to_file returns an error if ", {
    target_columns <- c("fake_field", "pos")
    columns_to_split <- "VEP_ensembl_Transcript_ID"
    error_msg <- paste0("to_split columns missing in desired_columns: ",
                        "VEP_ensembl_Transcript_ID", sep = "")
    expect_error(parse_to_file("1k_annotation.gz",
                               destination = "parsed_chr_1.csv",
                               desired_columns = target_columns,
                               to_split = columns_to_split,
                               chunk_size = 1000),
                 error_msg)
  }
)
