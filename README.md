# wgsaparsr

This package is the code the [TOPMED DCC](https://www.nhlbiwgs.org/group/dcc)
uses to parse genetic variant annotation files produced by
the [WGSA annotation tool](https://sites.google.com/site/jpopgen/wgsa)

## Installation

You can install wgsaparsr from github with:

```R
# install.packages("devtools") 
devtools::install_github("UW-GAC/wgsaparsr")
```

## Example

```R 
# list all fields in an annotation file: 
all_fields <- get_fields("WGSA_chr_1.gz")

# parse a freeze 4 general annotation file for SNVs and dbnsfp annotations:
parse_to_file(source_file = WGSA_file,
              destination = "chr_22_snv_test.tsv",
              dbnsfp_destination = "chr_22_dbnsfp_test.tsv",
              freeze = 4,
              chunk_size = 10000,
              verbose = TRUE)

# parse a freeze 4 general annotation file for indel annotations:
parse_to_file(source_file = indel_file,
              destination = "chr_22_indel_test.tsv",
              freeze = 4,
              chunk_size = 10000,
              verbose = TRUE)
```

