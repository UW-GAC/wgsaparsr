[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3352386.svg)](https://doi.org/10.5281/zenodo.3352386)

| **Service** | **Master** | **Develop** |
|:-------------:|:------:|:-------:|
| CI Status | [![Travis-CI Build Status](https://travis-ci.org/UW-GAC/wgsaparsr.svg?branch=master)](https://travis-ci.org/UW-GAC/wgsaparsr) | [![Travis-CI Build Status](https://travis-ci.org/UW-GAC/wgsaparsr.svg?branch=develop)](https://travis-ci.org/UW-GAC/wgsaparsr?branch=develop) |
| Test Coverage | [![Coverage Status](https://img.shields.io/codecov/c/github/UW-GAC/wgsaparsr/master.svg)](https://codecov.io/github/UW-GAC/wgsaparsr/branch/master) | [![Coverage Status](https://img.shields.io/codecov/c/github/UW-GAC/wgsaparsr/develop.svg)](https://codecov.io/github/UW-GAC/wgsaparsr/branch/develop) |

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

# load a configuration file
local_config <- load_config("config.tsv")

# parse WGSA output file tsv output files 
# (one for dbnsfp annotations, one for snv/indel annotaitons)
parse_to_file(source_file = snv_source_file,
  destination = snv_destination,
  dbnsfp_destination = dbnsfp_destination,
  config = config,
  freeze = 5,
  chunk_size = 1000,
  verbose = TRUE)
```

Expanded configuration file documentation coming soon. In the meantime, see 
`?wgsaparsr::load_config()`
