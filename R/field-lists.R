#' define lists of fields in one place, and return based on argument
.get_list <- function(which_list) {
  if (which_list == "old_names") {
    old_names <- c(
      "`#chr`",
      "`MAP20(+-149bp)`",
      "`MAP35(+-149bp)`",
      "`GMS_single-end`",
      "`GMS_paired-end`",
      "`H1-hESC_fitCons_score`", #nolint
      "`H1-hESC_fitCons_rankscore`", #nolint
      "`H1-hESC_confidence_value`", #nolint
      "`1000G_strict_masked`",
      "`1000Gp3_AC`",
      "`1000Gp3_AF`",
      "`1000Gp3_AFR_AC`",
      "`1000Gp3_AFR_AF`",
      "`1000Gp3_EUR_AC`",
      "`1000Gp3_EUR_AF`",
      "`1000Gp3_AMR_AC`",
      "`1000Gp3_AMR_AF`",
      "`1000Gp3_EAS_AC`",
      "`1000Gp3_EAS_AF`",
      "`1000Gp3_SAS_AC`",
      "`1000Gp3_SAS_AF`",
      "`fathmm-MKL_non-coding_score`",
      "`fathmm-MKL_non-coding_rankscore`", #nolint
      "`fathmm-MKL_non-coding_group`",
      "`fathmm-MKL_coding_score`",
      "`fathmm-MKL_coding_rankscore`",
      "`fathmm-MKL_coding_pred`",
      "`fathmm-MKL_coding_group`",
      "`Eigen-raw`",
      "`Eigen-phred`",
      "`Eigen-raw_rankscore`",
      "`Eigen-PC-raw`",
      "`Eigen-PC-raw_rankscore`"
    )
    return(old_names)
  } else if (which_list == "new_names") {
    new_names <- c(
      "chr",
      "MAP20_149bp",
      "MAP35_149bp",
      "GMS_single_end",
      "GMS_paired_end",
      "H1_hESC_fitCons_score", #nolint
      "H1_hESC_fitCons_rankscore",
      #nolint
      "H1_hESC_confidence_value",
      #nolint
      "KGP_strict_masked",
      "KGP3_AC",
      "KGP3_AF",
      "KGP3_AFR_AC",
      "KGP3_AFR_AF",
      "KGP3_EUR_AC",
      "KGP3_EUR_AF",
      "KGP3_AMR_AC",
      "KGP3_AMR_AF",
      "KGP3_EAS_AC",
      "KGP3_EAS_AF",
      "KGP3_SAS_AC",
      "KGP3_SAS_AF",
      "fathmm_MKL_non_coding_score",
      "fathmm_MKL_non_coding_rankscore",
      #nolint
      "fathmm_MKL_non_coding_group",
      "fathmm_MKL_coding_score",
      "fathmm_MKL_coding_rankscore",
      "fathmm_MKL_coding_pred",
      "fathmm_MKL_coding_group",
      "Eigen_raw",
      "Eigen_phred",
      "Eigen_raw_rankscore",
      "Eigen_PC_raw",
      "Eigen_PC_raw_rankscore"
    )
    return(new_names)
  } else if (which_list == "parseable_fields") {
    parseable_fields <- c(
      "splicing_consensus_ada_score",
      "splicing_consensus_rf_score",
      "MAP20",
      "MAP35",
      "MAP20(+-149bp)",
      "MAP35(+-149bp)",
      "GMS_single-end",
      "GMS_paired-end",
      "1000G_strict_masked",
      "RepeatMasker_masked",
      "phyloP46way_primate",
      "phyloP46way_primate_rankscore",
      "phyloP46way_placental",
      "phyloP46way_placental_rankscore",
      "phyloP100way_vertebrate",
      "phyloP100way_vertebrate_rankscore",
      "phastCons46way_primate",
      "phastCons46way_primate_rankscore",
      "phastCons46way_placental",
      "phastCons46way_placental_rankscore",
      "phastCons100way_vertebrate",
      "phastCons100way_vertebrate_rankscore",
      "GERP_NR",
      "GERP_RS",
      "GERP_RS_rankscore",
      "SiPhy_29way_logOdds",
      "SiPhy_29way_logOdds_rankscore",
      "integrated_fitCons_score",
      "integrated_fitCons_rankscore",
      "integrated_confidence_value",
      "GM12878_fitCons_score",
      "GM12878_fitCons_rankscore",
      "GM12878_confidence_value",
      "H1-hESC_fitCons_score",
      "H1-hESC_fitCons_rankscore",
      "H1-hESC_confidence_value",
      "HUVEC_fitCons_score",
      "HUVEC_fitCons_rankscore",
      "HUVEC_confidence_value",
      "GenoCanyon_score",
      "GenoCanyon_rankscore",
      "DANN_score",
      "DANN_rank_score",
      "fathmm-MKL_non-coding_score",
      "fathmm-MKL_non-coding_rankscore",
      "fathmm-MKL_non-coding_group",
      "fathmm-MKL_coding_score",
      "fathmm-MKL_coding_rankscore",
      "fathmm-MKL_coding_pred",
      "fathmm-MKL_coding_group",
      "Eigen-raw",
      "Eigen-phred",
      "Eigen_coding_or_noncoding",
      "Eigen-raw_rankscore",
      "Eigen-PC-raw",
      "Eigen-PC-raw_rankscore",
      "ENCODE_TFBS_score",
      "ENCODE_Dnase_score",
      "ENCODE_Dnase_cells",
      "EnhancerFinder_general_developmental_enhancer",
      "EnhancerFinder_brain_enhancer",
      "EnhancerFinder_heart_enhancer",
      "EnhancerFinder_limb_enhancer",
      "FANTOM5_enhancer_permissive",
      "FANTOM5_enhancer_robust",
      "FANTOM5_CAGE_peak_permissive",
      "FANTOM5_CAGE_peak_robust"
    )
    return(parseable_fields)
  } else if (which_list == "parse_max") {
    parse_max <- c(
      "splicing_consensus_ada_score",
      "splicing_consensus_rf_score",
      "MAP20",
      "MAP35",
      "MAP20(+-149bp)",
      "MAP35(+-149bp)",
      "GMS_single-end",
      "GMS_paired-end",
      "ENCODE_TFBS_score",
      "ENCODE_Dnase_score",
      "ENCODE_Dnase_cells"
    )
    return(parse_max)
  } else   if (which_list == "parse_triples") {
    parse_triples <- list(
      c(
        "Eigen-raw",
        "Eigen-raw_rankscore",
        "Eigen-phred"
      )
    )
    return(parse_triples)
  } else if (which_list == "parse_pairs") {
    parse_pairs <- list(
      c("phyloP46way_primate",
        "phyloP46way_primate_rankscore"),
      c(
        "phyloP46way_placental",
        "phyloP46way_placental_rankscore"
      ),
      c(
        "phyloP100way_vertebrate",
        "phyloP100way_vertebrate_rankscore"
      ),
      c(
        "phastCons46way_primate",
        "phastCons46way_primate_rankscore"
      ),
      c(
        "phastCons46way_placental",
        "phastCons46way_placental_rankscore"
      ),
      c(
        "phastCons100way_vertebrate",
        "phastCons100way_vertebrate_rankscore"
      ),
      c("SiPhy_29way_logOdds",
        "SiPhy_29way_logOdds_rankscore"),
      c("GenoCanyon_score",
        "GenoCanyon_rankscore"),
      c("DANN_score",
        "DANN_rank_score"),
      c("Eigen-PC-raw",
        "Eigen-PC-raw_rankscore"),
      c("GERP_RS",
        "GERP_RS_rankscore"),
      c(
        "integrated_fitCons_score",
        "integrated_fitCons_rankscore"
      ),
      c(
        "GM12878_fitCons_score",
        "GM12878_fitCons_rankscore"
      ),
      c(
        "H1-hESC_fitCons_score",
        "H1-hESC_fitCons_rankscore"
      ),
      c(
        "HUVEC_fitCons_score",
        "HUVEC_fitCons_rankscore"
      ),
      c(
        "fathmm-MKL_non-coding_score",
        "fathmm-MKL_non-coding_rankscore"
      ),
      c(
        "fathmm-MKL_coding_score",
        "fathmm-MKL_coding_rankscore"
      )
    )
    return(parse_pairs)
  } else if (which_list == "parse_string_yes") {
    parse_string_yes <- c(
      "EnhancerFinder_general_developmental_enhancer",
      "EnhancerFinder_brain_enhancer",
      "EnhancerFinder_heart_enhancer",
      "EnhancerFinder_limb_enhancer",
      "FANTOM5_enhancer_permissive",
      "FANTOM5_enhancer_robust",
      "FANTOM5_CAGE_peak_permissive",
      "FANTOM5_CAGE_peak_robust",
      "RepeatMasker_masked"
    )
    return(parse_string_yes)
  } else if (which_list == "parse_string_no") {
    parse_string_no <- c(
      "1000G_strict_masked"
    )
    return(parse_string_no)
  } else {
    msg <- paste0("which_list must be one of 'old_names', 'new_names', ",
                 "'parseable_fields', 'parse_max', 'parse_pairs', ",
                 "'parse_triples', 'parse_group', 'parse_string_yes', ",
                 "or 'parse_string_no'.")
    stop(msg)
  }
}
