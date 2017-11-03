context("test_.expand_chunk - unit tests")

test_that(".expand_chunk returns expected tibble - snp", {
  example_snp <- dplyr::tibble(
    "VEP_ensembl_Consequence" = c("a|b"),
    "VEP_ensembl_Transcript_ID" = c("a|b"),
    "VEP_ensembl_Gene_Name" = c("a|b"),
    "VEP_ensembl_Gene_ID" = c("a|b"),
    "VEP_ensembl_Protein_ID" = c("a|b"),
    "VEP_ensembl_CCDS" = c("a|b"),
    "VEP_ensembl_SWISSPROT" = c("a|b"),
    "VEP_ensembl_Codon_Change_or_Distance" = c("a|b"),
    "VEP_ensembl_Amino_Acid_Change" = c("a|b"),
    "VEP_ensembl_HGVSc" = c("a|b"),
    "VEP_ensembl_HGVSp" = c("a|b"),
    "VEP_ensembl_cDNA_position" = c("a|b"),
    "VEP_ensembl_CDS_position" = c("a|b"),
    "VEP_ensembl_Protein_position" = c("a|b"),
    "VEP_ensembl_Exon_or_Intron_Rank" = c("a|b"),
    "VEP_ensembl_STRAND" = c("a|b"),
    "VEP_ensembl_CANONICAL" = c("a|b"),
    "VEP_ensembl_LoF" = c("a|b"),
    "VEP_ensembl_LoF_filter" = c("a|b"),
    "VEP_ensembl_LoF_flags" = c("a|b"),
    "VEP_ensembl_LoF_info" = c("a|b"),
    "SIFT4G_AAref" = c("a|b"), # for SNV file, to pivot
    "SIFT4G_AAalt" = c("a|b"), # for SNV file, to pivot
    "SIFT4G_AApos" = c("a|b"), # for SNV file, to pivot
    "SIFT4G_score" = c("a|b"), # for SNV file, to pivot
    "SIFT4G_pred" = c("a|b"),

    "GTEx_V6_gene" = c("1|2"),
    "GTEx_V6_tissue" = c("1|2"),

    "Ensembl_Regulatory_Build_Overviews" = c("3;4"),

    "Ensembl_Regulatory_Build_TFBS" = c("5;6")
  )

  target <- dplyr::tibble(
    "VEP_ensembl_Consequence" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_Transcript_ID" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_Gene_Name" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_Gene_ID" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_Protein_ID" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_CCDS" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_SWISSPROT" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_Codon_Change_or_Distance" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_Amino_Acid_Change" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_HGVSc" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_HGVSp" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_cDNA_position" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_CDS_position" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_Protein_position" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_Exon_or_Intron_Rank" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_STRAND" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_CANONICAL" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_LoF" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_LoF_filter" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_LoF_flags" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_LoF_info" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "SIFT4G_AAref" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "SIFT4G_AAalt" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "SIFT4G_AApos" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "SIFT4G_score" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "SIFT4G_pred" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),

    "Ensembl_Regulatory_Build_Overviews" =
      c("3", "3", "3", "3", "4", "4", "4", "4", "3", "3", "3", "3", "4", "4",
        "4", "4"),

    "Ensembl_Regulatory_Build_TFBS" =
      c("5", "5", "6", "6", "5", "5", "6", "6", "5", "5", "6", "6", "5", "5",
        "6", "6"),

    "GTEx_V6_gene" =
      c("1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2",
        "1", "2"),
    "GTEx_V6_tissue" =
      c("1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2",
        "1", "2")
  )
  result <- .expand_chunk(example_snp, freeze = 4)
  expect_identical(result, target)
})

test_that(".expand_chunk returns expected tibble - dbnsfp", {
  example_dbnsfp <- dplyr::tibble(
    "aaref" = c("a|b"),
    "aaalt" = c("a|b"),
    "Uniprot_acc" = c("a|b"),
    "Uniprot_id" = c("a|b"),
    "Uniprot_aapos" = c("a|b"),
    "Interpro_domain" = c("a|b"),
    "cds_strand" = c("a|b"),
    "refcodon" = c("a|b"),
    "SLR_test_statistic" = c("a|b"),
    "codonpos" = c("a|b"),
    "fold_degenerate" = c("a|b"),
    "Ensembl_geneid" = c("a|b;c"),
    "aapos" = c("a|b"),
    "Polyphen2_HDIV_score" = c("a|b"),
    "Polyphen2_HDIV_rankscore" = c("a|b"),
    "Polyphen2_HDIV_pred" = c("a|b"),
    "Polyphen2_HVAR_score" = c("a|b"),
    "Polyphen2_HVAR_rankscore" = c("a|b"),
    "Polyphen2_HVAR_pred" = c("a|b"),
    "LRT_score" = c("a|b"),
    "LRT_converted_rankscore" = c("a|b"),
    "LRT_pred" = c("a|b"),
    "MutationTaster_score" = c("a|b"),
    "MutationTaster_converted_rankscore" = c("a|b"),
    "MutationTaster_pred" = c("a|b"),
    "MutationAssessor_score" = c("a|b"),
    "MutationAssessor_rankscore" = c("a|b"),
    "MutationAssessor_pred" = c("a|b"),
    "FATHMM_score" = c("a|b"),
    "FATHMM_rankscore" = c("a|b"),
    "FATHMM_pred" = c("a|b"),
    "MetaSVM_score" = c("a|b"),
    "MetaSVM_rankscore" = c("a|b"),
    "MetaSVM_pred" = c("a|b"),
    "MetaLR_score" = c("a|b"),
    "MetaLR_rankscore" = c("a|b"),
    "MetaLR_pred" = c("a|b"),
    "Reliability_index" = c("a|b"),
    "VEST3_score" = c("a|b"),
    "VEST3_rankscore" = c("a|b"),
    "PROVEAN_score" = c("a|b"),
    "PROVEAN_converted_rankscore" = c("a|b"),
    "PROVEAN_pred" = c("a|b")
  )
  target <- dplyr::tibble(
    "aaref" = c("a", "b", "b"),
    "aaalt" = c("a", "b", "b"),
    "Uniprot_acc" = c("a", "b", "b"),
    "Uniprot_id" = c("a", "b", "b"),
    "Uniprot_aapos" = c("a", "b", "b"),
    "Interpro_domain" = c("a", "b", "b"),
    "cds_strand" = c("a", "b", "b"),
    "refcodon" = c("a", "b", "b"),
    "SLR_test_statistic" = c("a", "b", "b"),
    "codonpos" = c("a", "b", "b"),
    "fold_degenerate" = c("a", "b", "b"),
    "aapos" = c("a", "b", "b"),
    "Polyphen2_HDIV_score" = c("a", "b", "b"),
    "Polyphen2_HDIV_rankscore" = c("a", "b", "b"),
    "Polyphen2_HDIV_pred" = c("a", "b", "b"),
    "Polyphen2_HVAR_score" = c("a", "b", "b"),
    "Polyphen2_HVAR_rankscore" = c("a", "b", "b"),
    "Polyphen2_HVAR_pred" = c("a", "b", "b"),
    "LRT_score" = c("a", "b", "b"),
    "LRT_converted_rankscore" = c("a", "b", "b"),
    "LRT_pred" = c("a", "b", "b"),
    "MutationTaster_score" = c("a", "b", "b"),
    "MutationTaster_converted_rankscore" = c("a", "b", "b"),
    "MutationTaster_pred" = c("a", "b", "b"),
    "MutationAssessor_score" = c("a", "b", "b"),
    "MutationAssessor_rankscore" = c("a", "b", "b"),
    "MutationAssessor_pred" = c("a", "b", "b"),
    "FATHMM_score" = c("a", "b", "b"),
    "FATHMM_rankscore" = c("a", "b", "b"),
    "FATHMM_pred" = c("a", "b", "b"),
    "MetaSVM_score" = c("a", "b", "b"),
    "MetaSVM_rankscore" = c("a", "b", "b"),
    "MetaSVM_pred" = c("a", "b", "b"),
    "MetaLR_score" = c("a", "b", "b"),
    "MetaLR_rankscore" = c("a", "b", "b"),
    "MetaLR_pred" = c("a", "b", "b"),
    "Reliability_index" = c("a", "b", "b"),
    "VEST3_score" = c("a", "b", "b"),
    "VEST3_rankscore" = c("a", "b", "b"),
    "PROVEAN_score" = c("a", "b", "b"),
    "PROVEAN_converted_rankscore" = c("a", "b", "b"),
    "PROVEAN_pred" = c("a", "b", "b"),
    "Ensembl_geneid" = c("a", "b", "c")
  )
  result <- .expand_chunk(example_dbnsfp, freeze = 4, dbnsfp_flag = TRUE)
  expect_identical(result, target)
})

test_that(".expand_chunk returns expected tibble - indel", {
  example_indel <- dplyr::tibble(
    "VEP_ensembl_Consequence" = c("a|b"),
    "VEP_ensembl_Transcript_ID" = c("a|b"),
    "VEP_ensembl_Gene_Name" = c("a|b"),
    "VEP_ensembl_Gene_ID" = c("a|b"),
    "VEP_ensembl_Protein_ID" = c("a|b"),
    "VEP_ensembl_CCDS" = c("a|b"),
    "VEP_ensembl_SWISSPROT" = c("a|b"),
    "VEP_ensembl_Codon_Change_or_Distance" = c("a|b"),
    "VEP_ensembl_Amino_Acid_Change" = c("a|b"),
    "VEP_ensembl_HGVSc" = c("a|b"),
    "VEP_ensembl_HGVSp" = c("a|b"),
    "VEP_ensembl_cDNA_position" = c("a|b"),
    "VEP_ensembl_CDS_position" = c("a|b"),
    "VEP_ensembl_Protein_position" = c("a|b"),
    "VEP_ensembl_Exon_or_Intron_Rank" = c("a|b"),
    "VEP_ensembl_STRAND" = c("a|b"),
    "VEP_ensembl_CANONICAL" = c("a|b"),
    "VEP_ensembl_LoF" = c("a|b"),
    "VEP_ensembl_LoF_filter" = c("a|b"),
    "VEP_ensembl_LoF_flags" = c("a|b"),
    "VEP_ensembl_LoF_info" = c("a|b"),

    "GTEx_V6_gene" = c("1|2"),
    "GTEx_V6_tissue" = c("1|2"),

    "Ensembl_Regulatory_Build_Overviews" = c("3;4"),

    "Ensembl_Regulatory_Build_TFBS" = c("5;6")
  )

  target <- dplyr::tibble(
    "VEP_ensembl_Consequence" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_Transcript_ID" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_Gene_Name" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_Gene_ID" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_Protein_ID" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_CCDS" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_SWISSPROT" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_Codon_Change_or_Distance" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_Amino_Acid_Change" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_HGVSc" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_HGVSp" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_cDNA_position" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_CDS_position" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_Protein_position" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_Exon_or_Intron_Rank" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_STRAND" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_CANONICAL" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_LoF" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_LoF_filter" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_LoF_flags" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),
    "VEP_ensembl_LoF_info" =
      c("a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b",
        "b", "b"),

    "Ensembl_Regulatory_Build_Overviews" =
      c("3", "3", "3", "3", "4", "4", "4", "4", "3", "3", "3", "3", "4", "4",
        "4", "4"),

    "Ensembl_Regulatory_Build_TFBS" =
      c("5", "5", "6", "6", "5", "5", "6", "6", "5", "5", "6", "6", "5", "5",
        "6", "6"),

    "GTEx_V6_gene" =
      c("1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2",
        "1", "2"),
    "GTEx_V6_tissue" =
      c("1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2", "1", "2",
        "1", "2")
  )
  result <- .expand_chunk(example_indel, freeze = 4, indel_flag = TRUE)
  expect_identical(result, target)
})
