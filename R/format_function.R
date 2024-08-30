
#----------format outcome data-----------------------------------------------------------------
format_exposure_data <- function (filename, clump = FALSE, sep = " ", phenotype_col = "Phenotype",
                                  snp_col = "SNP", beta_col = "beta", se_col = "se", eaf_col = "eaf",
                                  effect_allele_col = "effect_allele", other_allele_col = "other_allele",
                                  pval_col = "pval", units_col = "units", ncase_col = "ncase",
                                  ncontrol_col = "ncontrol", samplesize_col = "samplesize",
                                  gene_col = "gene", id_col = "id", min_pval = 1e-200, log_pval = FALSE,
                                  chr_col = "chr", pos_col = "pos")
{
  exposure_dat <- filename
  exposure_dat <- format_data(as.data.frame(exposure_dat),
                              type = "exposure", snps = NULL, phenotype_col = phenotype_col,
                              snp_col = snp_col, beta_col = beta_col, se_col = se_col,
                              eaf_col = eaf_col, effect_allele_col = effect_allele_col,
                              other_allele_col = other_allele_col, pval_col = pval_col,
                              units_col = units_col, ncase_col = ncase_col, ncontrol_col = ncontrol_col,
                              samplesize_col = samplesize_col, gene_col = gene_col,
                              id_col = id_col, min_pval = min_pval, log_pval = log_pval,
                              chr_col = chr_col, pos_col = pos_col)
  exposure_dat$data_source.exposure <- "textfile"
  if (clump) {
    exposure_dat <- clump_data(exposure_dat)
  }
  return(exposure_dat)
}

#----------format outcome data-----------------------------------------------------------------
format_outcome_data <- function (filename, snps = NULL, sep = " ", phenotype_col = "Phenotype",
                                 snp_col = "SNP", beta_col = "beta", se_col = "se",
                                 eaf_col = "eaf", effect_allele_col = "effect_allele",
                                 other_allele_col = "other_allele", pval_col = "pval",
                                 units_col = "units", ncase_col = "ncase", ncontrol_col = "ncontrol",
                                 samplesize_col = "samplesize", gene_col = "gene",
                                 id_col = "id", min_pval = 1e-200, log_pval = FALSE,
                                 chr_col = "chr", pos_col = "pos")
{
  outcome_dat <- filename
  outcome_dat <- format_data(as.data.frame(outcome_dat), type = "outcome",
                             snps = snps, phenotype_col = phenotype_col, snp_col = snp_col,
                             beta_col = beta_col, se_col = se_col, eaf_col = eaf_col,
                             effect_allele_col = effect_allele_col, other_allele_col = other_allele_col,
                             pval_col = pval_col, units_col = units_col, ncase_col = ncase_col,
                             ncontrol_col = ncontrol_col, samplesize_col = samplesize_col,
                             gene_col = gene_col, id_col = id_col, min_pval = min_pval,
                             log_pval = log_pval, chr_col = chr_col, pos_col = pos_col)
  outcome_dat$data_source.outcome <- "textfile"

  return(outcome_dat)
}
