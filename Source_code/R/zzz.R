#' @importFrom utils globalVariables
NULL

.onLoad <- function(libname, pkgname) {
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(
      c(
        # Variables from ezstat()
        "P_value", "taxa", "group1", "group2", "statistic",
        "p.adj", "P.adj", "Comparison1", "Comparison2", "Comparison",
        "FDR_P_value", "Group", "Median", "Mean", "CLD",
        "y_max", "y", "Letter",
        # Additional variables for data manipulation and stats
        "Value", "Abundance", "Sample", "SampleID", "Taxonomy",
        "teststat_value", "padj", "letters", "cld_out",
        # Variables for plotting
        "x", "fill", "geom_bar", "geom_text", "aes",
        # Data-table/dplyr operators
        ":=", ".data",
        # Other potential variables
        "dir_name","data", "canonical_level", "microbiome_base",
        "sample_id_col", "taxa_cols", "cols_to_keep",
        "stat_df", "df_stat", "temp_df", "dunn_res",
        "pairwise_df", "cld_summary_df", "box_dir",
        "out_kw", "out_pw", "out_cld", "out_png",
        "sig_taxa", "kw_df", "pw_df", "tax_pw",
        "pval_vec", "groups_present", "n_groups",
        "needed", "cld_df", "summary_stats", "stats", "setNames", "y_pos_df",
        "letter_df", "letter_pos", "p"
      )
    )
  }
}