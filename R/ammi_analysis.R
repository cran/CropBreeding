#' Perform AMMI Analysis for a Single Trait
#'
#' This function performs Additive Main Effects and Multiplicative Interaction (AMMI) analysis
#' for a single trait to evaluate genotype x environment interactions. It generates biplots
#' and PC1 vs. Trait visualizations without relying on predictions.
#'
#' @importFrom metan performs_ammi plot_scores theme_metan theme_metan_minimal
#' @importFrom dplyr sym
#' @param data A data frame containing the dataset with required columns.
#' @param env_col Character. Name of the environment column.
#' @param gen_col Character. Name of the genotype column.
#' @param rep_col Character. Name of the replication column.
#' @param trait_col Character. Name of the trait column to be analyzed.
#' @return A list containing:
#' \itemize{
#'   \item `analysis`: The AMMI analysis results.
#'   \item `biplot`: The biplot (PC1 vs PC2).
#'   \item `pc1_plot`: The PC1 vs Trait plot.
#' }
#' @examples
#' \donttest{
#' set.seed(123)
#' data <- data.frame(
#'   GEN = rep(c("G1", "G2", "G3", "G4"), each = 12),
#'   ENV = rep(c("E1", "E2", "E3"), each = 4, times = 4),
#'   REP = rep(1:3, times = 16),
#'   Y = c(rnorm(12, 50, 5), rnorm(12, 55, 5), rnorm(12, 60, 5), rnorm(12, 65, 5))
#' )
#' results <- perform_ammi_single_trait(data, "ENV", "GEN", "REP", "Y")
#' }
#' @export

perform_ammi_single_trait <- function(data, env_col, gen_col, rep_col, trait_col) {
  # Perform AMMI analysis using metan::performs_ammi
  ammi_result <- metan::performs_ammi(
    .data = data,
    env = dplyr::sym(env_col),  # Environment column
    gen = dplyr::sym(gen_col),  # Genotype column
    rep = dplyr::sym(rep_col),  # Replication column
    resp = dplyr::sym(trait_col)  # Trait column
  )
  
  # Generate Customized Biplot (PC1 vs PC2)
  custom_biplot <- metan::plot_scores(
    ammi_result,
    type = 2,             # Type 2 for PC1 vs PC2 interaction biplot
    polygon = TRUE,       # Add polygon for clustering
    col.gen = "purple",   # Customize genotype points
    col.env = "orange",   # Customize environment points
    col.segm.env = "blue",
    size.gen = 4,         # Increase genotype point size
    size.env = 5,         # Increase environment point size
    plot_theme = metan::theme_metan(grid = "both")
  )
  
  # Generate PC1 vs Trait Plot
  pc1_plot <- metan::plot_scores(
    ammi_result,
    type = 1,             # Type 1 for PC1 vs Trait
    col.gen = "red",
    col.env = "blue",
    axis.expand = 1.2,
    plot_theme = metan::theme_metan_minimal()
  )
  
  # Return results as a list
  return(list(
    analysis = ammi_result,
    biplot = custom_biplot,
    pc1_plot = pc1_plot
  ))
}
