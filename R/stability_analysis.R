#' Stability Analysis using Eberhart-Russell Model
#'
#' This function performs stability analysis for multiple traits across different environments using
#' Eberhart and Russell's regression model provided by the `metan` package. It computes ANOVA tables
#' and regression parameters for assessing genotype stability.
#'
#' @importFrom metan ge_reg
#' @importFrom rlang sym
#' @param data A data frame containing the dataset with required columns.
#' @param genotype_col Character. Name of the genotype column.
#' @param environment_col Character. Name of the environment column.
#' @param replication_col Character. Name of the replication column.
#' @param trait_cols A vector of trait column names (response variables).
#' @return A list containing results for each trait:
#' \itemize{
#'   \item `anova`: The ANOVA table for each trait.
#'   \item `regression`: Regression parameters for stability analysis.
#' }
#' @examples
#' \donttest{
#' if (!requireNamespace("metan", quietly = TRUE)) {
#'   install.packages("metan")
#' }
#' library(metan)
#' 
#' # Simulated dataset
#' set.seed(123)
#' data <- data.frame(
#'   Genotype = rep(c("G1", "G2", "G3"), each = 12),
#'   Environment = rep(c("E1", "E2", "E3", "E4"), times = 9),
#'   Replication = rep(1:3, times = 12),
#'   Trait1 = c(rnorm(36, 50, 5)),
#'   Trait2 = c(rnorm(36, 150, 10)),
#'   Trait3 = c(rnorm(36, 250, 15))
#' )
#' 
#' results <- stability_analysis(
#'   data = data,
#'   genotype_col = "Genotype",
#'   environment_col = "Environment",
#'   replication_col = "Replication",
#'   trait_cols = c("Trait1", "Trait2", "Trait3")
#' )
#' 
#' print(results$Trait1$anova)
#' print(results$Trait1$regression)
#' }
#' @references
#' Eberhart, S. A., & Russell, W. A. (1966). "Stability Parameters for Comparing Varieties". 
#' Crop Science, 6(1), 36–40. doi:10.2135/cropsci1966.0011183X000600010011x
#' @export
stability_analysis <- function(data, genotype_col, environment_col, replication_col, trait_cols) {
  # Validate input data
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  
  required_cols <- c(genotype_col, environment_col, replication_col)
  if (!all(required_cols %in% colnames(data))) {
    stop("One or more required columns are not present in the dataset.")
  }
  
  # Perform stability analysis for each trait
  results <- metan::ge_reg(
    .data = data,
    env = rlang::sym(environment_col),
    gen = rlang::sym(genotype_col),
    rep = rlang::sym(replication_col),
    resp = trait_cols,  # Pass as a character vector
    verbose = FALSE
  )
  
  # Structure results into a list for each trait
  formatted_results <- list()
  for (trait in trait_cols) {
    formatted_results[[trait]] <- list(
      anova = results[[trait]]$anova,
      regression = results[[trait]]$regression
    )
  }
  
  return(formatted_results)
}
