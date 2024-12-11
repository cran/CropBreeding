#' Two-Way ANOVA for Genotype x Environment Interaction with Multiple Traits
#'
#' This function performs a two-way ANOVA to analyze genotype and environment interactions 
#' for multiple traits, including replication effects. It provides separate ANOVA results 
#' for each specified trait in the dataset.
#'
#' @importFrom stats aov as.formula na.omit
#' @param data A data frame containing the dataset with required columns.
#' @param genotype_col Character. Name of the genotype column.
#' @param environment_col Character. Name of the environment column.
#' @param replication_col Character. Name of the replication column.
#' @param trait_cols A vector of trait column names to analyze.
#' @return A list containing ANOVA results for each trait.
#' @examples
#' \donttest{
#' set.seed(123)
#' data <- data.frame(
#'   Genotype = rep(c("G1", "G2", "G3"), each = 12),
#'   Environment = rep(c("E1", "E2", "E3", "E4"), times = 9),
#'   Replication = rep(c("R1", "R2", "R3"), times = 12),
#'   Trait1 = c(rnorm(36, 50, 5)),
#'   Trait2 = c(rnorm(36, 150, 10)),
#'   Trait3 = c(rnorm(36, 250, 15))
#' )
#' anova_results <- gxe_analysis_multiple(
#'   data = data,
#'   genotype_col = "Genotype",
#'   environment_col = "Environment",
#'   replication_col = "Replication",
#'   trait_cols = c("Trait1", "Trait2", "Trait3")
#' )
#' print(anova_results$Trait1)
#' }
#' @references
#' Fisher, R. A. (1935). "The Design of Experiments". ISBN: 9780198522294.
#' @export

gxe_analysis_multiple <- function(data, genotype_col, environment_col, replication_col, trait_cols) {
  # Validate input data
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  required_cols <- c(genotype_col, environment_col, replication_col)
  if (!all(required_cols %in% colnames(data))) {
    stop("One or more specified columns are not present in the input data.")
  }
  
  # Handle missing data
  data <- stats::na.omit(data)
  
  # Initialize a list to store ANOVA results for each trait
  results <- list()
  
  for (trait_col in trait_cols) {
    if (!trait_col %in% colnames(data)) {
      stop(paste("Trait column", trait_col, "not found in the dataset."))
    }
    
    # Build the formula for two-way ANOVA
    formula <- stats::as.formula(
      paste(trait_col, "~", genotype_col, "*", environment_col, "+", replication_col)
    )
    
    # Perform ANOVA
    aov_result <- stats::aov(formula, data = data)
    
    # Store the summary of the ANOVA
    results[[trait_col]] <- summary(aov_result)
  }
  
  # Return the list of ANOVA results
  return(results)
}
