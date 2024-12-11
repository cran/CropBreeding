#' Breeding Metrics Calculation
#'
#' This function calculates key breeding metrics such as genotypic variance, environmental variance, 
#' genotypic coefficient of variation (GCV), phenotypic coefficient of variation (PCV), heritability, 
#' and genetic advance (GA). These metrics are critical for assessing genotype performance and 
#' genetic potential in crop breeding experiments.
#'
#' @importFrom stats aov as.formula na.omit
#' @param data A data frame containing the dataset with required columns.
#' @param genotype_col Character. Name of the genotype column.
#' @param trait_col Character. Name of the trait column.
#' @param replication_col Character. Name of the replication column.
#' @return A list containing:
#' \itemize{
#'   \item `ANOVA`: The summary of the ANOVA table.
#'   \item `GenotypicVariance`: Genotypic variance, which measures the genetic variability among genotypes.
#'   \item `EnvironmentalVariance`: Environmental variance, which reflects the variability due to environmental factors.
#'   \item `PhenotypicVariance`: Phenotypic variance, the sum of genotypic and environmental variances.
#'   \item `GCV`: Genotypic coefficient of variation, expressed as a percentage of the mean.
#'   \item `PCV`: Phenotypic coefficient of variation, expressed as a percentage of the mean.
#'   \item `Heritability`: Broad-sense heritability, the proportion of phenotypic variance attributable to genetic variance.
#'   \item `GeneticAdvance`: Genetic advance under selection, a measure of genetic improvement.
#'   \item `GAPercentage`: Genetic advance as a percentage of the mean, indicating the relative genetic improvement.
#' }
#' @examples
#' \donttest{
#' set.seed(123)
#' data <- data.frame(
#'   Genotype = rep(c("G1", "G2", "G3"), each = 10),
#'   Replication = rep(1:10, times = 3),
#'   Trait = c(rnorm(10, 50, 5), rnorm(10, 55, 5), rnorm(10, 60, 5))
#' )
#' result <- breeding_metrics(data, "Genotype", "Trait", "Replication")
#' print(result)
#' }
#' @references
#' Falconer, D. S. (1996). "Introduction to Quantitative Genetics". ISBN: 9780582243026.
#' Singh, R. K., & Chaudhary, B. D. (1985). "Biometrical Methods in Quantitative Genetic Analysis". ISBN: 9788122433764.
#' @export

breeding_metrics <- function(data, genotype_col, trait_col, replication_col) {
  # Check if required columns exist in the data
  required_cols <- c(genotype_col, trait_col, replication_col)
  if (!all(required_cols %in% colnames(data))) {
    stop("One or more specified columns are not present in the input data.")
  }
  
  # Handle missing data
  data <- stats::na.omit(data)
  
  # Calculate mean squares from ANOVA
  formula <- stats::as.formula(paste(trait_col, "~", genotype_col, "+", replication_col))
  anova_result <- stats::aov(formula, data = data)
  
  # Extract mean squares
  anova_summary <- summary(anova_result)[[1]]
  ms_genotype <- anova_summary[genotype_col, "Mean Sq"]
  ms_error <- anova_summary["Residuals", "Mean Sq"]
  replications <- length(unique(data[[replication_col]]))
  
  # Calculate variances
  genotypic_variance <- max((ms_genotype - ms_error) / replications, 0)  # Ensure non-negative
  environmental_variance <- ms_error
  phenotypic_variance <- genotypic_variance + environmental_variance
  
  # GCV and PCV
  grand_mean <- mean(data[[trait_col]], na.rm = TRUE)
  gcv <- ifelse(grand_mean > 0, sqrt(genotypic_variance) / grand_mean * 100, 0)
  pcv <- ifelse(grand_mean > 0, sqrt(phenotypic_variance) / grand_mean * 100, 0)
  
  # Heritability
  heritability <- ifelse(phenotypic_variance > 0, genotypic_variance / phenotypic_variance, 0)
  
  # Genetic Advance (GA)
  selection_intensity <- 2.06  # For 5% selection intensity
  genetic_advance <- selection_intensity * sqrt(phenotypic_variance) * heritability
  ga_percentage <- ifelse(grand_mean > 0, (genetic_advance / grand_mean) * 100, 0)
  
  # Print ANOVA summary
  message("ANOVA Summary:")
  print(anova_summary)
  
  # Return results as a list
  return(list(
    ANOVA = anova_summary,
    GenotypicVariance = genotypic_variance,
    EnvironmentalVariance = environmental_variance,
    PhenotypicVariance = phenotypic_variance,
    GCV = gcv,
    PCV = pcv,
    Heritability = heritability,
    GeneticAdvance = genetic_advance,
    GAPercentage = ga_percentage
  ))
}
