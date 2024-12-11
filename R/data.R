# creating radom data set with file name data.R for documentation purposes
genotypes <- paste0("G", 1:40)
environments <- 1:3
traits <- data.frame(
  genotype = rep(genotypes, each = length(environments)),
  environment = rep(environments, times = length(genotypes)),
  trait1 = rnorm(120, mean = 50, sd = 5),  # Trait 1
  trait2 = rnorm(120, mean = 25, sd = 3),  # Trait 2
  trait3 = rnorm(120, mean = 75, sd = 10)  # Trait 3
)



