# Load packages
packages <- c("magrittr")
package_check <- lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x, dependencies = TRUE)
  library(x, character.only = TRUE)
})
rm(packages, package_check )

future::plan("multicore", workers = future::availableCores() - 1)

