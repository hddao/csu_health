# Load packages
packages <- c("magrittr", "tictoc")
package_check <- lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x, dependencies = TRUE)
  library(x, character.only = TRUE)
})
rm(packages, package_check )
