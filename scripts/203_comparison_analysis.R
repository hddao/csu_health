rm(list = ls())

# Functions ---------------------------------------------------------------

save_data <- function(dataset.name, file.location, file.location.arc,
                      csv = TRUE, sas = FALSE, xlsx = FALSE){
  saveRDS(dataset.name, file = paste0(file.location, ".rds"))
  saveRDS(dataset.name, file = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".rds"))

  # CSV
  if(csv) {
    readr::write_csv(dataset.name, paste0(file.location, ".csv"))
    readr::write_csv(dataset.name, paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".csv"))
  }

  # SAS
  if(sas) {
    dataset.name %<>% dplyr::mutate(dplyr::across(where(is.factor), as.character))
    foreign::write.foreign(dataset.name,
                           datafile = paste0(file.location, ".txt"),
                           codefile = paste0(file.location, ".sas"),
                           package = "SAS")
    foreign::write.foreign(dataset.name,
                           datafile = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".txt"),
                           codefile = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".sas"),
                           package = "SAS")
  }

  # XLSX
  if(xlsx) {
    openxlsx::write.xlsx(dataset.name,paste0(file.location, ".xlsx"))
    openxlsx::write.xlsx(dataset.name,paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".xlsx"))
  }
}


# Load Data ---------------------------------------------------------------

raw_greenspaceall_geometry <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_greenspaceall_geometry.rds")

# Note: Comparison analysis -----------------------------------------------

# Follow Parker 2020 paper for agreement analysis
# https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-020-01022-x
# "scripts/Example scripts/Aim2_Parker2020.R"

# Data set:
#   Distinct geometry/residential locations
#   Has residential locations
#   Within Colorado
# Identified linear mixed-model
#   Fixed effect: raster
#   Random effect: buffer
#   Each observation is a distinct residential location
#   Interaction between buffer and raster
# Calculate agreement indices pairwise
#   Scaled: ICC, CCC
#   Unscaled: LOA, MSD, CP, TDI
#   Plots: Bland-Altman plots pairwise raster with different colors by buffer distance (R package nlme)
# Identify:
#   Inter-subject variance
#   Inter-raster variance
#   Inter-buffer variance
# Descriptive stats
#   Table 1
#   Scatterplot pairwise with a line of equality




# 1. Linear mixed-model ---------------------------------------------------


# Prepare dataset
greenspace_df <- tibble::as_tibble(raw_greenspaceall_geometry) %>%
  dplyr::rename(ndvi_weightedmean = weighted_mean) %>%
  dplyr::mutate(distance = distance %>% as.numeric() %>% as.factor(),
                raster = raster %>% as.factor(),
                id_dao = id_dao %>% as.factor(),
                ndvi_weightedmean = dplyr::case_when(
                  raster == "nlcd_26953" ~ ndvi_weightedmean/100,
                  TRUE ~ ndvi_weightedmean)
                ) %>%
  dplyr::mutate(ndvi_weightedmean100 = ndvi_weightedmean*100) %>%
  dplyr::select(id_dao, ndvi_weightedmean, raster, distance)

gs_pair_df <- c("landsat_26953", "nlcd_26953", "modis_26953") %>%
  purrr::map(~greenspace_df %>%
               dplyr::filter(!(raster %in% c(.x))))

#Mixed effects model (1) in the main paper
tictoc::tic("lme4::lmer")
res <- lme4::lmer(ndvi_weightedmean ~ raster +
                    (1|id_dao) + (1|distance) +
                    (1|id_dao:raster) + (1|id_dao:distance) +
                    (1|distance:raster),
                  data = greenspace_df,
                  control = lme4::lmerControl(optimizer = "bobyqa"))
res_sum <- summary(res)
tictoc::toc()
# lme4::lmer: 1011.92 sec elapsed

# Variance statistics

# beta coefficient (Estimate) for raster
beta2.est <- coef(summary(res))[2]
# varcor for id_dao
sigma2.alpha.est <- as.numeric(summary(res)$varcor[3])
# varcor for distance
sigma2.gamma.est <- as.numeric(summary(res)$varcor[5])
# varcor for id_dao:distance
sigma2.alpha.gamma.est <- as.numeric(summary(res)$varcor[1])
# varcor for id_dao:raster
sigma2.alpha.beta.est <- as.numeric(summary(res)$varcor[2])
# varcor for distance:raster
sigma2.beta.gamma.est <- as.numeric(summary(res)$varcor[4])
# varcor for error (residual)
sigma2.epsilon.est <- as.numeric(summary(res)$sigma)^2
# squared beta coefficient (Estimate) for raster
phi2.beta.est <- beta2.est^2


#Concordance correlation coefficient
num_ccc <- sigma2.alpha.est + sigma2.gamma.est + sigma2.alpha.gamma.est
den_ccc <- sigma2.alpha.est + phi2.beta.est + sigma2.gamma.est +
  sigma2.alpha.gamma.est + sigma2.alpha.beta.est +
  sigma2.beta.gamma.est + sigma2.epsilon.est
CCC <- num_ccc/den_ccc
CCC

#Mean squared deviation
MSD <- (beta2.est^2) + 2*(sigma2.alpha.beta.est+sigma2.beta.gamma.est+sigma2.epsilon.est)
MSD

#Total deviation index
p <- 0.95
TDI <- qnorm((1+p)/2)*sqrt(MSD)
TDI

#Coverage probability
delta <- c(.05, .1)
CP <- 1-2*(1-pnorm(delta/sqrt(MSD)))
CP

#Coefficient of individual agreement
CIA <- 2*sigma2.epsilon.est/MSD
CIA



# Descriptive stats -------------------------------------------------------


gs_desc_df <- greenspace_df %>%
  tidyr::pivot_wider(names_from = raster,
                     values_from = ndvi_weightedmean)

ggplot2::ggplot(gs_desc_df) +
  ggplot2::geom_point(ggplot2::aes(x = landsat_26953, y = modis_26953,
                                   colour = distance),
                      shape = "circle open") +
  ggplot2::xlim(0, 1) +
  ggplot2::ylim(0, 1) +
  ggplot2::geom_abline(slope = 1, intercept = 0)


  ggplot2::ggplot(result_res, aes_string(x = covar[varorder], y = ".observed")) +
  ggplot2::geom_point(aes(colour = (cdenumber)),
                      shape = "circle open",
                      show.legend = FALSE) +
  ggplot2::geom_smooth(method = "loess", se = TRUE)
