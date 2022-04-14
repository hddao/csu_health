rm(list = ls())

# Functions ---------------------------------------------------------------

source("scripts/Functions/save_data.R")
source("scripts/Functions/create_folder.R")


# Load Data ---------------------------------------------------------------

raw_greenspaceall_geometry_landsat <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_greenspaceall_geometry_summerclear.rds") %>%
  dplyr::mutate(raster = dplyr::recode(raster, "landsat_26953_summerclear" = "landsat_26953"))

raw_greenspaceall_geometry_modis <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_greenspaceall_geometry_summer.rds") %>%
  dplyr::mutate(raster = dplyr::recode(raster, "modis_26953_summer" = "modis_26953")) %>%
  dplyr::filter(raster == "modis_26953")

raw_greenspaceall_geometry_nlcd <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_greenspaceall_geometry.rds") %>%
  dplyr::filter(raster == "nlcd_26953" & !(distance %in% c("2000", "4000")))

raw_greenspaceall_geometry <- dplyr::bind_rows(raw_greenspaceall_geometry_landsat,
                                               raw_greenspaceall_geometry_modis,
                                               raw_greenspaceall_geometry_nlcd)
# Check no missing greenspace value
all(!is.na(raw_greenspaceall_geometry$weighted_mean))
# TRUE

rm(raw_greenspaceall_geometry_landsat,
   raw_greenspaceall_geometry_modis,
   raw_greenspaceall_geometry_nlcd)


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



# Prepare dataset ---------------------------------------------------------

# Create an exported folder
create_folder("DATA/Processed/Aim2/", "Agreement_summerclear")
create_folder("DATA/Processed/Aim2/Agreement_summerclear/", "Archived")


# Create a function to clean greenspace data
clean_greenspace <- function(raw_gs_df) {
  tibble::as_tibble(raw_gs_df) %>%
    # Rename to greenspace
    dplyr::rename(greenspace = weighted_mean) %>%
    # Factorize variables
    dplyr::mutate(distance = distance %>% as.numeric() %>% as.factor(),
                  raster = raster %>% as.factor(),
                  id_dao = id_dao %>% as.factor()) %>%
    # Rescale NLCD % Tree canopy to between 0-1
    dplyr::mutate(greenspace = dplyr::case_when(
      raster == "nlcd_26953" ~ greenspace/100,
      TRUE ~ greenspace)) %>%
    # Remove unnecessary variables
    dplyr::select(-c(weight, weighted_value, type))
}

# Summer months
gs_df <- raw_greenspaceall_geometry %>%
  # Remove distance 2000 & 4000
  dplyr::filter(!(distance %in% c("2000", "4000"))) %>%
  # Create month = "summer months"
  dplyr::mutate(month = "summer months") %>%
  clean_greenspace()

# All
gs_all_list <- list(gs_df)
gs_all_pair_list <- gs_all_list %>%
  purrr::map(function(x) {c("landsat_26953", "nlcd_26953", "modis_26953") %>%
      purrr::map(~x %>%
                   dplyr::filter(!(raster %in% c(.x))))
  })

save_data(gs_all_list,
          "DATA/Processed/Aim2/Agreement_summerclear/gs_all_list",
          "DATA/Processed/Aim2/Agreement_summerclear/Archived/gs_all_list",
          csv = FALSE)
save_data(gs_all_pair_list,
          "DATA/Processed/Aim2/Agreement_summerclear/gs_all_pair_list",
          "DATA/Processed/Aim2/Agreement_summerclear/Archived/gs_all_pair_list",
          csv = FALSE)

gs_all_list <- readr::read_rds("DATA/Processed/Aim2/Agreement_summerclear/gs_all_list.rds")
gs_all_pair_list <- readr::read_rds("DATA/Processed/Aim2/Agreement_summerclear/gs_all_pair_list.rds")




# Descriptive stats -------------------------------------------------------

# gs_desc_df <- gs_all_pair_list %>%
#   purrr::map(function(x){
#     purrr::map(x, ~.x %>% tidyr::pivot_wider(names_from = raster,
#                                              values_from = greenspace))
#   })
#
# gs_desc_df %>%
#   purrr::walk(.f = function(list){
#     plot_list <- list %>%
#       purrr::map(.f = function(df){
#         ggplot2::ggplot(df, ggplot2::aes(x = get(names(df)[4]), y = get(names(df)[5]))) +
#           ggplot2::geom_point(ggplot2::aes(colour = distance),
#                               shape = "circle open") +
#           ggplot2::xlim(0, 0.7) +
#           ggplot2::ylim(0, 0.7) +
#           ggplot2::geom_abline(slope = 1, intercept = 0) +
#           ggplot2::xlab(names(df)[4]) +
#           ggplot2::ylab(names(df)[5]) +
#           ggplot2::ggtitle(paste0("Month: ", df$month[1]))
#       })
#     month <- list[[1]]$month[1]
#     p <- gridExtra::arrangeGrob(plot_list[[1]], plot_list[[2]], plot_list[[3]],
#                                 nrow=2, ncol=2,
#                                 top = grid::textGrob(paste0("Month: ", month)))
#     ggplot2::ggsave(plot = p,
#                     paste0("outputs/figures/Aim2/summerclear/scatterplot_", month, ".jpg"),
#                     device = "jpeg",
#                     width = 9,
#                     height = 6,
#                     units = "in")
#   })




# 1. Linear mixed-model ---------------------------------------------------
# lmerControl
# https://cran.r-project.org/web/packages/lme4/vignettes/lmerperf.html
# https://joshua-nugent.github.io/allFit/
gs_all_pair_list <- readr::read_rds("DATA/Processed/Aim2/Agreement_summerclear/gs_all_pair_list.rds")


# List of model info
lmer_info <- tibble::tibble(month = rep(c("summer months"), each = 3),
                            landsat_26953 = rep(c(0,1,1), times = 1),
                            nlcd_26953 = rep(c(1,0,1), times = 1),
                            modis_26953= rep(c(1,1,0), times = 1)) %>%
  split(seq(nrow(.)))


#Mixed effects model (1) in the main paper
lmer_res <- gs_all_pair_list %>%
  unlist(recursive = FALSE) %>%
  purrr::map(function(df){
    tictoc::tic("lme4::lmer")
    res <- lme4::lmer(greenspace ~ raster +
                        (1|id_dao) + (1|distance) +
                        (1|id_dao:raster) + (1|id_dao:distance) +
                        (1|distance:raster),
                      control = lme4::lmerControl(optimizer = "bobyqa"),
                      data = df)
    res_sum <- summary(res)
    tictoc::toc()
    result <- list(res, res_sum)
  })

# (mixed model approach--modelling the differences) for calculating LOA
lmer_res_diff <- gs_all_pair_list %>%
  # unlist to create no nested list
  unlist(recursive = FALSE) %>%
  # run for each df
  purrr::map(function(df) {
    tictoc::tic("lme4::lmer() for res_diff")
    # Edit df to create variable d
    df <- df %>%
      tidyr::pivot_wider(names_from = raster,
                         values_from = greenspace) %>%
      dplyr::mutate(d = .[[4]] - .[[5]])
    # run lme4::lmer() for res_diff
    res_diff <- lme4::lmer(d ~ (1|id_dao) + (1|distance),
                           # control = lme4::lmerControl(optimizer = "bobyqa"),
                           data = df)
    res_diff_sum <- summary(res_diff)
    tictoc::toc()
    result <- list(res_diff, res_diff_sum)
  })


lmer_res_diff_1 <- gs_all_pair_list %>%
  # unlist to create no nested list
  unlist(recursive = FALSE) %>%
  # run for each df
  purrr::map(function(df) {
    tictoc::tic("lme4::lmer() for res_diff_1")
    # Edit df to create variable d
    df <- df %>%
      tidyr::pivot_wider(names_from = raster,
                         values_from = greenspace) %>%
      dplyr::mutate(d = .[[4]] - .[[5]])
    # run lme4::lmer() for res_diff_1
    res_diff_1 <- lme4::lmer(d ~ 1 + (1|id_dao),
                             # control = lme4::lmerControl(optimizer = "bobyqa"),
                             data = df)
    res_diff_1_sum <- summary(res_diff_1)
    tictoc::toc()
    result <- list(res_diff_1, res_diff_1_sum)
  })

save_data(lmer_res_diff,
          "DATA/Processed/Aim2/Agreement_summerclear/lmer_res_diff",
          "DATA/Processed/Aim2/Agreement_summerclear/Archived/lmer_res_diff",
          csv = FALSE)
save_data(lmer_res_diff_1,
          "DATA/Processed/Aim2/Agreement_summerclear/lmer_res_diff_1",
          "DATA/Processed/Aim2/Agreement_summerclear/Archived/lmer_res_diff_1",
          csv = FALSE)
save_data(lmer_res,
          "DATA/Processed/Aim2/Agreement_summerclear/lmer_res",
          "DATA/Processed/Aim2/Agreement_summerclear/Archived/lmer_res",
          csv = FALSE)
save_data(lmer_info,
          "DATA/Processed/Aim2/Agreement_summerclear/lmer_info",
          "DATA/Processed/Aim2/Agreement_summerclear/Archived/lmer_info",
          csv = FALSE)

lmer_info <- readr::read_rds("DATA/Processed/Aim2/Agreement_summerclear/lmer_info.rds")
lmer_res <- readr::read_rds("DATA/Processed/Aim2/Agreement_summerclear/lmer_res.rds")
lmer_res_diff <- readr::read_rds("DATA/Processed/Aim2/Agreement_summerclear/lmer_res_diff.rds")
lmer_res_diff_1 <- readr::read_rds("DATA/Processed/Aim2/Agreement_summerclear/lmer_res_diff_1.rds")


# 2. Agreement statistics -------------------------------------------------

lmer_info <- readr::read_rds("DATA/Processed/Aim2/Agreement_summerclear/lmer_info.rds")
lmer_res <- readr::read_rds("DATA/Processed/Aim2/Agreement_summerclear/lmer_res.rds")
lmer_res_diff <- readr::read_rds("DATA/Processed/Aim2/Agreement_summerclear/lmer_res_diff.rds")
lmer_res_diff_1 <- readr::read_rds("DATA/Processed/Aim2/Agreement_summerclear/lmer_res_diff_1.rds")

create_agreement_stats <- function(lmer_res, lmer_res_diff, lmer_res_diff_1,
                                   p = c(0.9, 0.95), delta = c(0.05, 0.1), alpha = 0.05) {
  # Get summary
  res_sum <- lmer_res[[2]]
  res_diff_sum <- lmer_res_diff[[2]]
  res_diff_1_sum <- lmer_res_diff_1[[2]]

  # beta coefficient (Estimate) for raster
  beta2.est <- coef(res_sum)[2]
  # varcor for id_dao
  sigma2.alpha.est <- res_sum$varcor$id_dao %>% attr("stddev") %>% as.numeric()
  # varcor for distance
  sigma2.gamma.est <- res_sum$varcor$distance %>% attr("stddev") %>% as.numeric()
  # varcor for id_dao:distance
  sigma2.alpha.gamma.est <- res_sum$varcor$`id_dao:distance` %>% attr("stddev") %>% as.numeric()
  # varcor for id_dao:raster
  sigma2.alpha.beta.est <- res_sum$varcor$`id_dao:raster` %>% attr("stddev") %>% as.numeric()
  # varcor for distance:raster
  sigma2.beta.gamma.est <- res_sum$varcor$`distance:raster` %>% attr("stddev") %>% as.numeric()
  # varcor for error (residual)
  sigma2.epsilon.est <- as.numeric(res_sum$sigma)^2
  # squared beta coefficient (Estimate) for raster
  phi2.beta.est <- beta2.est^2

  #Concordance correlation coefficient
  num_ccc <- sigma2.alpha.est + sigma2.gamma.est + sigma2.alpha.gamma.est
  den_ccc <- sigma2.alpha.est + phi2.beta.est + sigma2.gamma.est +
    sigma2.alpha.gamma.est + sigma2.alpha.beta.est +
    sigma2.beta.gamma.est + sigma2.epsilon.est
  CCC <- num_ccc/den_ccc
  #Mean squared deviation
  MSD <- (beta2.est^2) + 2*(sigma2.alpha.beta.est+sigma2.beta.gamma.est+sigma2.epsilon.est)
  #Total deviation index
  # p <- c(0.90, 0.95)
  TDI <- (qnorm((1+p)/2)*sqrt(MSD)) %>% list()
  #Coverage probability
  # delta <- c(0.05, 0.1)
  CP <- (1-2*(1-pnorm(delta/sqrt(MSD)))) %>% list()
  #Coefficient of individual agreement
  CIA <- 2*sigma2.epsilon.est/MSD

  # Limits of agreement (mixed model approach--modelling the differences)
  # totalsd
  totalsd <- sqrt(as.numeric(res_diff_sum$varcor[1]) +
                    as.numeric(res_diff_sum$varcor[2]) +
                    as.numeric(res_diff_sum$sigma^2))
  # meanb
  meanb <- coef(res_diff_1_sum)[1]
  # lcl; ucl
  # alpha <- 0.05
  z <- qnorm(1-alpha/2)
  lcl <- meanb - z*totalsd
  ucl <- meanb + z*totalsd
  #limits of agreement (mixed model approach--raw data)
  ll_raw <- beta2.est - z*sqrt(2*sigma2.alpha.beta.est + 2*sigma2.beta.gamma.est + 2*sigma2.epsilon.est)
  ul_raw <- beta2.est + z*sqrt(2*sigma2.alpha.beta.est + 2*sigma2.beta.gamma.est + 2*sigma2.epsilon.est)
  mean_raw <- beta2.est

  # Exported df
  var_stat_df <- tibble::tibble(beta2.est,
                                sigma2.alpha.est,
                                sigma2.gamma.est,
                                sigma2.alpha.gamma.est,
                                sigma2.alpha.beta.est,
                                sigma2.beta.gamma.est,
                                sigma2.epsilon.est,
                                phi2.beta.est,
                                CCC, MSD, TDI, CP, CIA,
                                totalsd, meanb, lcl, ucl, mean_raw, ll_raw, ul_raw)
}

# Create a df for purrr::map()
map_df <- tibble::tibble(lmer_res,
                         lmer_res_diff,
                         lmer_res_diff_1)

agreement_stat_df <- purrr::pmap(map_df, .f = create_agreement_stats) %>%
  # Added information on the model
  purrr::map2(lmer_info, function(df1, df2) {dplyr::bind_cols(df1, df2)}) %>%
  # Combine all df
  dplyr::bind_rows()

save_data(agreement_stat_df,
          "DATA/Processed/Aim2/Agreement_summerclear/agreement_stat_df_pairwise_allmonths",
          "DATA/Processed/Aim2/Agreement_summerclear/Archived/agreement_stat_df_pairwise_allmonths")




# 3. Bootstrap ------------------------------------------------------------
# * a. Create boot samples ------------------------------------------------

# Create an exported folder
create_folder("DATA/Processed/Aim2/Agreement_summerclear", "Bootstrap")
create_folder("DATA/Processed/Aim2/Agreement_summerclear/Bootstrap", "Archived")


set.seed(123)
gs_all_list <- readr::read_rds("DATA/Processed/Aim2/Agreement_summerclear/gs_all_list.rds")
# create id_dao_df to merge with sample number
id_dao_df <- gs_all_list[[1]] %>%
  dplyr::distinct(id_dao) %>%
  dplyr::arrange(id_dao) %>%
  tibble::rowid_to_column("value")

# Create a function to get resample data for bootstrap
create_boot_sample <- function(data, n, B){
  tictoc::tic("create boot sample")
  # convert to tibble to data.table
  data <- data %>% data.table::as.data.table()
  # sample with replacement 1:n
  boot_data <- sample(1:n, n, replace = TRUE) %>%
    tibble::as_tibble() %>%
    # Get id_dao values
    dplyr::left_join(id_dao_df, by = "value") %$%
    id_dao %>% as.character() %>% as.list() %>%
    # Get data for each id_dao
    purrr::map(~data[id_dao == .x, ]) %>%
    # combine all data + include idb as new id_dao
    data.table::rbindlist(use.names = FALSE, idcol = "idb")
  # Export
  B <- B %>% stringr::str_pad(3, pad = "0")
  save_data(boot_data,
            paste0("DATA/Processed/Aim2/Agreement_summerclear/Bootstrap/boot_data_", B),
            paste0("DATA/Processed/Aim2/Agreement_summerclear/Bootstrap/Archived/boot_data_", B),
            csv = FALSE)
  tictoc::toc()
  gc()
  boot_data
}

# Prepare dataset to run purrr::pmap()
map_df <- tidyr::crossing(data = gs_all_list[1],
                          n = 21950,
                          B = c(1:500))
# Create list of boot samples
boot_data <- map_df[1:170, ] %>% purrr::pwalk(create_boot_sample)
boot_data <- map_df[171:340, ] %>% purrr::pwalk(create_boot_sample)
boot_data <- map_df[341:500, ] %>% purrr::pwalk(create_boot_sample)
# EACH create boot sample: ~43 sec elapsed







# * b. Get lmer() model summary: res, res_diff, res_diff_1 ----------------

create_folder("DATA/Processed/Aim2/Agreement_summerclear/Bootstrap", "lmer")
create_folder("DATA/Processed/Aim2/Agreement_summerclear/Bootstrap/lmer", "Archived")


# Create a function to get res_sum, res_diff_sum, and res_diff_1_sum
get_mixed_model_sum <- function(file_location){
  B <- file_location %>% stringr::str_sub(-7, -5)
  tictoc::tic(paste0("read data ", B))
  # read data in
  data <- file_location %>% readr::read_rds()
  tictoc::toc()

  # convert to data for pair of raster
  data_pair <- c("landsat_26953", "nlcd_26953", "modis_26953") %>%
    purrr::map(~data %>% dplyr::filter(!(raster %in% c(.x))))
  data_pair_diff <- data_pair %>%
    purrr::map(~.x %>%
                 # Edit df to create variable d
                 tidyr::pivot_wider(names_from = raster,
                                    values_from = greenspace) %>%
                 dplyr::mutate(d = .[[5]] - .[[6]]))

  # run lmer
  # res_sum
  res_sum <- data_pair %>%
    purrr::map(function(df){
      tictoc::tic("lme4::lmer res")
      res <- lme4::lmer(greenspace ~ raster +
                          (1|idb) + (1|distance) +
                          (1|idb:raster) + (1|idb:distance) +
                          (1|distance:raster),
                        # control = lme4::lmerControl(optimizer = "bobyqa"),
                        data = df)
      res_sum <- summary(res)
      tictoc::toc()
      res_sum
    })
  # res_diff_sum
  res_diff_sum <- data_pair_diff %>%
    purrr::map(function(df){
      tictoc::tic("lme4::lmer res_diff")
      # run lme4::lmer() for res_diff
      res_diff <- lme4::lmer(d ~ (1|idb) + (1|distance),
                             # control = lme4::lmerControl(optimizer = "bobyqa"),
                             data = df)
      res_diff_sum <- summary(res_diff)
      tictoc::toc()
      res_diff_sum
    })
  # res_diff_1_sum
  res_diff_1_sum <- data_pair_diff %>%
    purrr::map(function(df){
      tictoc::tic("lme4::lmer res_diff_1")
      # run lme4::lmer() for res_diff_1
      res_diff_1 <- lme4::lmer(d ~ (1|idb),
                               # control = lme4::lmerControl(optimizer = "bobyqa"),
                               data = df)
      res_diff_1_sum <- summary(res_diff_1)
      tictoc::toc()
      res_diff_1_sum
    })
  # Export
  tictoc::tic(paste0("Export ", B))
  save_data(res_sum,
            paste0("DATA/Processed/Aim2/Agreement_summerclear/Bootstrap/lmer/res_sum_", B),
            paste0("DATA/Processed/Aim2/Agreement_summerclear/Bootstrap/lmer/Archived/res_sum_", B),
            csv = FALSE)
  save_data(res_diff_sum,
            paste0("DATA/Processed/Aim2/Agreement_summerclear/Bootstrap/lmer/res_diff_sum_", B),
            paste0("DATA/Processed/Aim2/Agreement_summerclear/Bootstrap/lmer/Archived/res_diff_sum_", B),
            csv = FALSE)
  save_data(res_diff_1_sum,
            paste0("DATA/Processed/Aim2/Agreement_summerclear/Bootstrap/lmer/res_diff_1_sum_", B),
            paste0("DATA/Processed/Aim2/Agreement_summerclear/Bootstrap/lmer/Archived/res_diff_1_sum_", B),
            csv = FALSE)
  tictoc::toc()
  gc()
  B
}

files <- list.files(path = "DATA/Processed/Aim2/Agreement_summerclear/Bootstrap/",
                    pattern = "^boot_data_\\d{3}\\.rds$",
                    full.names = TRUE) %>%
  sort()

# Run and export mixed model
lmer_sum <- files[1:100] %>% purrr::map(get_mixed_model_sum)
lmer_sum <- files[101:200] %>% purrr::map(get_mixed_model_sum)
lmer_sum <- files[201:300] %>% purrr::map(get_mixed_model_sum)

lmer_sum <- files[301:370] %>% purrr::map(get_mixed_model_sum)
lmer_sum <- files[401:470] %>% purrr::map(get_mixed_model_sum)
lmer_sum <- files[c(371:400, 471:500)] %>% purrr::map(get_mixed_model_sum)



# res_sum_xxx: 3 model summary for the pairs of c("MODIS & NLCD", "MODIS & Landsat 8", "Landsat 8 & NLCD") consequentially



# * c. Caculate and export agreement stats --------------------------------

# Create an exported folder
create_folder("DATA/Processed/Aim2/Agreement_summerclear/Bootstrap/", "agreement_stat")
create_folder("DATA/Processed/Aim2/Agreement_summerclear/Bootstrap/agreement_stat", "Archived")


# Create a function to get agreement stats from lmer model summary
create_agreement_stats <- function(res_sum, res_diff_sum, res_diff_1_sum,
                                   p = c(0.9, 0.95), delta = c(0.05, 0.1), alpha = 0.05) {
  # beta coefficient (Estimate) for raster
  beta2.est <- coef(res_sum)[2]
  # varcor for idb
  sigma2.alpha.est <- res_sum$varcor$idb %>% attr("stddev") %>% as.numeric()
  # varcor for distance
  sigma2.gamma.est <- res_sum$varcor$distance %>% attr("stddev") %>% as.numeric()
  # varcor for idb:distance
  sigma2.alpha.gamma.est <- res_sum$varcor$`idb:distance` %>% attr("stddev") %>% as.numeric()
  # varcor for idb:raster
  sigma2.alpha.beta.est <- res_sum$varcor$`idb:raster` %>% attr("stddev") %>% as.numeric()
  # varcor for distance:raster
  sigma2.beta.gamma.est <- res_sum$varcor$`distance:raster` %>% attr("stddev") %>% as.numeric()
  # varcor for error (residual)
  sigma2.epsilon.est <- as.numeric(res_sum$sigma)^2
  # squared beta coefficient (Estimate) for raster
  phi2.beta.est <- beta2.est^2

  #Concordance correlation coefficient
  num_ccc <- sigma2.alpha.est + sigma2.gamma.est + sigma2.alpha.gamma.est
  den_ccc <- sigma2.alpha.est + phi2.beta.est + sigma2.gamma.est +
    sigma2.alpha.gamma.est + sigma2.alpha.beta.est +
    sigma2.beta.gamma.est + sigma2.epsilon.est
  CCC <- num_ccc/den_ccc
  #Mean squared deviation
  MSD <- (beta2.est^2) + 2*(sigma2.alpha.beta.est+sigma2.beta.gamma.est+sigma2.epsilon.est)
  #Total deviation index
  # p <- c(0.90, 0.95)
  TDI <- (qnorm((1+p)/2)*sqrt(MSD)) %>% list()
  #Coverage probability
  # delta <- c(0.05, 0.1)
  CP <- (1-2*(1-pnorm(delta/sqrt(MSD)))) %>% list()
  #Coefficient of individual agreement
  CIA <- 2*sigma2.epsilon.est/MSD

  # Limits of agreement (mixed model approach--modelling the differences)
  # totalsd
  totalsd <- sqrt(as.numeric(res_diff_sum$varcor[1]) +
                    as.numeric(res_diff_sum$varcor[2]) +
                    as.numeric(res_diff_sum$sigma^2))
  # meanb
  meanb <- coef(res_diff_1_sum)[1]
  # lcl; ucl
  # alpha <- 0.05
  z <- qnorm(1-alpha/2)
  lcl <- meanb - z*totalsd
  ucl <- meanb + z*totalsd
  #limits of agreement (mixed model approach--raw data)
  ll_raw <- 0-(beta2.est + z*sqrt(2*sigma2.alpha.beta.est + 2*sigma2.beta.gamma.est + 2*sigma2.epsilon.est))
  ul_raw <- 0-(beta2.est - z*sqrt(2*sigma2.alpha.beta.est + 2*sigma2.beta.gamma.est + 2*sigma2.epsilon.est))
  mean_raw <- 0-beta2.est

  # Exported df
  var_stat_df <- tibble::tibble(beta2.est,
                                sigma2.alpha.est,
                                sigma2.gamma.est,
                                sigma2.alpha.gamma.est,
                                sigma2.alpha.beta.est,
                                sigma2.beta.gamma.est,
                                sigma2.epsilon.est,
                                phi2.beta.est,
                                CCC, MSD, TDI, CP, CIA,
                                totalsd, meanb, lcl, ucl, mean_raw, ll_raw, ul_raw) %>%
    dplyr::rename_all(tolower) %>%
    # change cp and tdi from list to numeric
    dplyr::mutate(cp_05 = cp %>% purrr::map(dplyr::first) %>% as.numeric(),
                  cp_10 = cp %>% purrr::map(dplyr::last) %>% as.numeric(),
                  tdi_05 = tdi %>% purrr::map(dplyr::first) %>% as.numeric(),
                  tdi_10 = tdi %>% purrr::map(dplyr::last) %>% as.numeric())
}

# create function to get agreementstat for boot sample
run_agreement_stats <- function(df){
  df %>%
    purrr::pmap(function(files_res_sum, files_res_diff_sum, files_res_diff_1_sum) {
      B <- files_res_sum %>% stringr::str_sub(-7, -5)
      # Create a df for purrr::map()
      tictoc::tic(paste0("create map_df ", B))
      map_df <- tibble::tibble(res_sum = files_res_sum %>% readr::read_rds(),
                               res_diff_sum = files_res_diff_sum %>% readr::read_rds(),
                               res_diff_1_sum = files_res_diff_1_sum %>% readr::read_rds())
      tictoc::toc()

      # Create agreement_stat_df
      tictoc::tic(paste0("create agreement_stat_df ", B))
      agreement_stat_df <- map_df %>% purrr::pmap(create_agreement_stats) %>%
        # Added information on the model
        purrr::map2(lmer_info, function(df1, df2) {dplyr::bind_cols(df1, df2)}) %>%
        # Combine all df
        dplyr::bind_rows() %>%
        # Create a variable for sample id
        dplyr::mutate(b = B)
      tictoc::toc()

      # Export
      tictoc::tic(paste0("Export ", B))
      save_data(agreement_stat_df,
                paste0("DATA/Processed/Aim2/Agreement_summerclear/Bootstrap/agreement_stat/agreement_stat_df_", B),
                paste0("DATA/Processed/Aim2/Agreement_summerclear/Bootstrap/agreement_stat/Archived/agreement_stat_df_", B),
                csv = FALSE)
      tictoc::toc()
      gc()
      B
    })
}

# List of model info
lmer_info <- tibble::tibble(landsat_26953 = c(0,1,1),
                            nlcd_26953 = c(1,0,1),
                            modis_26953= c(1,1,0),
                            pair = c("MODIS & NLCD", "MODIS & Landsat 8", "Landsat 8 & NLCD")) %>%
  split(seq(nrow(.)))

# Get a df of all lmer model summary
files_df <- tibble::tibble(files_res_sum = list.files(path = "DATA/Processed/Aim2/Agreement_summerclear/Bootstrap/lmer/",
                                                      pattern = "^res_sum_\\d{3}\\.rds$",
                                                      full.names = TRUE) %>% sort(),
                           files_res_diff_sum = list.files(path = "DATA/Processed/Aim2/Agreement_summerclear/Bootstrap/lmer/",
                                                           pattern = "^res_diff_sum_\\d{3}\\.rds$",
                                                           full.names = TRUE) %>% sort(),
                           files_res_diff_1_sum = list.files(path = "DATA/Processed/Aim2/Agreement_summerclear/Bootstrap/lmer/",
                                                             pattern = "^res_diff_1_sum_\\d{3}\\.rds$",
                                                             full.names = TRUE) %>% sort())


# Calculate agreement stats and export
stat <- files_df[1:170, ] %>% run_agreement_stats()
stat <- files_df[171:340, ] %>% run_agreement_stats()
stat <- files_df[341:500, ] %>% run_agreement_stats()

# EACH create map_df 031: ~27 sec elapsed
# EACH create agreement_stat_df 031: ~0.05 sec elapsed
# EACH Export 031: ~0.02 sec elapsed





# * d. Get quantile 0.025 & 0.975 -----------------------------------------

# Create a reference df for raster pair
pair_df <- tibble::tibble(landsat_26953 = c(0,1,1),
                          nlcd_26953 = c(1,0,1),
                          modis_26953 = c(1,1,0),
                          pair = c("MODIS & NLCD", "MODIS & Landsat 8", "Landsat 8 & NLCD"))


files <- list.files(path = "DATA/Processed/Aim2/Agreement_summerclear/Bootstrap/agreement_stat/",
                    pattern = "^agreement_stat_df_\\d{3}\\.rds$",
                    full.names = TRUE) %>% sort()

# Combine all agreement_stat_df
agreement_stat_all_list <- files %>%
  purrr::map(readr::read_rds) %>%
  dplyr::bind_rows() %>%
  # Drop list column
  dplyr::select(-c(tdi, cp)) %>%
  dplyr::group_split(landsat_26953, nlcd_26953, modis_26953)


quantile_list <- agreement_stat_all_list %>%
  purrr::map(~apply(.x %>% dplyr::select_if(is.numeric),
                    MARGIN = 2,
                    FUN = quantile , probs = c(0.025, 0.975) , na.rm = FALSE ) %>%
               # t() %>%
               tibble::as_tibble(rownames = NA) %>%
               tibble::rownames_to_column()) %>%
  purrr::map(~.x %>% dplyr::left_join(pair_df, by = c("landsat_26953", "modis_26953", "nlcd_26953")))

save_data(quantile_list,
          "DATA/Processed/Aim2/Agreement_summerclear/quantile_list",
          "DATA/Processed/Aim2/Agreement_summerclear/Archived/quantile_list",
          csv = FALSE)


# 4. Agreement, 3 raster --------------------------------------------------

gs_all_list <- readr::read_rds("DATA/Processed/Aim2/Agreement_summerclear/gs_all_list.rds")

#Mixed effects model (1) in the main paper
lmer3_res <- gs_all_list %>%
  purrr::map(function(df){
    tictoc::tic("lme4::lmer() for 3 raster")
    res <- lme4::lmer(greenspace ~ raster +
                        (1|id_dao) + (1|distance) +
                        (1|id_dao:raster) + (1|id_dao:distance) +
                        (1|distance:raster),
                      # control = lme4::lmerControl(optimizer = "bobyqa"),
                      data = df)
    res_sum <- summary(res)
    tictoc::toc()
    result <- list(res, res_sum)
  })

save_data(lmer3_res,
          "DATA/Processed/Aim2/Agreement_summerclear/lmer3_res",
          "DATA/Processed/Aim2/Agreement_summerclear/Archived/lmer3_res",
          csv = FALSE)
