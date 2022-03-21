rm(list = ls())

# Functions ---------------------------------------------------------------

source("scripts/Functions/save_data.R")


# Load Data ---------------------------------------------------------------

# raw_greenspaceall_geometry <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_greenspaceall_geometry.rds")
# raw_greenspaceall_geometry_monthly <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_greenspaceall_geometry_monthly.rds")


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

# # Create a function to clean greenspace data
# clean_greenspace <- function(raw_gs_df) {
#   tibble::as_tibble(raw_gs_df) %>%
#     # Rename to greenspace
#     dplyr::rename(greenspace = weighted_mean) %>%
#     # Factorize variables
#     dplyr::mutate(distance = distance %>% as.numeric() %>% as.factor(),
#                   raster = raster %>% as.factor(),
#                   id_dao = id_dao %>% as.factor()) %>%
#     # Rescale NLCD % Tree canopy to between 0-1
#     dplyr::mutate(greenspace = dplyr::case_when(
#       raster == "nlcd_26953" ~ greenspace/100,
#       TRUE ~ greenspace)) %>%
#     # Remove unnecessary variables
#     dplyr::select(-c(weight, weighted_value, type))
# }
#
# # Yearly
# gs_df <- raw_greenspaceall_geometry %>%
#   # Remove distance 2000 & 4000
#   dplyr::filter(!(distance %in% c("2000", "4000"))) %>%
#   # Create month = "All months"
#   dplyr::mutate(month = "All months") %>%
#   clean_greenspace()
#
# # NLCD
# gs_nlcd_df <- gs_df %>%
#   dplyr::filter(raster == "nlcd_26953")
#
# # Monthly
# gs_monthly_list <- raw_greenspaceall_geometry_monthly %>%
#   dplyr::mutate(raster = raster %>% stringr::str_sub(1, -4)) %>%
#   dplyr::mutate(month = month %>% as.factor()) %>%
#   clean_greenspace() %>%
#   dplyr::group_split(month) %>%
#   purrr::map2(c(1:12) %>% stringr::str_pad(2, pad = "0"),
#               function(x, y){
#                 nlcd <- gs_nlcd_df %>% dplyr::mutate(month = y)
#                 x %>% dplyr::bind_rows(nlcd)
#               })
#
# # All
# gs_all_list <- append(gs_df %>% list(),
#                       gs_monthly_list)
# gs_all_pair_list <- gs_all_list %>%
#   purrr::map(function(x) {c("landsat_26953", "nlcd_26953", "modis_26953") %>%
#       purrr::map(~x %>%
#                    dplyr::filter(!(raster %in% c(.x))))
#   })
#
# save_data(gs_all_list,
#           "DATA/Processed/Aim2/Agreement/gs_all_list",
#           "DATA/Processed/Aim2/Agreement/Archived/gs_all_list",
#           csv = FALSE)
# save_data(gs_all_pair_list,
#           "DATA/Processed/Aim2/Agreement/gs_all_pair_list",
#           "DATA/Processed/Aim2/Agreement/Archived/gs_all_pair_list",
#           csv = FALSE)

gs_all_list <- readr::read_rds("DATA/Processed/Aim2/Agreement/gs_all_list.rds")
gs_all_pair_list <- readr::read_rds("DATA/Processed/Aim2/Agreement/gs_all_pair_list.rds")




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
#                     paste0("outputs/figures/Aim2/scatterplot_", month, ".jpg"),
#                     device = "jpeg",
#                     width = 9,
#                     height = 6,
#                     units = "in")
#   })




# 1. Linear mixed-model ---------------------------------------------------

# # List of model info
# lmer_info <- tibble::tibble(month = rep(c(0:12) %>% stringr::str_pad(2, pad = "0"), each = 3),
#                             landsat_26953 = rep(c(0,1,1), times = 13),
#                             nlcd_26953 = rep(c(1,0,1), times = 13),
#                             modis_26953= rep(c(1,1,0), times = 13)) %>%
#   split(seq(nrow(.)))
#
#
# #Mixed effects model (1) in the main paper
# lmer_res <- gs_all_pair_list %>%
#   unlist(recursive = FALSE) %>%
#   purrr::map(function(df){
#     tictoc::tic("lme4::lmer")
#     res <- lme4::lmer(greenspace ~ raster +
#                         (1|id_dao) + (1|distance) +
#                         (1|id_dao:raster) + (1|id_dao:distance) +
#                         (1|distance:raster),
#                       data = df,
#                       control = lme4::lmerControl(optimizer = "bobyqa"))
#     res_sum <- summary(res)
#     tictoc::toc()
#     result <- list(res, res_sum)
#   })
#
# # (mixed model approach--modelling the differences) for calculating LOA
# lmer_res_diff <- gs_all_pair_list %>%
#   # unlist to create no nested list
#   unlist(recursive = FALSE) %>%
#   # run for each df
#   purrr::map(function(df) {
#     tictoc::tic("lme4::lmer() for res_diff")
#     # Edit df to create variable d
#     df <- df %>%
#       tidyr::pivot_wider(names_from = raster,
#                          values_from = greenspace) %>%
#       dplyr::mutate(d = .[[4]] - .[[5]])
#     # run lme4::lmer() for res_diff
#     res_diff <- lme4::lmer(d ~ (1|id_dao) + (1|distance),
#                            data = df,
#                            control = lme4::lmerControl(optimizer = "bobyqa"))
#     res_diff_sum <- summary(res_diff)
#     tictoc::toc()
#     result <- list(res_diff, res_diff_sum)
#   })
#
#
# lmer_res_diff_1 <- gs_all_pair_list %>%
#   # unlist to create no nested list
#   unlist(recursive = FALSE) %>%
#   # run for each df
#   purrr::map(function(df) {
#     tictoc::tic("lme4::lmer() for res_diff_1")
#     # Edit df to create variable d
#     df <- df %>%
#       tidyr::pivot_wider(names_from = raster,
#                          values_from = greenspace) %>%
#       dplyr::mutate(d = .[[4]] - .[[5]])
#     # run lme4::lmer() for res_diff_1
#     res_diff_1 <- lme4::lmer(d ~ 1 + (1|id_dao),
#                              data = df,
#                              control = lme4::lmerControl(optimizer = "bobyqa"))
#     res_diff_1_sum <- summary(res_diff_1)
#     tictoc::toc()
#     result <- list(res_diff_1, res_diff_1_sum)
#   })

# save_data(lmer_res_diff,
#           "DATA/Processed/Aim2/Agreement/lmer_res_diff",
#           "DATA/Processed/Aim2/Agreement/Archived/lmer_res_diff",
#           csv = FALSE)
# save_data(lmer_res_diff_1,
#           "DATA/Processed/Aim2/Agreement/lmer_res_diff_1",
#           "DATA/Processed/Aim2/Agreement/Archived/lmer_res_diff_1",
#           csv = FALSE)
# save_data(lmer_res,
#           "DATA/Processed/Aim2/Agreement/lmer_res",
#           "DATA/Processed/Aim2/Agreement/Archived/lmer_res",
#           csv = FALSE)
# save_data(lmer_info,
#           "DATA/Processed/Aim2/Agreement/lmer_info",
#           "DATA/Processed/Aim2/Agreement/Archived/lmer_info",
#           csv = FALSE)

lmer_info <- readr::read_rds("DATA/Processed/Aim2/Agreement/lmer_info.rds")
lmer_res <- readr::read_rds("DATA/Processed/Aim2/Agreement/lmer_res.rds")
lmer_res_diff <- readr::read_rds("DATA/Processed/Aim2/Agreement/lmer_res_diff.rds")
lmer_res_diff_1 <- readr::read_rds("DATA/Processed/Aim2/Agreement/lmer_res_diff_1.rds")



# Agreement statistics ----------------------------------------------------

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
          "DATA/Processed/Aim2/Agreement/agreement_stat_df_pairwise_allmonths",
          "DATA/Processed/Aim2/Agreement/Archived/agreement_stat_df_pairwise_allmonths")








# Bootstrap ---------------------------------------------------------------


# # CREATE BOOT SAMPLES
# set.seed(123)
# gs_all_list <- readr::read_rds("DATA/Processed/Aim2/Agreement/gs_all_list.rds")
# # create id_dao_df to merge with sample number
# id_dao_df <- gs_all_list[[1]] %>%
#   dplyr::distinct(id_dao) %>%
#   dplyr::arrange(id_dao) %>%
#   tibble::rowid_to_column("value")
#
# # Create a function to get resample data for bootstrap
# create_boot_sample <- function(data, n, B){
#   tictoc::tic("create boot sample")
#   # convert to tibble to data.table
#   data <- data %>% data.table::as.data.table()
#   # sample with replacement 1:n
#   boot_data <- sample(1:n, n, replace = TRUE) %>%
#     tibble::as_tibble() %>%
#     # Get id_dao values
#     dplyr::left_join(id_dao_df, by = "value") %$%
#     id_dao %>% as.character() %>% as.list() %>%
#     # Get data for each id_dao
#     purrr::map(~data[id_dao == .x, ]) %>%
#     # combine all data + include idb as new id_dao
#     data.table::rbindlist(use.names = FALSE, idcol = "idb")
#   # Export
#   B <- B %>% stringr::str_pad(3, pad = "0")
#   save_data(boot_data,
#             paste0("DATA/Processed/Aim2/Agreement/Bootstrap/boot_data_", B),
#             paste0("DATA/Processed/Aim2/Agreement/Bootstrap/Archived/boot_data_", B),
#             csv = FALSE)
#   tictoc::toc()
#   gc()
#   boot_data
# }
#
# # Prepare dataset to run purrr::pmap()
# map_df <- tidyr::crossing(data = gs_all_list[1],
#                           n = 21950,
#                           B = c(11:500))
# # Create list of boot samples
# boot_data <- map_df %>%
#   # Create boot sample for 3 set of df
#   purrr::pwalk(create_boot_sample)


files <- list.files(path = "DATA/Processed/Aim2/Agreement/Bootstrap/",
                    pattern = "^boot_data_\\d{3}\\.rds$",
                    full.names = TRUE) %>%
  sort()



# GET LMER RESULTS: res, res_diff, res_diff_1


file_location <- files[[3]]


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
                          (1|id_dao) + (1|distance) +
                          (1|id_dao:raster) + (1|id_dao:distance) +
                          (1|distance:raster),

                        data = df,
                        control = lme4::lmerControl(optimizer = "bobyqa"))
      res_sum <- summary(res)
      tictoc::toc()
      res_sum
    })
  # res_diff_sum
  res_diff_sum <- data_pair_diff %>%
    purrr::map(function(df){
      tictoc::tic("lme4::lmer res_diff")
      # run lme4::lmer() for res_diff
      res_diff <- lme4::lmer(d ~ (1|id_dao) + (1|distance),
                             data = df,
                             control = lme4::lmerControl(optimizer = "bobyqa"))
      res_diff_sum <- summary(res_diff)
      tictoc::toc()
      res_diff_sum
    })
  # res_diff_1_sum
  res_diff_1_sum <- data_pair_diff %>%
    purrr::map(function(df){
      tictoc::tic("lme4::lmer res_diff_1")
      # run lme4::lmer() for res_diff_1
      res_diff_1 <- lme4::lmer(d ~ (1|id_dao),
                               data = df,
                               control = lme4::lmerControl(optimizer = "bobyqa"))
      res_diff_1_sum <- summary(res_diff_1)
      tictoc::toc()
      res_diff_1_sum
    })
  # Export
  tictoc::tic(paste0("Export ", B))
  save_data(res_sum,
            paste0("DATA/Processed/Aim2/Agreement/Bootstrap/res_sum_", B),
            paste0("DATA/Processed/Aim2/Agreement/Bootstrap/Archived/res_sum_", B),
            csv = FALSE)
  save_data(res_diff_sum,
            paste0("DATA/Processed/Aim2/Agreement/Bootstrap/res_diff_sum_", B),
            paste0("DATA/Processed/Aim2/Agreement/Bootstrap/Archived/res_diff_sum_", B),
            csv = FALSE)
  save_data(res_diff_1_sum,
            paste0("DATA/Processed/Aim2/Agreement/Bootstrap/res_diff_1_sum_", B),
            paste0("DATA/Processed/Aim2/Agreement/Bootstrap/Archived/res_diff_1_sum_", B),
            csv = FALSE)
  tictoc::toc()
  gc()
  B
}


# Run and export mixed model
lmer_sum <- files[1] %>% purrr::map(get_mixed_model_sum)





# CALCULATE AGREEMENT STATS


# BOOT
# Create boot sample
# Get lmer results: res, res_diff, res_diff_1
# Calculate agreement stats
# Get quantile 0.025 & 0.975

# PLOT





# Prepare for loop: Set empty list for model result
resb <- resdb <- resd1b <- list()
for(l in 1:B){
  # ind: sample with replace subject
  ind <- sample(1:n, n, replace = TRUE)
  # Prepare for loop: Set empty list boot
  subject_boot <- list()
  for(j in 1:n){
    # Get subject data from original data (wide format)
    subject_boot[[j]] <- data[data$PatientID==ind[j],c(1,3,5,12)]
  }
  # Combine all subject data
  datab <- rbind(subject_boot[[1]], subject_boot[[2]], subject_boot[[3]], subject_boot[[4]],
                 subject_boot[[5]], subject_boot[[6]], subject_boot[[7]], subject_boot[[8]],
                 subject_boot[[9]], subject_boot[[10]], subject_boot[[11]], subject_boot[[12]],
                 subject_boot[[13]], subject_boot[[14]], subject_boot[[15]], subject_boot[[16]],
                 subject_boot[[17]], subject_boot[[18]], subject_boot[[19]], subject_boot[[20]],
                 subject_boot[[21]]
  )
  # prepare outcome for long data format
  yb <- as.vector(c(datab$RRox,datab$RRcb))
  # prepare NEW patientID
  aux <- c(rep(1,nrow(subject_boot[[1]])),rep(2,nrow(subject_boot[[2]])),
           rep(3,nrow(subject_boot[[3]])),rep(4,nrow(subject_boot[[4]])),
           rep(5,nrow(subject_boot[[5]])),rep(6,nrow(subject_boot[[6]])),
           rep(7,nrow(subject_boot[[7]])),rep(8,nrow(subject_boot[[8]])),
           rep(9,nrow(subject_boot[[9]])),rep(10,nrow(subject_boot[[10]])),
           rep(11,nrow(subject_boot[[11]])),rep(12,nrow(subject_boot[[12]])),
           rep(13,nrow(subject_boot[[13]])),rep(14,nrow(subject_boot[[14]])),
           rep(15,nrow(subject_boot[[15]])),rep(16,nrow(subject_boot[[16]])),
           rep(17,nrow(subject_boot[[17]])),rep(18,nrow(subject_boot[[18]])),
           rep(19,nrow(subject_boot[[19]])),rep(20,nrow(subject_boot[[20]])),
           rep(21,nrow(subject_boot[[21]]))
  )
  # prepare NEW patientID for long data format
  subjectb <- as.vector(as.factor(c(aux,aux)))
  # prepare device for long data format
  deviceb <- as.vector(as.factor(c(rep(1,nrow(datab)),rep(2,nrow(datab)))))
  # prepare activity for long data format
  activityb <- c(as.factor(datab$Activity),as.factor(datab$Activity))
  # prepare d = diffference for long data format
  db <- yb[(nrow(datab)+1):(2*nrow(datab))] - yb[1:nrow(datab)]
  # Create long data
  copdb <- data.frame(yb, subjectb, deviceb, activityb, db)
  copdb$deviceb <- as.factor(copdb$deviceb)
  copdb$activityb <- as.factor(copdb$activityb)
  copdb$subjectb <- as.factor(copdb$subjectb)
  # Run lme4::lmer() res
  resb[[l]] <- lmer(yb ~ deviceb+(1|subjectb)+(1|activityb)+
                      (1|subjectb:activityb)+(1|subjectb:deviceb)+(1|activityb:deviceb),
                    data = copdb,
                    control = lmerControl(optimizer = "bobyqa")
  )
  # Run lme4::lmer() res_diff
  resdb[[l]] <- lmer(db ~ (1|subjectb) + (1|activityb),
                     data = copdb,
                     control = lmerControl(optimizer = "bobyqa")
  )
  # Run lme4::lmer() res_diff_1
  resd1b[[l]] <- lmer(db ~ 1 + (1|subjectb),
                      data = copdb,
                      control = lmerControl(optimizer = "bobyqa")
  )
}

# Prepare empty object for later loop
beta2.est.b <- numeric(B)
sigma2.alpha.est.b <- sigma2.gamma.est.b <- numeric(B)
sigma2.alpha.gamma.est.b <- sigma2.alpha.beta.est.b <- numeric(B)
sigma2.beta.gamma.est.b <- sigma2.epsilon.est.b <- numeric(B)
phi2.beta.est.b <- numeric(B)
CCCb <- TDIb <- CPb <- CIAb <- MSDb <- numeric(B)
meanbb <- totalsdb <- numeric(B)
lclb <- uclb <- ll_rawb <- ul_rawb <- numeric(B)
for(l in 1:B){
  beta2.est.b[l] <- coef(summary(resb[[l]]))[2]
  sigma2.alpha.est.b[l] <- as.numeric(summary(resb[[l]])$varcor[4])
  sigma2.gamma.est.b[l] <- as.numeric(summary(resb[[l]])$varcor[5])
  sigma2.alpha.gamma.est.b[l] <- as.numeric(summary(resb[[l]])$varcor[1])
  sigma2.alpha.beta.est.b[l] <- as.numeric(summary(resb[[l]])$varcor[2])
  sigma2.beta.gamma.est.b[l] <- as.numeric(summary(resb[[l]])$varcor[3])
  sigma2.epsilon.est.b[l] <- as.numeric(summary(resb[[l]])$sigma)^2
  phi2.beta.est.b[l] <- beta2.est.b[l]^2
  num_ccc.b <- sigma2.alpha.est.b[l] + sigma2.gamma.est.b[l] + sigma2.alpha.gamma.est.b[l]
  den_ccc.b <- sigma2.alpha.est.b[l] + phi2.beta.est.b[l] +
    sigma2.gamma.est.b[l] + sigma2.alpha.gamma.est.b[l] +
    sigma2.alpha.beta.est.b[l] + sigma2.beta.gamma.est.b[l] +
    sigma2.epsilon.est.b[l]
  CCCb[l] <- num_ccc.b/den_ccc.b
  MSDb[l] <- (beta2.est.b[l]^2) +
    2*(sigma2.alpha.beta.est.b[l]+sigma2.beta.gamma.est.b[l]+sigma2.epsilon.est.b[l])
  TDIb[l] <- qnorm((1+p)/2)*sqrt(MSDb[l])
  CPb[l] <- 1-2*(1-pnorm(delta/sqrt(MSDb[l])))
  CIAb[l] <- 2*sigma2.epsilon.est.b[l]/MSDb[l]
  totalsdb[l] <- sqrt(as.numeric(summary(resdb[[l]])$varcor[1])+
                        as.numeric(summary(resdb[[l]])$varcor[2])+
                        as.numeric(summary(resdb[[l]])$sigma^2)
  )
  meanbb[l] <- coef(summary(resd1b[[l]]))[1]
  lclb[l] <- meanbb[l] - z*totalsdb[l]
  uclb[l] <- meanbb[l] + z*totalsdb[l]
  ll_rawb[l] <- beta2.est.b[l] - z*sqrt(2*sigma2.alpha.beta.est.b[l] +
                                          2*sigma2.beta.gamma.est.b[l] + 2*sigma2.epsilon.est.b[l])
  ul_rawb[l] <- beta2.est.b[l] + z*sqrt(2*sigma2.alpha.beta.est.b[l] +
                                          2*sigma2.beta.gamma.est.b[l] + 2*sigma2.epsilon.est.b[l])
}
MSD; quantile(MSDb, c(0.025,0.975))
## [1] 30.78389
## 2.5% 97.5%
## 22.98767 41.65718
CCC; quantile(CCCb, c(0.025,0.975))
## [1] 0.676822
## 2.5% 97.5%
## 0.5960483 0.7208343
TDI; quantile(TDIb, c(0.025,0.975))
## [1] 10.87451
## 2.5% 97.5%
## 9.397085 12.650072
CP; quantile(CPb, c(0.025,0.975))
## [1] 0.6325038
## 2.5% 97.5%
## 0.5614741 0.7029882
CIA; quantile(CIAb, c(0.025,0.975))
## [1] 0.6820637
## 2.5% 97.5%
## 0.5653724 0.7526850
lb <- quantile(lclb, c(0.025, 0.975))
lcl; lb
## [1] -11.57078
## 2.5% 97.5%
## -13.515309 -9.937825
ub <- quantile(uclb, c(0.025, 0.975))
ucl; ub
## [1] 8.378797
## 2.5% 97.5%
## 6.372063 10.690036
meanbbq <- quantile(meanbb, c(0.025, 0.975))
meanb; meanbbq
## [1] -1.595991
## 2.5% 97.5%
## -2.1307454 -0.9747718
lrawb <- quantile(ll_rawb, c(0.025, 0.975))
ll_raw; lrawb
## [1] -11.86257
## 2.5% 97.5%
## -13.73020 -10.33243
urawb <- quantile(ul_rawb, c(0.025, 0.975))
ul_raw; urawb
## [1] 9.297345
## 2.5% 97.5%
## 7.372611 11.421008
b2b <- quantile(beta2.est.b, c(0.025, 0.975))
beta2.est; b2b
## [1] -1.282612
## 2.5% 97.5%
## -1.8945034 -0.4927148

# Bland-Altman plot
m <- (copd$y[1:385] + copd$y[386:770])/2
plot(m, d, xlab = "Average", ylab = "Difference", ylim = c(-30,30))
abline(h=ucl, lwd = 2, lty = 2)
abline(h=ub[1], lwd = 3, lty = 3)
abline(h=ub[2], lwd = 3, lty = 3)
abline(h=lcl, lwd = 2, lty = 2)
abline(h=lb[1], lwd = 3, lty = 3)
abline(h=lb[2], lwd = 3, lty = 3)
abline(h=0, lwd = 2)
abline(h=meanb, lwd = 2, lty = 2)
abline(h=meanbbq[1], lwd = 3, lty = 3)
abline(h=meanbbq[2], lwd = 3, lty = 3)

# Clean Agreement table ---------------------------------------------------
agreement_stat_df <- readr::read_rds("DATA/Processed/Aim2/Agreement/agreement_stat_df_pairwise_allmonths.rds") %>%
  dplyr::group_split(month)





# Agreement, 3 raster -----------------------------------------------------

#Mixed effects model (1) in the main paper
lmer3_res <- gs_all_df %>%
  dplyr::group_split(month) %>%
  purrr::map(function(df){
    tictoc::tic("lme4::lmer() for 3 raster")
    res <- lme4::lmer(greenspace ~ raster +
                        (1|id_dao) + (1|distance) +
                        (1|id_dao:raster) + (1|id_dao:distance) +
                        (1|distance:raster),
                      data = df,
                      control = lme4::lmerControl(optimizer = "bobyqa"))
    res_sum <- summary(res)
    tictoc::toc()
    result <- list(res, res_sum)
  })


lmer12_res <- gs_all_df %>%
  dplyr::filter(month != "All months") %>%
  list() %>%
  purrr::map(function(df){
    tictoc::tic("lme4::lmer")
    res <- lme4::lmer(greenspace ~ raster +
                        (1|id_dao) + (1|distance) +
                        (1|month) +
                        (1|raster:month) + (1|month:distance) + (1|month:id_dao) +
                        (1|id_dao:raster) + (1|id_dao:distance) +
                        (1|distance:raster),
                      data = df,
                      control = lme4::lmerControl(optimizer = "bobyqa"))
    res_sum <- summary(res)
    tictoc::toc()
    result <- list(res, res_sum)
  })

save_data(lmer3_res,
          "DATA/Processed/Aim2/Agreement/lmer3_res",
          "DATA/Processed/Aim2/Agreement/Archived/lmer3_res",
          csv = FALSE)
save_data(lmer12_res,
          "DATA/Processed/Aim2/Agreement/lmer12_res",
          "DATA/Processed/Aim2/Agreement/Archived/lmer12_res",
          csv = FALSE)

