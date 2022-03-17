rm(list = ls())

# Functions ---------------------------------------------------------------

source("scripts/Functions/save_data.R")


# Load Data ---------------------------------------------------------------

raw_greenspaceall_geometry <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_greenspaceall_geometry.rds")
raw_greenspaceall_geometry_monthly <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_greenspaceall_geometry_monthly.rds")

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

# Yearly
gs_df <- raw_greenspaceall_geometry %>%
  # Remove distance 2000 & 4000
  dplyr::filter(!(distance %in% c("2000", "4000"))) %>%
  # Create month = "All months"
  dplyr::mutate(month = "All months") %>%
  clean_greenspace()
# List of data in pair of raster
gs_pair_list <- c("landsat_26953", "nlcd_26953", "modis_26953") %>%
  purrr::map(~gs_df %>%
               dplyr::filter(!(raster %in% c(.x))))


# NLCD
gs_nlcd_df <- gs_df %>%
  dplyr::filter(raster == "nlcd_26953")


# Monthly
gs_monthly_df <- raw_greenspaceall_geometry_monthly %>%
  dplyr::mutate(raster = raster %>% stringr::str_sub(1, -4)) %>%
  dplyr::mutate(month = month %>% as.factor()) %>%
  clean_greenspace()
# List of data by month in pair of raster
gs_monthly_pair_list <- gs_monthly_df %>%
  dplyr::group_split(month)
# With NLCD
gs_monthly_pair_nlcd_list <- gs_monthly_pair_list %>%
  purrr::map2(c(1:12) %>% stringr::str_pad(2, pad = "0"),
    function(x, y){
      nlcd <- gs_nlcd_df %>% dplyr::mutate(month = y)
      x %>% dplyr::bind_rows(nlcd)
  }) %>%
  purrr::map(function(x) {c("landsat_26953", "nlcd_26953", "modis_26953") %>%
      purrr::map(~x %>%
                   dplyr::filter(!(raster %in% c(.x))))
    })



# All
gs_all_df <- dplyr::bind_rows(gs_df, gs_monthly_df)
gs_all_pair_list <- append(gs_pair_list %>% list(),
                      gs_monthly_pair_nlcd_list)

# Descriptive stats -------------------------------------------------------

gs_desc_df <- gs_all_pair_list %>%
  purrr::map(function(x){
    purrr::map(x, ~.x %>% tidyr::pivot_wider(names_from = raster,
                                          values_from = greenspace))
  })

gs_desc_df %>%
  purrr::walk(.f = function(list){
    plot_list <- list %>%
      purrr::map(.f = function(df){
        ggplot2::ggplot(df, ggplot2::aes(x = get(names(df)[4]), y = get(names(df)[5]))) +
          ggplot2::geom_point(ggplot2::aes(colour = distance),
                              shape = "circle open") +
          ggplot2::xlim(0, 0.7) +
          ggplot2::ylim(0, 0.7) +
          ggplot2::geom_abline(slope = 1, intercept = 0) +
          ggplot2::xlab(names(df)[4]) +
          ggplot2::ylab(names(df)[5]) +
          ggplot2::ggtitle(paste0("Month: ", df$month[1]))
      })
    month <- list[[1]]$month[1]
    p <- gridExtra::arrangeGrob(plot_list[[1]], plot_list[[2]], plot_list[[3]],
                                nrow=2, ncol=2,
                                top = grid::textGrob(paste0("Month: ", month)))
    ggplot2::ggsave(plot = p,
                    paste0("outputs/figures/Aim2/scatterplot_", month, ".jpg"),
                    device = "jpeg",
                    width = 9,
                    height = 6,
                    units = "in")
  })




# 1. Linear mixed-model ---------------------------------------------------

#Mixed effects model (1) in the main paper
lmer_res <- gs_all_pair_list %>%
  unlist(recursive = FALSE) %>%
  purrr::map(function(df){
    tictoc::tic("lme4::lmer")
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
lmer_info <- tibble::tibble(month = rep(c(0:12) %>% stringr::str_pad(2, pad = "0"), each = 3),
                             landsat_26953 = rep(c(0,1,1), times = 13),
                             nlcd_26953 = rep(c(1,0,1), times = 13),
                             modis_26953= rep(c(1,1,0), times = 13)) %>%
 split(seq(nrow(.)))
# save_data(lmer_res,
#           "DATA/Processed/Aim2/Agreement/lmer_res",
#           "DATA/Processed/Aim2/Agreement/Archived/lmer_res",
#           csv = FALSE)
# save_data(lmer_info,
#           "DATA/Processed/Aim2/Agreement/lmer_info",
#           "DATA/Processed/Aim2/Agreement/Archived/lmer_info",
#           csv = FALSE)



# Variance statistics -----------------------------------------------------

create_var_stat_df <- function(res_sum, p = c(0.90, 0.95), delta = c(0.05, 0.1)) {
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

  var_stat_df <- tibble::tibble(beta2.est,
                                sigma2.alpha.est,
                                sigma2.gamma.est,
                                sigma2.alpha.gamma.est,
                                sigma2.alpha.beta.est,
                                sigma2.beta.gamma.est,
                                sigma2.epsilon.est,
                                phi2.beta.est,
                                CCC,
                                MSD,
                                TDI,
                                CP,
                                CIA)
}

var_stat_list <- lmer_res %>%
  # Calculate variance statistics for each lmer result
  purrr::map(function(list){list[[2]] %>% create_var_stat_df()}) %>%
  # Added information on the model
  purrr::map2(lmer_info, function(x, y) {dplyr::bind_cols(x, y)}) %>%
  # Combine all df
  dplyr::bind_rows()





# LOA ---------------------------------------------------------------------


data <- gs_all_pair_list[[1]][[1]] %>%
  tidyr::pivot_wider(names_from = raster,
                     values_from = greenspace) %>%
  dplyr::mutate(d = .[[4]] + .[[5]])


#Limits of agreement (mixed model approach--modelling the differences)
res_diff <- lme4::lmer(d ~ (1|id_dao) + (1|distance),
                 data = data,
                 control = lme4::lmerControl(optimizer = "bobyqa"))
summary(res_diff)
## Linear mixed model fit by REML ['lmerMod']
## Formula: d ~ (1 | id_dao) + (1 | distance)
## Control: lmerControl(optimizer = "bobyqa")
##
## REML criterion at convergence: 2231.1
##
## Scaled residuals:
## Min 1Q Median 3Q Max
## -4.2106 -0.4468 -0.0138 0.3886 5.8757
##
## Random effects:
## Groups Name Variance Std.Dev.
## id_dao (Intercept) 0.9602 0.9799
## distance (Intercept) 7.5652 2.7505
## Residual 17.3753 4.1684
## Number of obs: 385, groups: id_dao, 21; distance, 11
##
## Fixed effects:
## Estimate Std. Error t value
## (Intercept) -1.273 0.895 -1.422
totalsd <- sqrt(as.numeric(summary(res.diff)$varcor[1])+
                  as.numeric(summary(res.diff)$varcor[2])+
                  as.numeric(summary(res.diff)$sigma^2)
)
res.diff.1 <- lmer(d ~ 1 + (1|id_dao),
                   control = lmerControl(optimizer = "bobyqa")
)
summary(res.diff.1)
## Linear mixed model fit by REML ['lmerMod']
## Formula: d ~ 1 + (1 | id_dao)
## Control: lmerControl(optimizer = "bobyqa")
##
## REML criterion at convergence: 2297.7
##
## Scaled residuals:
## Min 1Q Median 3Q Max
## -4.7264 -0.4244 0.1472 0.3797 5.9935
##
## Random effects:
## Groups Name Variance Std.Dev.
## id_dao (Intercept) 0.67 0.8186
## Residual 22.36 4.7288
## Number of obs: 385, groups: id_dao, 21
##
## Fixed effects:
## Estimate Std. Error t value
## (Intercept) -1.5960 0.3001 -5.317
meanb <- coef(summary(res.diff.1))[1]
meanb
## [1] -1.595991
alpha <- 0.05
z <- qnorm(1-alpha/2)
lcl <- meanb - z*totalsd
ucl <- meanb + z*totalsd
lcl; ucl
## [1] -11.57078
## [1] 8.378797
#limits of agreement (mixed model approach--raw data)
ll_raw <- beta2.est - z*sqrt(2*sigma2.alpha.beta.est + 2*sigma2.beta.gamma.est + 2*sigma2.epsilon.est)
ul_raw <- beta2.est + z*sqrt(2*sigma2.alpha.beta.est + 2*sigma2.beta.gamma.est + 2*sigma2.epsilon.est)
ll_raw; ul_raw
## [1] -11.86257
## [1] 9.297345
beta2.est
## [1] -1.282612




# Bootstrap for CI --------------------------------------------------------


####bootstrap procedure
set.seed(123)
n <- 21
B <- 500
resb <- resdb <- resd1b <- list()
for(l in 1:B){
  ind <- sample(1:n, n, replace = TRUE)
  id_dao_boot <- list()
  for(j in 1:n){
    id_dao_boot[[j]] <- data[data$PatientID==ind[j],c(1,3,5,12)]
  }
  datab <- rbind(id_dao_boot[[1]], id_dao_boot[[2]], id_dao_boot[[3]], id_dao_boot[[4]],
                 id_dao_boot[[5]], id_dao_boot[[6]], id_dao_boot[[7]], id_dao_boot[[8]],
                 id_dao_boot[[9]], id_dao_boot[[10]], id_dao_boot[[11]], id_dao_boot[[12]],
                 id_dao_boot[[13]], id_dao_boot[[14]], id_dao_boot[[15]], id_dao_boot[[16]],
                 id_dao_boot[[17]], id_dao_boot[[18]], id_dao_boot[[19]], id_dao_boot[[20]],
                 id_dao_boot[[21]]
  )
  yb <- as.vector(c(datab$RRox,datab$RRcb))
  aux <- c(rep(1,nrow(id_dao_boot[[1]])),rep(2,nrow(id_dao_boot[[2]])),
           rep(3,nrow(id_dao_boot[[3]])),rep(4,nrow(id_dao_boot[[4]])),
           rep(5,nrow(id_dao_boot[[5]])),rep(6,nrow(id_dao_boot[[6]])),
           rep(7,nrow(id_dao_boot[[7]])),rep(8,nrow(id_dao_boot[[8]])),
           rep(9,nrow(id_dao_boot[[9]])),rep(10,nrow(id_dao_boot[[10]])),
           rep(11,nrow(id_dao_boot[[11]])),rep(12,nrow(id_dao_boot[[12]])),
           rep(13,nrow(id_dao_boot[[13]])),rep(14,nrow(id_dao_boot[[14]])),
           rep(15,nrow(id_dao_boot[[15]])),rep(16,nrow(id_dao_boot[[16]])),
           rep(17,nrow(id_dao_boot[[17]])),rep(18,nrow(id_dao_boot[[18]])),
           rep(19,nrow(id_dao_boot[[19]])),rep(20,nrow(id_dao_boot[[20]])),
           rep(21,nrow(id_dao_boot[[21]]))
  )
  id_daob <- as.vector(as.factor(c(aux,aux)))
  rasterb <- as.vector(as.factor(c(rep(1,nrow(datab)),rep(2,nrow(datab)))))
  distanceb <- c(as.factor(datab$distance),as.factor(datab$distance))
  db <- yb[(nrow(datab)+1):(2*nrow(datab))] - yb[1:nrow(datab)]
  datab <- data.frame(yb, id_daob, rasterb, distanceb, db)
  datab$rasterb <- as.factor(datab$rasterb)
  datab$distanceb <- as.factor(datab$distanceb)
  datab$id_daob <- as.factor(datab$id_daob)
  resb[[l]] <- lmer(yb ~ rasterb+(1|id_daob)+(1|distanceb)+
                      (1|id_daob:distanceb)+(1|id_daob:rasterb)+(1|distanceb:rasterb),
                    data = datab,
                    control = lmerControl(optimizer = "bobyqa")
  )
  resdb[[l]] <- lmer(db ~ (1|id_daob) + (1|distanceb),
                     data = datab,
                     control = lmerControl(optimizer = "bobyqa")
  )
  resd1b[[l]] <- lmer(db ~ 1 + (1|id_daob),
                      data = datab,
                      control = lmerControl(optimizer = "bobyqa")
  )
}
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
m <- (data$y[1:385] + data$y[386:770])/2
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
