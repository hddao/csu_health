rm(list = ls())

# Functions ---------------------------------------------------------------

source("scripts/Functions/save_data.R")



# Load Data ---------------------------------------------------------------



# Descriptive table -------------------------------------------------------
gs_all_list <- readr::read_rds("DATA/Processed/Aim2/Agreement_summer/gs_all_list.rds")

aim2_desc <- gs_all_list[[1]] %>%
  dplyr::mutate(distance = distance %>% stringr::str_pad(4, pad = "0")) %>%
  dplyr::arrange(distance) %>%
  tidyr::pivot_wider(names_from = distance ,
                     values_from = greenspace,
                     names_prefix = "gs_")
save_data(aim2_desc,
          "DATA/Processed/Aim2/Agreement_summer/aim2_desc",
          "DATA/Processed/Aim2/Agreement_summer/Archived/aim2_desc",
          xlsx = TRUE)

# Export and run sas code for descriptive table
# scripts/204a_desc_table.sas


# Stats for txt
aim2_desc_text <- gs_all_list[[1]] %>%
  dplyr::filter(month == "All months") %>%
  dplyr::group_by(raster) %>%
  dplyr::summarise(gs_mean = mean(greenspace), gs_sd = sd(greenspace))
save_data(aim2_desc_text,
          "DATA/Processed/Aim2/Agreement_summer/aim2_desc_text",
          "DATA/Processed/Aim2/Agreement_summer/Archived/aim2_desc_text")


# Desc table
aim2_desc_table <- aim2_desc %>%
  dplyr::select(-id_dao) %>%
  dplyr::group_split(raster) %>%
  purrr::map(vtable::st, out = "csv", add.median = TRUE) %>%
  purrr::map(~.x %>%
               dplyr::filter(Variable %>% stringr::str_detect("gs_")) %>%
               dplyr::mutate_at(dplyr::vars(-Variable), as.numeric) %>%
               dplyr::mutate(`Mean+_SD` = paste0(sprintf("%.3f", Mean),
                                                 "\u00b1",
                                                 sprintf("%.3f", `Std. Dev.`)),
                             Median = sprintf("%.3f", `Pctl. 50`),
                             `Q1-Q3` = paste0(sprintf("%.3f", `Pctl. 25`),
                                              "-",
                                              sprintf("%.3f", `Pctl. 75`)),
                             `Median (IQR)` = paste0(Median,
                                                     " (",
                                                     `Q1-Q3`,
                                                     ")"),
                             `Min-Max` = paste0(sprintf("%.3f", Min), "-", sprintf("%.3f", Max)),
                             n = N %>% as.character(),
                             `Buffer radius (m)` = Variable %>%
                               stringr::str_sub(-4, -1) %>%
                               as.numeric() %>%
                               paste0("m")
                               ) %>%
               dplyr::select(Variable, `Buffer radius (m)`, n,
                             `Mean+_SD`, Median, `Q1-Q3`, `Median (IQR)`, `Min-Max`) %>%
               tidyr::pivot_longer(cols = `Buffer radius (m)`:`Min-Max`,
                                   names_to = "stats",
                                   values_to = "value") %>%
               dplyr::mutate(stats = ifelse(stats == "Buffer radius (m)", value, paste0("  ", stats)),
                             value = ifelse(stats == value, "", value)) %>%
               dplyr::select(-Variable)
               ) %>%
  purrr::map2(c("Landsat 8 (n=21,950)", "MODIS (n=21,950)", "NLCD (n=21,950)"),
                ~.x %>% dplyr::rename({{.y}} := value)) %>%
  dplyr::bind_cols() %>%
  dplyr::select(-c(3,5)) %>%
  dplyr::rename(`Greenspace buffer radii` = `stats...1`) %>%
  dplyr::mutate(`Greenspace buffer radii` = `Greenspace buffer radii` %>%
                  dplyr::recode("  Mean+_SD" = "  Mean\u00b1SD"))


aim2_desc_table_1 <- aim2_desc_table %>%
  dplyr::filter(!(`Greenspace buffer radii` %in% c("  Median (IQR)", "  n")))
aim2_desc_table_2 <- aim2_desc_table %>%
  dplyr::filter(!(`Greenspace buffer radii` %in% c("  Median", "  Q1-Q3", "  n")))


save_data(aim2_desc_table_1,
          "DATA/Processed/Aim2/Agreement_summer/aim2_desc_table_1",
          "DATA/Processed/Aim2/Agreement_summer/Archived/aim2_desc_table_1",
          xlsx = TRUE, csv = FALSE)
save_data(aim2_desc_table_2,
          "DATA/Processed/Aim2/Agreement_summer/aim2_desc_table_2",
          "DATA/Processed/Aim2/Agreement_summer/Archived/aim2_desc_table_2",
          xlsx = TRUE, csv = FALSE)

# Histogram ---------------------------------------------------------------
gs_all_list <- readr::read_rds("DATA/Processed/Aim2/Agreement_summer/gs_all_list.rds")

gs_00_df <- gs_all_list[[1]] %>%
  dplyr::mutate(raster = raster %>% dplyr::recode("landsat_26953" = "Landsat 8",
                                                  "nlcd_26953" = "NLCD",
                                                  "modis_26953" = "MODIS")) %>%
  dplyr::rename(`Greenspace measurement` = greenspace,
                `Greenspace source` = raster,
                `Buffer radius (m)` = distance) %>%
  dplyr::mutate(`Buffer radius (m)` = `Buffer radius (m)` %>%
                  as.character())

mu <- plyr::ddply(gs_00_df, "`Greenspace source`",
                  dplyr::summarise, grp.mean=mean(`Greenspace measurement`))

# Histogram by raster
p<-ggplot2::ggplot(gs_00_df, ggplot2::aes(x=`Greenspace measurement`, color=`Greenspace source`)) +
  ggplot2::geom_histogram(fill="white", position="dodge")+
  ggplot2::geom_vline(data=mu, ggplot2::aes(xintercept=grp.mean, color=`Greenspace source`),
             linetype="dashed") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = c(0.95, 0.95),
                 legend.justification = c("right", "top"),
                 legend.box.just = "left") +
  ggplot2::theme(legend.background = ggplot2::element_rect(fill="gray90")) +
  # ggplot2::theme(legend.position="bottom") +
  ggsci::scale_color_nejm() +
  ggplot2::ylab("Count")

ggplot2::ggsave(plot = p,
                "outputs/figures/Aim2/summer/histogram_by_raster.jpg",
                device = "jpeg",
                width = 6.5,
                height = 4,
                units = "in")

# Histogram by buffer radius
hist_list <- c("25", "50", "100", "250", "500", "1000") %>%
  purrr::map(function(x) {
    ggplot2::ggplot(gs_00_df %>%
                      dplyr::filter(`Buffer radius (m)` == x),
                    ggplot2::aes(x=`Greenspace measurement`, color=`Buffer radius (m)`)) +
      ggplot2::geom_histogram(fill="white", position="dodge", show.legend = FALSE)+
      ggplot2::theme_bw() +
      ggsci::scale_color_nejm() +
      ggplot2::xlim(0, 0.6) +
      ggplot2::ylab("Count") +
      ggplot2::xlab(paste0("Greenspace measurement with buffer radius of ", x, "m")) +
      ggplot2::scale_y_continuous(breaks = seq(0, 12000, by = 2000),
                                  limits = c(0, 11000))
  })
p <- gridExtra::arrangeGrob(hist_list[[1]], hist_list[[2]], hist_list[[3]],
                       hist_list[[4]], hist_list[[5]], hist_list[[6]],
                       nrow=2, ncol=3)
ggplot2::ggsave(plot = p,
                "outputs/figures/Aim2/summer/histogram_by_radius.jpg",
                device = "jpeg",
                width = 15,
                height = 10,
                units = "in")


# Scatter plots & equality line -------------------------------------------

gs_all_pair_list <- readr::read_rds("DATA/Processed/Aim2/Agreement_summer/gs_all_pair_list.rds")

gs_desc_df <- gs_all_pair_list[1] %>%
  purrr::map(function(x){
    purrr::map(x, ~.x %>% tidyr::pivot_wider(names_from = raster,
                                             values_from = greenspace) %>%
                 dplyr::rename(`Radius (m)` = distance))
  })

scatterplot <- gs_desc_df %>%
  purrr::map(function(list){
    plot_list <- list %>%
      purrr::map(function(df){
        ggplot2::ggplot(df) +
          ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey50", size = 1.0) +
          ggplot2::geom_point(ggplot2::aes(x = get(names(df)[4]),
                                           y = get(names(df)[5]),
                                           colour = `Radius (m)`),
                              shape = 4, size = 0.8) +
          ggplot2::xlim(0, 0.6) +
          ggplot2::ylim(0, 0.6) +
          ggplot2::xlab(names(df)[4]) +
          ggplot2::ylab(names(df)[5]) +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.position = c(0.95, 0.05),
                         legend.justification = c("right", "bottom"),
                         legend.box.just = "left") +
          ggplot2::theme(legend.background = ggplot2::element_rect(fill="gray90")) +
          ggsci::scale_color_nejm()})
    plot_list
  })

scatterplot[[1]][[1]] <- scatterplot[[1]][[1]] + ggplot2::xlab("MODIS") + ggplot2::ylab("NLCD") +
  ggplot2::theme(legend.position = c(0.05, 0.95),
                 legend.justification = c("left", "top"),
                 legend.box.just = "left")
scatterplot[[1]][[2]] <- scatterplot[[1]][[2]] + ggplot2::xlab("Landsat 8") + ggplot2::ylab("MODIS")
scatterplot[[1]][[3]] <- scatterplot[[1]][[3]] + ggplot2::xlab("Landsat 8") + ggplot2::ylab("NLCD")

tictoc::tic("ggsave")
scatterplot[[1]] %>%
  purrr::map2(c(1:3),
              ~ggplot2::ggsave(plot = .x,
                              paste0("outputs/figures/Aim2/summer/scatterplot_nejm_",
                                     .y,
                                     ".jpg"),
                              device = "jpeg",
                              width = 7,
                              height = 7,
                              units = "in"))
tictoc::toc()


# Variance Components -----------------------------------------------------

lmer3_res <- readr::read_rds("DATA/Processed/Aim2/Agreement_summer/lmer3_res.rds")

# Create a function to get agreement stats from lmer model summary
create_agreement_stats <- function(res_sum,
                                   p = c(0.9, 0.95), delta = c(0.05, 0.1), alpha = 0.05) {
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

    # Exported df
  var_stat_df <- tibble::tibble(beta2.est,
                                sigma2.alpha.est,
                                sigma2.gamma.est,
                                sigma2.alpha.gamma.est,
                                sigma2.alpha.beta.est,
                                sigma2.beta.gamma.est,
                                sigma2.epsilon.est,
                                phi2.beta.est,
                                CCC, MSD, TDI, CP, CIA) %>%
    dplyr::rename_all(tolower) %>%
    # change cp and tdi from list to numeric
    dplyr::mutate(cp_05 = cp %>% purrr::map(dplyr::first) %>% as.numeric(),
                  cp_10 = cp %>% purrr::map(dplyr::last) %>% as.numeric(),
                  tdi_05 = tdi %>% purrr::map(dplyr::first) %>% as.numeric(),
                  tdi_10 = tdi %>% purrr::map(dplyr::last) %>% as.numeric())
}

# Create a reference table for agreement stats name
agreement_full_desc_df <- tibble::tibble(
  rowname = c("meanb",  "lcl", "ucl", "msd", "cp_05", "cp_10", "tdi_05", "tdi_10", "ccc",
              "sigma2.alpha.est", "sigma2.gamma.est", "sigma2.alpha.gamma.est",
              "sigma2.alpha.beta.est", "sigma2.beta.gamma.est", "sigma2.epsilon.est",
              "phi2.beta.est"),
  `Agreement Statistics` = c("Limits of agreement (LOA)",
                             "Lower limit of agreement (LOA + 1.96SD)",
                             "Upper limit of agreement (LOA - 1.96SD)",
                             "Mean squared deviation (MSD)",
                             "Coverage Probability at greenspace of 0.05 (CP 0.05)",
                             "Coverage Probability at greenspace of 0.10 (CP 0.10)",
                             "Total deviation index at 90% probability (TDI 0.90)",
                             "Total deviation index at 95% probability (TDI 0.95)",
                             "Concordance Correlation Coefficient (CCC)",
                             "Variance from subject",
                             "Variance from buffer radius",
                             "Variance from subject & buffer radius interaction",
                             "Variance from subject & raster interaction",
                             "Variance from buffer radius & raster interaction",
                             "Variance from error term",
                             "Variance from raster"))

agreement_stat_3_df <- lmer3_res[[13]][[2]] %>%
  create_agreement_stats() %>%
  # select only needed stats
  dplyr::select(msd, cp_05, cp_10, tdi_05, tdi_10, ccc,
                sigma2.alpha.est, sigma2.gamma.est, sigma2.alpha.gamma.est,
                sigma2.alpha.beta.est, sigma2.beta.gamma.est, sigma2.epsilon.est,
                phi2.beta.est) %>%
  t() %>%
  tibble::as_tibble(rownames = NA) %>%
  tibble::rownames_to_column() %>%
  dplyr::left_join(agreement_full_desc_df, by = "rowname")


save_data(agreement_stat_3_df,
          "DATA/Processed/Aim2/Agreement_summer/agreement_stat_3_df",
          "DATA/Processed/Aim2/Agreement_summer/Archived/agreement_stat_3_df")



# Agreement table ---------------------------------------------------------
agreement_stat_df <- readr::read_rds("DATA/Processed/Aim2/Agreement_summer/agreement_stat_df_pairwise_allmonths.rds")
quantile_list <- readr::read_rds("DATA/Processed/Aim2/Agreement_summer/quantile_list.rds")


# Create a reference df for raster pair
pair_df <- tibble::tibble(landsat_26953 = c(0,1,1),
                          nlcd_26953 = c(1,0,1),
                          modis_26953 = c(1,1,0),
                          pair = c("MODIS & NLCD", "MODIS & Landsat 8", "Landsat 8 & NLCD"))

# Create a reference table for agreement stats name
agreement_full_desc_df <- tibble::tibble(
  rowname = c("meanb",  "lcl", "ucl", "msd", "cp_05", "cp_10", "tdi_05", "tdi_10", "ccc",
              "sigma2.alpha.est", "sigma2.gamma.est", "sigma2.alpha.gamma.est",
              "sigma2.alpha.beta.est", "sigma2.beta.gamma.est", "sigma2.epsilon.est",
              "phi2.beta.est"),
  `Agreement Statistics` = c("Limits of agreement (LOA)",
                             "Lower limit of agreement (LOA + 1.96SD)",
                             "Upper limit of agreement (LOA - 1.96SD)",
                             "Mean squared deviation (MSD)",
                             "Coverage Probability at greenspace of 0.05 (CP 0.05)",
                             "Coverage Probability at greenspace of 0.10 (CP 0.10)",
                             "Total deviation index at 90% probability (TDI 0.90)",
                             "Total deviation index at 95% probability (TDI 0.95)",
                             "Concordance Correlation Coefficient (CCC)",
                             "Variance from subject",
                             "Variance from buffer radius",
                             "Variance from subject & buffer radius interaction",
                             "Variance from subject & raster interaction",
                             "Variance from buffer radius & raster interaction",
                             "Variance from error term",
                             "Variance from raster"))

agreement_stat_df <- agreement_stat_df %>%
  dplyr::rename_all(tolower) %>%
  # change cp and tdi from list to numeric
  dplyr::mutate(cp_05 = cp %>% purrr::map(dplyr::first) %>% as.numeric(),
                cp_10 = cp %>% purrr::map(dplyr::last) %>% as.numeric(),
                tdi_05 = tdi %>% purrr::map(dplyr::first) %>% as.numeric(),
                tdi_10 = tdi %>% purrr::map(dplyr::last) %>% as.numeric()) %>%
  # Add pair value
  dplyr::left_join(pair_df, by = c("landsat_26953", "modis_26953", "nlcd_26953")) %>%
  # select only needed stats
  dplyr::select(meanb, lcl, ucl,
                msd, cp_05, cp_10, tdi_05, tdi_10, ccc,
                sigma2.alpha.est, sigma2.gamma.est, sigma2.alpha.gamma.est,
                sigma2.alpha.beta.est, sigma2.beta.gamma.est, sigma2.epsilon.est,
                phi2.beta.est,
                pair, month)
# flip the sign for the pair "MODIS - Landsat 8"
agreement_stat_df[agreement_stat_df$pair == "MODIS & Landsat 8", ] <- agreement_stat_df[agreement_stat_df$pair == "MODIS & Landsat 8", ] %>%
  dplyr::mutate_at(c("meanb", "lcl", "ucl"), function(x) {x <- (0 - x)})


# flip the sign for the pair "MODIS - Landsat 8"
quantile_list[[2]] <- quantile_list[[2]] %>%
  dplyr::mutate_at(c("meanb", "lcl", "ucl"), function(x) {x <- (0 - x)})
quantile_df <- quantile_list %>%
  # select only needed stats
  purrr::map(~dplyr::select(.x, c(meanb, lcl, ucl,
                msd, cp_05, cp_10, tdi_05, tdi_10, ccc,
                sigma2.alpha.est, sigma2.gamma.est, sigma2.alpha.gamma.est,
                sigma2.alpha.beta.est, sigma2.beta.gamma.est, sigma2.epsilon.est,
                phi2.beta.est,
                pair)) %>%
               t() %>% tibble::as_tibble(rownames = NA) %>%
               tibble::rownames_to_column() %>%
               dplyr::mutate(ci = ifelse(
                 rowname == "pair",
                 V1,
                 stringr::str_c("(", sprintf("%.4f", as.numeric(V1)),
                                " \u2013 ", sprintf("%.4f", as.numeric(V2)), ")"))) %>%
               dplyr::select(-c(V1, V2))
             ) %>%
  purrr::map2(c("MODIS & NLCD", "MODIS & Landsat 8", "Landsat 8 & NLCD"),
             ~.x %>% dplyr::rename({{.y}} := ci)) %>%
  purrr::reduce(dplyr::full_join, by = "rowname")

agreement_table <- agreement_stat_df %>%
  # filter to agreement stats for all months
  dplyr::filter(month == "00") %>%
  # Transpose and rename
  t() %>% tibble::as_tibble(rownames = NA) %>%
  tibble::rownames_to_column() %>%
  dplyr::rename("MODIS & NLCD" = 2,
                "MODIS & Landsat 8" = 3,
                "Landsat 8 & NLCD" = 4)

agreement_table_wide <- dplyr::full_join(agreement_table, quantile_df,
                                         by = "rowname", suffix = c(" (point)", " (CI)")) %>%
  dplyr::filter(!(rowname %in% c("pair", "month"))) %>%
  dplyr::mutate(
    `MODIS & NLCD` = paste0(sprintf("%.4f", as.numeric(`MODIS & NLCD (point)`)),
                            " ", `MODIS & NLCD (CI)`),
    `MODIS & Landsat 8` = paste0(sprintf("%.4f", as.numeric(`MODIS & Landsat 8 (point)`)),
                                 " ", `MODIS & Landsat 8 (CI)`),
    `Landsat 8 & NLCD` = paste0(sprintf("%.4f", as.numeric(`Landsat 8 & NLCD (point)`)),
                                " ", `Landsat 8 & NLCD (CI)`)) %>%
  dplyr::left_join(agreement_full_desc_df, by = "rowname") %>%
  dplyr::select(c("Agreement Statistics", "MODIS & NLCD", "MODIS & Landsat 8", "Landsat 8 & NLCD"))

save_data(agreement_table_wide,
          "DATA/Processed/Aim2/Agreement_summer/agreement_table_wide",
          "DATA/Processed/Aim2/Agreement_summer/Archived/agreement_table_wide",
          csv = FALSE, xlsx = TRUE)



# Plot agreement stats by month -------------------------------------------

# Create a reference table for agreement stats name
agreement_desc_df <- tibble::tibble(
  agreement_stats = c("meanb", "msd", "cp_05", "cp_10", "tdi_05", "tdi_10", "ccc"),
  `Agreement Statistics` = c("LOA", "MSD", "CP 0.05", "CP 0.10", "TDI 0.90", "TDI 0.95", "CCC")
)

# Clean agreement stats for ggplot
agreement_by_month <- agreement_stat_df %>%
  # Get absolute value for LOA (meanb)
  dplyr::mutate(meanb = abs(meanb)) %>%
  # Remove data for all months
  dplyr::filter(month != "00") %>%
  # pitvot table
  tidyr::pivot_longer(
    cols = meanb:ccc,
    names_to = "agreement_stats",
    values_to = "Value"
  ) %>%
  # get the name for agreement stats
  dplyr::left_join(agreement_desc_df, by = "agreement_stats") %>%
  dplyr::filter(!is.na(`Agreement Statistics`)) %>%
  # Rename for later ggplot
  dplyr::rename(Month = month) %>%
  # Split by pair
  dplyr::arrange(pair) %>%
  dplyr::group_split(pair) %>%
  setNames(unique(agreement_stat_df$pair))

# Plot agreementbymonth and export
plot_bymonth <- agreement_by_month %>%
  purrr::map(function(df) {
    # split data to scale and unscale stats
    df_scale <- df %>%
      dplyr::filter(`Agreement Statistics` %in% c("CCC", "CP 0.05", "CP 0.10"))
    df_unscale <- df %>%
      dplyr::filter(`Agreement Statistics` %in% c("LOA", "MSD", "TDI 0.95", "TDI 0.90"))
    # Create plots for agreement by months
    plot_list <- list(df_scale, df_unscale) %>%
      purrr::map(~ggplot2::ggplot(data = .x,
                                  ggplot2::aes(x = Month,
                                               y = Value,
                                               group = `Agreement Statistics`,
                                               colour = `Agreement Statistics`)) +
                   ggplot2::geom_point() +
                   ggplot2::geom_line() +
                   ggplot2::ggtitle(df$pair[1]) +
                   ggplot2::theme_bw() +
                   ggsci::scale_color_nejm() +
                   ggplot2::theme(legend.position = "bottom"))
    # Rescale
    plot_list[[1]] <- plot_list[[1]] + ggplot2::ylim(0, 0.5)
    plot_list[[2]] <- plot_list[[2]] + ggplot2::ylim(0, 1)
    # Export
    suffix <- df$pair[1] %>%
      stringr::str_replace_all(pattern = " ", replacement = "_") %>%
      rep(2) %>%
      stringr::str_c(c("_scale", "_unscale"))
    plot_list %>%
      purrr::map2(suffix,
                   ~ggplot2::ggsave(plot = .x,
                                    paste0("outputs/figures/Aim2/summer/agreementbymonth_",
                                           .y,
                                           ".jpg"),
                                    device = "jpeg",
                                    width = 5,
                                    height = 5,
                                    units = "in"))
    plot_list
    })



# Bland-Altman plots ------------------------------------------------------

gs_all_pair_list <- readr::read_rds("DATA/Processed/Aim2/Agreement_summer/gs_all_pair_list.rds")
quantile_list <- readr::read_rds("DATA/Processed/Aim2/Agreement_summer/quantile_list.rds")
agreement_stat_df <- "DATA/Processed/Aim2/Agreement_summer/agreement_stat_df_pairwise_allmonths.rds"


# # Get color palette
# scales::show_col(ggsci::pal_nejm(alpha = 0.7)(8))
# scales::show_col(ggsci::pal_jama(alpha = 0.7)(7))

# nejm
red_color <- "#BC3C29B2"
blue_color <- "#0072B5B2"

# # jama
# red_color <- "#B24745B2"
# blue_color <- "#00A1D5B2"


# Create gs_diff_list
gs_diff_list <- gs_all_pair_list[[1]] %>%
  purrr::map(~.x %>%
               # Edit df to create variable d
               tidyr::pivot_wider(names_from = raster,
                                  values_from = greenspace) %>%
               dplyr::mutate(d = .[[4]] - .[[5]],
                             m = (.[[4]] + .[[5]])/2) %>%
               dplyr::rename(`Radius (m)` = distance))
# flip the sign for the pair "MODIS - Landsat 8"
gs_diff_list[[2]] <- gs_diff_list[[2]] %>%
  dplyr::mutate(d = 0 - d)

# Create agreement_list
agreement_list <- agreement_stat_df %>%
  readr::read_rds() %>%
  dplyr::rename_all(base::tolower) %>%
  # filter to agreement stats for all months
  dplyr::filter(month == "00") %>%
  # change cp and tdi from list to numeric
  dplyr::mutate(cp_05 = cp %>% purrr::map(dplyr::first) %>% as.numeric(),
                cp_10 = cp %>% purrr::map(dplyr::last) %>% as.numeric(),
                tdi_05 = tdi %>% purrr::map(dplyr::first) %>% as.numeric(),
                tdi_10 = tdi %>% purrr::map(dplyr::last) %>% as.numeric()) %>%
  # Drop list column
  dplyr::select(-c(tdi, cp)) %>%
  # Arrange by raster pair
  dplyr::arrange(landsat_26953, nlcd_26953, modis_26953) %>%
  # Split
  dplyr::group_split(landsat_26953, nlcd_26953, modis_26953)
# flip the sign for the pair "MODIS - Landsat 8"
agreement_list[[2]] <- agreement_list[[2]] %>%
  dplyr::mutate_at(c("meanb", "lcl", "ucl", "mean_raw", "ll_raw", "ul_raw"),
                   function(x) {x <- (0 - x)})


# flip the sign for the pair "MODIS - Landsat 8"
quantile_list[[2]] <- quantile_list[[2]] %>%
  dplyr::mutate_at(c("meanb", "lcl", "ucl", "mean_raw", "ll_raw", "ul_raw"),
                   function(x) {x <- (0 - x)})



# Create ylab_list
ylab_list <- c("MODIS - NLCD", "MODIS - Landsat 8", "Landsat 8 - NLCD")

# Create suffix_list
suffix_list <- c("MODIS - NLCD", "MODIS - Landsat 8", "Landsat 8 - NLCD") %>%
  stringr::str_replace_all(pattern = " ", replacement = "_")


# Create map_df for purrr::pwalk()
map_df <- tibble::tibble(gs_diff_df = gs_diff_list,
                         agreement_df = agreement_list,
                         quantile_df = quantile_list,
                         ylab = ylab_list,
                         suffix = suffix_list)

# Create BA plots and export with purrr::pwalk()
ba_plot <- map_df %>%
  purrr::pwalk(function(gs_diff_df, agreement_df, quantile_df, ylab, suffix) {
    ggplot2::ggplot() +
      ggplot2::theme_bw() +
      ggplot2::xlim(0, 0.6) +
      ggplot2::ylim(-0.6, 0.6) +
      # Plot Bland-Altman lines
      # Horizontal guide line of y = 0
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                          colour = "grey50", size = 1.0) +
      # meanb
      ggplot2::annotate('ribbon',
                        x = c(-Inf, Inf),
                        ymin = quantile_df$meanb[1], ymax = quantile_df$meanb[2],
                        alpha = 0.2, fill = blue_color) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = agreement_df$meanb[1]),
                          colour = blue_color, size = 1.0, linetype = "longdash") +
      # ucl
      ggplot2::annotate('ribbon',
                        x = c(-Inf, Inf),
                        ymin = quantile_df$ucl[1], ymax = quantile_df$ucl[2],
                        alpha = 0.2, fill = red_color) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = agreement_df$ucl[1]),
                          colour = red_color, size = 1.0, linetype = "longdash") +
      # lcl
      ggplot2::annotate('ribbon',
                        x = c(-Inf, Inf),
                        ymin = quantile_df$lcl[1], ymax = quantile_df$lcl[2],
                        alpha = 0.2, fill = red_color) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = agreement_df$lcl[1]),
                          colour = red_color, size = 1.0, linetype = "longdash") +
      # Add point data
      ggplot2::geom_point(data = gs_diff_df,
                          ggplot2::aes(x = m,
                                       y = d,
                                       colour = `Radius (m)`),
                          shape = 4, size = 0.4,
                          ) +
      ggplot2::theme(legend.position = c(0.95, 0.05),
                     legend.justification = c("right", "bottom"),
                     legend.box.just = "left") +
      ggplot2::theme(legend.background = ggplot2::element_rect(fill="gray90")) +
      ggplot2::xlab(paste0("Average of the greenspace measurements from ", quantile_df$pair[1])) +
      ggplot2::ylab(paste0("Difference in greenspace measurements: ", ylab))

    # Export
    ggplot2::ggsave(paste0("outputs/figures/Aim2/summer/blandaltman_",
                           suffix,
                           ".jpg"),
                    device = "jpeg",
                    width = 6,
                    height = 6,
                    units = "in")
  })
