rm(list = ls())


# Functions ---------------------------------------------------------------

source("scripts/Functions/save_data.R")



# Load Data ---------------------------------------------------------------



# Descriptive table -------------------------------------------------------
gs_all_list <- readr::read_rds("DATA/Processed/Aim2/Agreement/gs_all_list.rds")

aim2_desc <- gs_all_list[[1]] %>%
  dplyr::mutate(distance = distance %>% stringr::str_pad(4, pad = "0")) %>%
  dplyr::arrange(distance) %>%
  tidyr::pivot_wider(names_from = distance ,
                     values_from = greenspace,
                     names_prefix = "gs_")
save_data(aim2_desc,
          "DATA/Processed/Aim2/Agreement/aim2_desc",
          "DATA/Processed/Aim2/Agreement/Archived/aim2_desc",
          xlsx = TRUE)

# Export and run sas code for descriptive table
# scripts/204a_desc_table.sas


# Scatter plots & equality line -------------------------------------------

gs_all_pair_list <- readr::read_rds("DATA/Processed/Aim2/Agreement/gs_all_pair_list.rds")

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
                              paste0("outputs/figures/Aim2/scatterplot_nejm_",
                                     .y,
                                     ".jpg"),
                              device = "jpeg",
                              width = 7,
                              height = 7,
                              units = "in"))
tictoc::toc()

# Agreement table ---------------------------------------------------------
agreement_stat_df <- readr::read_rds("DATA/Processed/Aim2/Agreement/agreement_stat_df_pairwise_allmonths.rds")
quantile_list <- readr::read_rds("DATA/Processed/Aim2/Agreement/Bootstrap/quantile_list.rds")


# Create a reference df for raster pair
pair_df <- tibble::tibble(landsat_26953 = c(0,1,1),
                          nlcd_26953 = c(1,0,1),
                          modis_26953 = c(1,1,0),
                          pair = c("MODIS & NLCD", "Landsat 8 & MODIS", "Landsat 8 & NLCD"))

# Create a reference table for agreement stats name
agreement_full_desc_df <- tibble::tibble(
  rowname = c("meanb",  "lcl", "ucl", "msd", "cp_05", "cp_10", "tdi_05", "tdi_10", "ccc"),
  `Agreement Statistics` = c("Limits of agreement (LOA)",
                             "Lower limit of agreement (LOA + 1.96SD)",
                             "Upper limit of agreement (LOA - 1.96SD)",
                             "Mean squared deviation (MSD)",
                             "Coverage Probability at greenspace of 0.05 (CP 0.05)",
                             "Coverage Probability at greenspace of 0.10 (CP 0.10)",
                             "Total deviation index at greenspace of 0.05 (TDI 0.05)",
                             "Total deviation index at greenspace of 0.10 (TDI 0.10)",
                             "Concordance Correlation Coefficient (CCC)"))


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
                pair, month)

quantile_df <- quantile_list %>%
  # select only needed stats
  purrr::map(~dplyr::select(.x, c(meanb, lcl, ucl,
                msd, cp_05, cp_10, tdi_05, tdi_10, ccc,
                pair)) %>%
               t() %>% tibble::as_tibble(rownames = NA) %>%
               tibble::rownames_to_column() %>%
               dplyr::mutate(ci = stringr::str_c("(", sprintf("%.4f", as.numeric(V1)),
                                                 " \u2013 ", sprintf("%.4f", as.numeric(V2)), ")")) %>%
               dplyr::mutate(ci = ifelse(rowname == "pair", V1, ci)) %>%
               dplyr::select(-c(V1, V2))
             ) %>%
  purrr::map2(c("MODIS & NLCD", "Landsat 8 & MODIS", "Landsat 8 & NLCD"),
             ~.x %>% dplyr::rename({{.y}} := ci)) %>%
  purrr::reduce(dplyr::full_join, by = "rowname")

agreement_table <- agreement_stat_df %>%
  # filter to agreement stats for all months
  dplyr::filter(month == "00") %>%
  # Transpose and rename
  t() %>% tibble::as_tibble(rownames = NA) %>%
  tibble::rownames_to_column() %>%
  dplyr::rename("MODIS & NLCD" = 2,
                "Landsat 8 & MODIS" = 3,
                "Landsat 8 & NLCD" = 4)

agreement_table_long <- dplyr::bind_rows(agreement_table,
                                         quantile_df %>%
                                           dplyr::mutate(rowname = rowname %>% paste0("_ci"))) %>%
  tibble::rowid_to_column() %>%
  dplyr::mutate(order = ifelse(rowid < 12, rowid*2-1, (rowid-11)*2)) %>%
  dplyr::arrange(order)

agreement_table_wide <- dplyr::full_join(agreement_table, quantile_df,
                                         by = "rowname", suffix = c(" (point)", " (CI)")) %>%
  dplyr::filter(!(rowname %in% c("pair", "month"))) %>%
  dplyr::mutate(
    `MODIS & NLCD` = paste0(sprintf("%.4f", as.numeric(`MODIS & NLCD (point)`)),
                            " ", `MODIS & NLCD (CI)`),
    `Landsat 8 & MODIS` = paste0(sprintf("%.4f", as.numeric(`Landsat 8 & MODIS (point)`)),
                                 " ", `Landsat 8 & MODIS (CI)`),
    `Landsat 8 & NLCD` = paste0(sprintf("%.4f", as.numeric(`Landsat 8 & NLCD (point)`)),
                                " ", `Landsat 8 & NLCD (CI)`)) %>%
  dplyr::left_join(agreement_full_desc_df, by = "rowname") %>%
  dplyr::select(c("Agreement Statistics", "MODIS & NLCD", "Landsat 8 & MODIS", "Landsat 8 & NLCD"))

save_data(agreement_table_wide,
          "DATA/Processed/Aim2/Agreement/agreement_table_wide",
          "DATA/Processed/Aim2/Agreement/Archived/agreement_table_wide",
          csv = FALSE, xlsx = TRUE)




# Plot agreement stats by month -------------------------------------------

# Create a reference table for agreement stats name
agreement_desc_df <- tibble::tibble(
  agreement_stats = c("meanb", "msd", "cp_05", "cp_10", "tdi_05", "tdi_10", "ccc"),
  `Agreement Statistics` = c("LOA", "MSD", "CP 0.05", "CP 0.10", "TDI 0.05", "TDI 0.10", "CCC")
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
      dplyr::filter(`Agreement Statistics` %in% c("LOA", "MSD", "TDI 0.05", "TDI 0.10"))
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
                                    paste0("outputs/figures/Aim2/agreementbymonth_",
                                           .y,
                                           ".jpg"),
                                    device = "jpeg",
                                    width = 5,
                                    height = 5,
                                    units = "in"))
    plot_list
    })



# Bland-Altman plots ------------------------------------------------------

gs_all_pair_list <- readr::read_rds("DATA/Processed/Aim2/Agreement/gs_all_pair_list.rds")
quantile_list <- readr::read_rds("DATA/Processed/Aim2/Agreement/Bootstrap/quantile_list.rds")
agreement_stat_df <- "DATA/Processed/Aim2/Agreement/agreement_stat_df_pairwise_allmonths.rds"


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
      # ggplot2::ylim(-0.6, 0.6) +
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
      ggplot2::theme(legend.position = c(.95, .95),
                     legend.justification = c("right", "top"),
                     legend.box.just = "left") +
      ggplot2::theme(legend.background = ggplot2::element_rect(fill="gray90")) +
      ggplot2::xlab(paste0("Average of the greenspace measurements from ", quantile_df$pair[1])) +
      ggplot2::ylab(paste0("Difference in greenspace measurements: ", ylab))

    # Export
    ggplot2::ggsave(paste0("outputs/figures/Aim2/blandaltman_",
                           suffix,
                           ".jpg"),
                    device = "jpeg",
                    width = 6,
                    height = 6,
                    units = "in")
  })
