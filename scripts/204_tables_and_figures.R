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
  purrr::map(.f = function(list){
    plot_list <- list %>%
      purrr::map(.f = function(df){
        ggplot2::ggplot(df) +
          ggplot2::geom_point(ggplot2::aes(x = get(names(df)[4]),
                                           y = get(names(df)[5]),
                                           colour = `Radius (m)`),
                              shape = 4) +
          ggplot2::xlim(0, 0.6) +
          ggplot2::ylim(0, 0.6) +
          ggplot2::geom_abline(slope = 1, intercept = 0) +
          ggplot2::xlab(names(df)[4]) +
          ggplot2::ylab(names(df)[5]) +
          ggplot2::theme_bw()+
          ggplot2::theme(legend.position = c(.95, .95),
                         legend.justification = c("right", "top"),
                         legend.box.just = "left") +
          ggplot2::theme(legend.background = ggplot2::element_rect(fill="gray90")) +
          ggsci::scale_color_nejm()})
    plot_list
  })

p1 <- scatterplot[[1]][[1]] +
  ggplot2::xlab("MODIS") +
  ggplot2::ylab("NLCD")
p2 <- scatterplot[[1]][[2]] +
  ggplot2::xlab("Landsat 8") +
  ggplot2::ylab("MODIS")
p3 <- scatterplot[[1]][[3]] +
  ggplot2::xlab("Landsat 8") +
  ggplot2::ylab("NLCD")



list(p1, p2, p3) %>%
  purrr::map2(c(1:3),
              ~ggplot2::ggsave(plot = .x,
                              paste0("outputs/figures/Aim2/scatterplot_nejm_",
                                     .y,
                                     ".jpg"),
                              device = "jpeg",
                              width = 7,
                              height = 7,
                              units = "in"))


# Agreement table ---------------------------------------------------------
agreement_stat_df <- "DATA/Processed/Aim2/Agreement/agreement_stat_df_pairwise_allmonths.rds"

pair_df <- tibble::tibble(landsat_26953 = c(0,1,1),
                          modis_26953 = c(1,0,1),
                          nlcd_26953 = c(1,1,0),
                          pair = c("MODIS & NLCD", "Landsat 8 & MODIS", "Landsat 8 & NLCD"))

agreement_stat_df <- agreement_stat_df %>%
  readr::read_rds() %>%
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

agreement_table <- agreement_stat_df %>%
  # filter to agreement stats for all months
  dplyr::filter(month == "00") %>%
  # Transpose and rename
  t() %>% tibble::as_tibble(rownames = NA) %>%
  tibble::rownames_to_column() %>%
  dplyr::rename("MODIS & NLCD" = 2,
                "Landsat 8 & MODIS" = 3,
                "Landsat 8 & NLCD" = 4)

save_data(agreement_table,
          "DATA/Processed/Aim2/Agreement/agreement_table",
          "DATA/Processed/Aim2/Agreement/Archived/agreement_table")




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
plot <- agreement_by_month %>%
  purrr::walk(function(df) {
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
                                               colour = `Agreement Statistics`)) +
                   ggplot2::geom_point() +
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
      purrr::walk2(suffix,
                   ~ggplot2::ggsave(plot = .x,
                                    paste0("outputs/figures/Aim2/agreementbymonth_",
                                           .y,
                                           ".jpg"),
                                    device = "jpeg",
                                    width = 5,
                                    height = 5,
                                    units = "in"))
    })



# Bland-Altman plots ------------------------------------------------------
