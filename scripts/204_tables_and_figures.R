rm(list = ls())


# Functions ---------------------------------------------------------------

source("scripts/Functions/save_data.R")



# Load Data ---------------------------------------------------------------

agreement_stat_df <- "DATA/Processed/Aim2/Agreement/agreement_stat_df_pairwise_allmonths.rds"


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
          # ggplot2::theme(legend.position="bottom")
          ggplot2::theme(legend.position = c(.05, .95),
                         legend.justification = c("left", "top"),
                         legend.box.just = "left",
                         legend.margin = margin(6, 6, 6, 6)) +
          ggplot2::theme_bw() +
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
  purrr::map(~ggplot2::ggsave(plot = .x,
                              paste0("outputs/figures/Aim2/scatterplot_nejm_A",
                                     deparse(substitute(.x))
                                     ".jpg"),
                              device = "jpeg",
                              width = 9,
                              height = 6,
                              units = "in"))

p2





# Agreement table ---------------------------------------------------------


# Bland-Altman plots ------------------------------------------------------




# Plot agreement stats by month -------------------------------------------


