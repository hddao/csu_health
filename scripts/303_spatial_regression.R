# Clean the environment ---------------------------------------------------
rm(list = ls())

# Functions ---------------------------------------------------------------

source("scripts/Functions/save_data.R")
source("scripts/Functions/create_folder.R")


# Create folders ----------------------------------------------------------


# Load Data ---------------------------------------------------------------
analysis <- readr::read_rds("DATA/Processed/Aim3/aim3_analysis.rds") %>%
  sf::st_as_sf() %>%
  dplyr::select(-c("x", "y", "yx")) %>%
  dplyr::mutate(grade = grade %>% as.character()) %>%
  dplyr::filter(grade %in% c("50", "80", "90")) %>%
  dplyr::mutate(ruca_str = dplyr::case_when(
    ruca == 1 ~ "1",
    ruca != 1 ~ "2+"))

analysis_bygrade <- analysis %>%
  dplyr::bind_cols(data.frame(sf::st_coordinates(analysis))) %>%
  dplyr::group_split(grade) %>%
  purrr::map(~.x %>% dplyr::distinct(studentkey, .keep_all = TRUE)) %>%
  `names<-`(c("50", "80", '90'))

varname_df <- tibble::tibble(
  term = c("ggs_ndvi_landsat_0250", "ggs_ndvi_modis_0250", "ggs_ndvi_nlcd_0250",
           "sgs_ndvi_landsat_0250", "sgs_ndvi_modis_0250", "sgs_ndvi_nlcd_0250"),
  variable_label = c(
    "Residential Landsat NDVI", "Residential MODIS NDVI", "Residential NLCD % Tree",
    "School Landsat NDVI", "School MODIS NDVI", "School NLCD % Tree"))


# Jitter the location -----------------------------------------------------


# Jitter the location for obs with same location
analysis_bygrade_jitter <- analysis_bygrade %>%
  purrr::map(function(df){
    tictoc::tic("jitter")
    df_org <- df %>%
      dplyr::select(-c("X", "Y")) %>%
      sf::st_drop_geometry()
    df <- df %>%
      dplyr::arrange(X, Y) %>%
      dplyr::group_by(X, Y) %>%
      dplyr::mutate(counter = 1:dplyr::n()) %>%
      dplyr::ungroup()
    # Number of duplication
    dups <- df %>%
      dplyr::arrange(desc(counter), X, Y) %>%
      dplyr::distinct(X, Y, .keep_all = TRUE) %>%
      dplyr::select(X, Y, counter) %>%
      dplyr::rename(counter_max = counter) %>%
      sf::st_drop_geometry()
    # Split df into dup and unique
    df_dup <- df %>%
      dplyr::select(id_dao, X, Y) %>%
      dplyr::left_join(dups, by = c("X", "Y")) %>%
      dplyr::filter(counter_max>1)
    df_unique <- df %>%
      dplyr::select(id_dao, X, Y) %>%
      dplyr::left_join(dups, by = c("X", "Y")) %>%
      dplyr::filter(counter_max==1)
    # Jitter the X of df_dup
    df_dup_jitter <- df_dup %>%
      sf::st_jitter(amount = 0.001) %>%
      dplyr::select(-c("X", "Y"))
    df_dup_all <- df_dup %>%
      sf::st_drop_geometry() %>%
      dplyr::select(-counter_max) %>%
      dplyr::rename(X_org = X, Y_org = Y) %>%
      dplyr::full_join(df_dup_jitter %>%
                         dplyr::bind_cols(data.frame(sf::st_coordinates(df_dup_jitter))),
                       by = "id_dao") %>%
      dplyr::mutate(X_diff = X - X_org,
                    Y_diff = Y - Y_org)

    # Combine df_dup_all and df_unique
    df_final <- dplyr::bind_rows(df_dup_all, df_unique) %>%
      dplyr::full_join(df_org, by = "id_dao") %>%
      sf::st_as_sf()
    tictoc::toc()
    df_final
  }
  )



# check that XY is distinct in df_jitter
analysis_bygrade_jitter %>%
  purrr::map(~(.x %>% dplyr::distinct(geometry) %>% nrow()) == (.x %>% nrow()))
# All TRUE

# check that jitter less than 0.001 m (less than 1mm) %>%
analysis_bygrade_jitter %>%
  purrr::map(function(df) {
    df <- df %>%
      dplyr::filter(!is.na(X_diff))
    paste0(all(df$X_diff < 0.001), " ", all(df$Y_diff < 0.001))
  })
# All TRUE


# Exclude low-quality geocoded locations, identified as counter_max =


# Distributions of counter
dups_dist_df <- analysis_bygrade_jitter %>%
  purrr::map2(c("50", "80", "90"),
              ~.x %>%
                dplyr::select(counter_max) %>%
                sf::st_drop_geometry() %>%
                dplyr::group_by(counter_max) %>%
                dplyr::count() %>%
                dplyr::mutate(grade = .y)) %>%
  dplyr::bind_rows()

readr::write_csv(dups_dist_df, "outputs/tables/Aim3/dups_dist.csv")


# Check assumptions -------------------------------------------------------

# outcome <- c("mathscalescore", "elascalescore")
# var <- c("ggs_ndvi_landsat_0250", "sgs_ndvi_landsat_0250",
#          "ggs_ndvi_modis_0250", "sgs_ndvi_modis_0250",
#          "ggs_ndvi_nlcd_0250", "sgs_ndvi_nlcd_0250",
#          "r_SE_nat", "ruca")
# map_df <- tidyr::crossing(outcome, var, df = analysis_bygrade_jitter)
# rm(outcome, var)
#
# create_dxplots <- function(outcome, var, df) {
#   name <- paste0(outcome, " - ", var, " - ", df[1,]$grade)
#   tictoc::tic(name)
#   # run bivar lmer
#   df <- df %>%
#     dplyr::filter(!is.na({{outcome}}) & !is.na({{var}}))
#   formula <- paste0(outcome, " ~ ", var, " + (1|cdenumber)") %>% as.formula()
#   lme <- lme4::lmer(formula, data = df, REML = TRUE)
#   # creat dx plots
#   p_redres <- redres::plot_redres(lme)
#   p_resqq <- redres::plot_resqq(lme)
#   p_ranef <- redres::plot_ranef(lme)
#   plist <- list(p_redres, p_resqq, p_ranef)
#   # p <- gridExtra::arrangeGrob(grobs = plist, nrow=2, ncol=2)
#   p <- gridExtra::arrangeGrob(plist[[1]], plist[[2]], plist[[3]],
#                               nrow=2, ncol=2)
#   ggplot2::ggsave(plot = p,
#                   paste0("outputs/figures/Aim3/lme4_dxplots_bivar_", name, ".jpg"),
#                   device = "jpeg",
#                   width = 6,
#                   height = 6,
#                   units = "in")
#   tictoc::toc()
#   plist
# }
#
# dxplots <- map_df %>% purrr::pmap(create_dxplots)
# rm(map_df)


# DAG approach ------------------------------------------------------------


# * Full mixed-effect model -----------------------------------------------

# outcome <- c("mathscalescore", "elascalescore")
# exp <- c("ggs_ndvi_landsat_0250 + sgs_ndvi_landsat_0250 + ",
#          "ggs_ndvi_modis_0250 + sgs_ndvi_modis_0250 + ",
#          "ggs_ndvi_nlcd_0250 + sgs_ndvi_nlcd_0250 + ")
# covar <- c("r_SE_nat + ruca")
#
# map_df <- tidyr::crossing(outcome, exp, covar, df = analysis_bygrade_jitter)
# name_df <- tidyr::crossing(outcome,
#                            exp = c("landsat", "modis", "nlcd"),
#                            covar,
#                            grade = c("50", "80", "90")) %>%
#   dplyr::mutate(name = paste0(outcome, " - ", exp, " - ", grade))
# rm(outcome, exp, covar)
#
# build_nlme_model <- function(outcome, exp, covar, df) {
#   tictoc::tic(paste0(outcome, " ~ ", exp, covar))
#   # Create vector of covar and exp
#   covar_chr <- covar %>% stringr::str_split(pattern = "\\+") %>%
#     dplyr::first() %>% stringr::str_trim(side = "both")
#   exp_chr <- exp %>% stringr::str_split(pattern = "\\+") %>%
#     dplyr::first() %>% stringr::str_trim(side = "both") %>%
#     purrr::discard(~.x == "")
#   # Remove NA observations
#   df <- df %>%
#     dplyr::select(c(outcome,
#                     tidyselect::all_of(covar_chr),
#                     tidyselect::all_of(exp_chr),
#                     "X", "Y", "cdenumber")) %>%
#     tidyr::drop_na()
#   # Create formula
#   formula <- paste0(outcome, " ~ ", exp, covar) %>% as.formula()
#   # Run model
#   lme_nlme <- nlme::lme(fixed = formula,
#                         random = ~1|cdenumber,
#                         data = df,
#                         method = "REML")
#   tictoc::toc()
#   lme_nlme
# }
#
# nlme <- map_df %>% purrr::pmap(build_nlme_model)
#
#
# # Check assumptions with lme4
# build_lme4_model <- function(outcome, exp, covar, df) {
#   tictoc::tic(paste0(outcome, " ~ ", exp, covar))
#   # Create vector of covar and exp
#   covar_chr <- covar %>% stringr::str_split(pattern = "\\+") %>%
#     dplyr::first() %>% stringr::str_trim(side = "both")
#   exp_chr <- exp %>% stringr::str_split(pattern = "\\+") %>%
#     dplyr::first() %>% stringr::str_trim(side = "both") %>%
#     purrr::discard(~.x == "")
#   # Remove NA observations
#   df <- df %>%
#     dplyr::select(c(outcome,
#                     tidyselect::all_of(covar_chr),
#                     tidyselect::all_of(exp_chr),
#                     "X", "Y", "cdenumber")) %>%
#     tidyr::drop_na()
#   # Create formula
#   formula <- paste0(outcome, " ~ ", exp, covar, " + (1|cdenumber)") %>% as.formula()
#   # Run model
#   lme <- lme4::lmer(formula,
#                     data = df,
#                     REML = TRUE)
#   tictoc::toc()
#   lme
# }
#
# lme4 <- map_df %>% purrr::pmap(build_lme4_model)
#
#
# # Check assumptions
# p <- lme4 %>%
#   purrr::map2(name_df$name,
#               function(model, name){
#                 tictoc::tic(name)
#                 p_redres <- redres::plot_redres(model)
#                 p_resqq <- redres::plot_resqq(model)
#                 p_ranef <- redres::plot_ranef(model)
#                 plist <- list(p_redres, p_resqq, p_ranef)
#                 p <- gridExtra::arrangeGrob(plist[[1]], plist[[2]], plist[[3]],
#                                             nrow=2, ncol=2)
#                 ggplot2::ggsave(plot = p,
#                                 paste0("outputs/figures/Aim3/lme4_dxplots_", name, ".jpg"),
#                                 device = "jpeg",
#                                 width = 6,
#                                 height = 6,
#                                 units = "in")
#                 tictoc::toc()
#                 plist
#               })



# Table X Main ------------------------------------------------------------

outcome <- c("mathscalescore", "elascalescore")
exp <- c("ggs_ndvi_landsat_0250 + sgs_ndvi_landsat_0250",
         "ggs_ndvi_modis_0250 + sgs_ndvi_modis_0250",
         "ggs_ndvi_nlcd_0250 + sgs_ndvi_nlcd_0250")
# exp <- c("")
# covar <- c("r_SE_nat + ruca")
# covar <- c("ses_medianhhincome_log10 + testscore_ethnicity + testscore_gender + testscore_gifted + testscore_special_ed + testscore_totalunexcuseddays_cat + testscore_totaldaysmissed + school_pct_frl_avg")
covar <- c("")
map_df <- tidyr::crossing(outcome, exp, covar,  df = analysis_bygrade_jitter)
rm(outcome, exp, covar)

build_nlme_model <- function(outcome, exp, covar, df) {
  grade <- df$grade[1]
  tictoc::tic(paste0(grade, " ", outcome, " ~ ", exp, covar))
  # get model info
  exp_name <- exp %>%
    stringr::str_split("ggs_ndvi_", simplify = TRUE) %>% dplyr::nth(2) %>%
    stringr::str_split("_", simplify = TRUE) %>% dplyr::first()
  # Create vector of covar and exp
  covar_chr <- covar %>% stringr::str_split(pattern = "\\+") %>%
    dplyr::first() %>% stringr::str_trim(side = "both")
  exp_chr <- exp %>% stringr::str_split(pattern = "\\+") %>%
    dplyr::first() %>% stringr::str_trim(side = "both") %>%
    purrr::discard(~.x == "")
  # Remove NA observations
  df <- df %>%
    dplyr::select(c(tidyselect::all_of(c(outcome)),
                    tidyselect::any_of(covar_chr),
                    tidyselect::all_of(exp_chr),
                    "X", "Y", "cdenumber")) %>%
    tidyr::drop_na()
  # Create formula
  # formula <- paste0(outcome, " ~ ", exp, " + ", covar) %>% as.formula()
  if (covar == c("")) {formula <- paste0(outcome, " ~ ", exp) %>% as.formula()}
  # Run model
  lme_nlme <- df %>%
    nlme::lme(fixed = formula,
              random = ~1|cdenumber,
              data = .,
              method = "REML")
  lme_nlme_glance <- lme_nlme %>%  broom.mixed::glance()
  nschool <- summary(lme_nlme)[["dims"]][["ngrps"]][[1]]
  lme_nlme_tidy <- lme_nlme %>%
    broom.mixed::tidy() %>%
    dplyr::mutate(outcome = outcome,
                  grade = grade,
                  exp_name = exp_name,
                  nschool = nschool) %>%
    dplyr::bind_cols(lme_nlme_glance)
  tictoc::toc()
  lme_nlme_tidy
}



lme_nlme <- map_df %>% purrr::pmap(build_nlme_model)

table_main <- lme_nlme %>%
  dplyr::bind_rows() %>%
  dplyr::group_split(outcome, grade) %>%
  purrr::map(~.x %>%
               dplyr::filter(term %in% c("(Intercept)",
                                         "ggs_ndvi_landsat_0250", "sgs_ndvi_landsat_0250",
                                         "ggs_ndvi_modis_0250", "sgs_ndvi_modis_0250",
                                         "ggs_ndvi_nlcd_0250", "sgs_ndvi_nlcd_0250")) %>%
               dplyr::mutate(ci95 = paste0("(", sprintf("%.1f", estimate-1.96*std.error),
                                           ", ", sprintf("%.1f", estimate+1.96*std.error),
                                           ")"
               )) %>%
               dplyr::mutate(p = dplyr::case_when(p.value > 0.05 ~"",
                                                  p.value %>% dplyr::between(0.01, 0.05) ~ "*",
                                                  p.value %>% dplyr::between(0.001, 0.01) ~ "**",
                                                  p.value < 0.001 ~"***"),
                             beta = sprintf("%.1f", estimate) %>% paste0(p)) %>%
               dplyr::left_join(varname_df, by = "term") %>%
               dplyr::mutate(variable_label = ifelse(is.na(variable_label), term, variable_label)) %>%
               dplyr::mutate(model = paste0(outcome, "|", grade, "|", exp_name))
  )

table_main_wide <- c(list(table_main[1:3]), list(table_main[4:6])) %>%
  purrr::map(~purrr::map(.x, function(x) {
    schoollevel <- ifelse(x$grade[1] == "50", "Grade 5 Elementary",
                          ifelse(x$grade[1] == "80", "Grade 8 Middle", "Grade 9 High"))
    label <- paste0(schoollevel, " (n=", scales::label_comma(accuracy = 1)(x$nobs[1]), ")")
    beta <- x %>%
      dplyr::select(variable_label, beta, ci95, model) %>%
      dplyr::bind_rows(tibble::tibble(variable_label = c("Variable", NA),
                                      beta = c(label, "\U03B2"),
                                      ci95 = c(NA, "95% CI"),
                                      model = NA),.) %>%
      dplyr::rename_with(~paste0("x", x$grade[1], .))
      }
    ) %>%
      dplyr::bind_cols() %>%
      dplyr::select(-tidyselect::ends_with("model"),
                    -c("x80variable_label", "x90variable_label"))) %>%
  dplyr::bind_rows()

# table_main_long <- c(list(table_main[1:3]), list(table_main[4:6])) %>%
#   purrr::map(~purrr::map(.x, function(x) {
#     schoollevel <- ifelse(x$grade[1] == "50", "Grade 5 Elementary",
#                           ifelse(x$grade[1] == "80", "Grade 8 Middle", "Grade 9 High"))
#     label <- paste0(schoollevel, " (n=", scales::label_comma(accuracy = 1)(x$nobs[1]), ")")
#     beta <- x %>%
#       dplyr::mutate(beta_ci95 = paste0(beta, " ", ci95)) %>%
#       dplyr::select(variable_label, beta_ci95, model) %>%
#       dplyr::bind_rows(tibble::tibble(variable_label = c("Variable", NA),
#                                       beta_ci95 = c(label, paste0("\U03B2", " (95% CI)")),
#                                       model = NA),.) %>%
#       dplyr::rename_with(~paste0("x", x$grade[1], .))
#   }
#   ) %>%
#     dplyr::bind_cols() %>%
#     dplyr::select(-tidyselect::ends_with("model"),
#                   -c("x80variable_label", "x90variable_label"))) %>%
#   dplyr::bind_rows()




# Create xlsx
df <- table_main_wide
startrow <- df$x50variable_label %>% {which(. == "Variable")} %>% purrr::map_dbl(~.x + 1)
endrow <- df$x50variable_label %>% {which(. == "School NLCD % Tree")} %>% purrr::map_dbl(~.x + 1)
section_index <- df$x50variable_label %>% {which(. == "(Intercept)")} %>% purrr::map_dbl(~.x + 1)
# Open an xlsx workbook
wb <- openxlsx::buildWorkbook(df)
# Column widths
openxlsx::setColWidths(wb, "Sheet 1", cols = 1, widths = 22)
openxlsx::setColWidths(wb, "Sheet 1", cols = c(2,4,6), widths = 5)
openxlsx::setColWidths(wb, "Sheet 1", cols = c(3,5,7), widths = 16)
# Merge cells for Variable
for (i in startrow) {openxlsx::mergeCells(wb, sheet = "Sheet 1", cols = 1, rows = i:(i+1))}
# # Merge cells for grade labels
# for (i in startrow) {
#   if (length(df) > 4) {
#     openxlsx::mergeCells(wb, sheet = "Sheet 1", cols = 2:3, rows = i)
#     openxlsx::mergeCells(wb, sheet = "Sheet 1", cols = 4:5, rows = i)
#     openxlsx::mergeCells(wb, sheet = "Sheet 1", cols = 6:7, rows = i)}}
# Edit Style for starting row, column 1:4
openxlsx::addStyle(wb, sheet = "Sheet 1",
                   rows = startrow, cols = 1:7,
                   gridExpand = TRUE,
                   style = openxlsx::createStyle(
                     textDecoration = "bold",
                     halign = "center", valign = "center",
                     border = "top", borderStyle = "thick"))
# Edit Style for row 2, column 1:4
openxlsx::addStyle(wb, sheet = "Sheet 1",
                   rows = (startrow+1), cols = 1:7,
                   gridExpand = TRUE,
                   style = openxlsx::createStyle(
                     halign = "center", valign = "center",
                     border = c("top", "bottom"), borderStyle = "thin"))
# Add thin borders for section
for (i in section_index) {openxlsx::addStyle(wb, sheet = "Sheet 1",
                                             rows = i, cols = 1:7,
                                             gridExpand = TRUE,
                                             style = openxlsx::createStyle(
                                               border = "top", borderStyle = "thin"))}
# Edit Style for last row, column 1
openxlsx::addStyle(wb, sheet = "Sheet 1",
                   rows = endrow, cols = 1:7,
                   gridExpand = TRUE,
                   style = openxlsx::createStyle(border = "bottom", borderStyle = "thick"))
# save workbook
# openxlsx::saveWorkbook(wb, "outputs/tables/Aim3/table_main_datadriven_wide_formatted.xlsx", TRUE)
openxlsx::saveWorkbook(wb, "outputs/tables/Aim3/table_main_unadj_wide_formatted.xlsx", TRUE)



# * Semivariogram from nlme residuals -------------------------------------

# semivar <- purrr::map2(nlme, name_df$name, function(model, name) {
#   tictoc::tic(name)
#   variogram <- nlme::Variogram(
#     model,
#     correlation = nlme::corSpatial(form = ~X + Y, type ="spherical", nugget = F))
#   tictoc::toc()
#   variogram
# })
#
# semivar <- purrr::map2(nlme, name_df$name, function(model, name) {
#   tictoc::tic(name)
#   variogram <- nlme::Variogram(model)
#   tictoc::toc()
#   variogram
# })
#
# # export to a pdf
# pdf("outputs/figures/Aim3/semivariogram_nlme (no corspatial)_.pdf")
# pdf.options(width = 5, height = 3)
# for (i in 1:length(semivar)){
#   nlme:::plot.Variogram(semivar[[i]])
# }
# dev.off()
#
# # create list data the model was run on
# df_resid <- map_df %>%
#   purrr::pmap(function(outcome, exp, covar, df) {
#     # Create vector of covar and exp
#     covar_chr <- covar %>% stringr::str_split(pattern = "\\+") %>%
#       dplyr::first() %>% stringr::str_trim(side = "both")
#     exp_chr <- exp %>% stringr::str_split(pattern = "\\+") %>%
#       dplyr::first() %>% stringr::str_trim(side = "both") %>%
#       purrr::discard(~.x == "")
#     # Remove NA observations
#     df <- df %>%
#       dplyr::select(c(outcome,
#                       tidyselect::all_of(covar_chr),
#                       tidyselect::all_of(exp_chr),
#                       "X", "Y", "cdenumber")) %>%
#       tidyr::drop_na()
#     tictoc::toc()
#     df
#   })
#
# variogram_gstat <- purrr::map2(df_resid, nlme, function(x, y) {
#   df <- x %>%
#     dplyr::select(X, Y, geometry) %>%
#     dplyr::bind_cols(resid = y %>% nlme:::residuals.lme(type = "response")) %>%
#     sf::st_as_sf()
#   variogram <- gstat::variogram(resid ~ 1, data = df)
# })
# # export to a pdf
# pdf("outputs/figures/Aim3/semivariogram_gstat(resid).pdf")
# pdf.options(width = 5, height = 3)
# for (i in 1:length(variogram_gstat)){
#   print(base::plot(variogram_gstat[[i]],
#                    main = name_df$name[[i]]))
# }
# dev.off()
#
# variogram_gstat <- purrr::map2(df_resid, nlme, function(x, y) {
#   df <- x %>%
#     dplyr::select(X, Y, geometry) %>%
#     dplyr::bind_cols(resid = y %>% nlme:::residuals.lme(type = "normalized")) %>%
#     sf::st_as_sf()
#   variogram <- gstat::variogram(resid ~ 1, data = df)
# })
# # export to a pdf
# pdf("outputs/figures/Aim3/semivariogram_gstat(normalizedresid).pdf")
# pdf.options(width = 5, height = 3)
# for (i in 1:length(variogram_gstat)){
#   print(base::plot(variogram_gstat[[i]],
#                    main = name_df$name[[i]]))
# }
# dev.off()


# * GLS -------------------------------------------------------------------

# build_gls_model <- function(outcome, exp, covar, df) {
#   tictoc::tic(paste0(outcome, " ~ ", exp, covar))
#   # Create vector of covar and exp
#   covar_chr <- covar %>% stringr::str_split(pattern = "\\+") %>%
#     dplyr::first() %>% stringr::str_trim(side = "both")
#   exp_chr <- exp %>% stringr::str_split(pattern = "\\+") %>%
#     dplyr::first() %>% stringr::str_trim(side = "both") %>%
#     purrr::discard(~.x == "")
#   # Remove NA observations
#   df <- df %>%
#     dplyr::select(c(outcome,
#                     tidyselect::all_of(covar_chr),
#                     tidyselect::all_of(exp_chr),
#                     "X", "Y", "cdenumber")) %>%
#     tidyr::drop_na()
#   # Create formula
#   formula <- paste0(outcome, " ~ ", exp, covar) %>% as.formula()
#   # Run model
#   lme_nlme <- nlme::lme(fixed = formula,
#                         random = ~1|cdenumber,
#                         data = df,
#                         correlation = nlme::corSpatial(form = ~X + Y, type ="spherical", nugget = F),
#                         method = "REML")
#   tictoc::toc()
#   lme_nlme
# }
#
# gls <- map_df %>% purrr::pmap(build_gls_model)
# save_data(gls,
#           "DATA/Processed/Aim3/gls",
#           "DATA/Processed/Aim3/Archived/gls",
#           csv = FALSE)


# * Stratified models -----------------------------------------------------

# * * Continuous ----------------------------------------------------------

# outcome <- c("mathscalescore", "elascalescore")
# exp <- c("ggs_ndvi_landsat_0250 + sgs_ndvi_landsat_0250 + ",
#          "ggs_ndvi_modis_0250 + sgs_ndvi_modis_0250 + ",
#          "ggs_ndvi_nlcd_0250 + sgs_ndvi_nlcd_0250 + ")
# covar <- c("r_SE_nat + ruca")
# ef <- c("testscore_gender", "r_SE_nat", "ruca")
#
# map_df <- tidyr::crossing(outcome, exp, covar, ef, df = analysis_bygrade_jitter)
# rm(outcome, exp, covar, ef)
#
# build_str_nlme_model <- function(outcome, exp, covar, ef, df) {
#   tictoc::tic(paste0(outcome, " ~ ", exp, covar))
#   # get model info
#   grade <- df$grade[1]
#   exp_name <- exp %>%
#     stringr::str_split("ggs_ndvi_", simplify = TRUE) %>% dplyr::nth(2) %>%
#     stringr::str_split("_", simplify = TRUE) %>% dplyr::first()
#   # Create vector of covar and exp
#   covar_chr <- covar %>% stringr::str_split(pattern = "\\+") %>%
#     dplyr::first() %>% stringr::str_trim(side = "both")
#   exp_chr <- exp %>% stringr::str_split(pattern = "\\+") %>%
#     dplyr::first() %>% stringr::str_trim(side = "both") %>%
#     purrr::discard(~.x == "")
#   # Remove NA observations
#   df <- df %>%
#     dplyr::select(c(tidyselect::all_of(outcome),
#                     tidyselect::all_of(covar_chr),
#                     tidyselect::all_of(exp_chr),
#                     tidyselect::all_of(ef),
#                     "X", "Y", "cdenumber")) %>%
#     tidyr::drop_na()
#   interaction <- paste0(exp_chr, "*", ef) %>%
#     paste0(collapse = " + ") %>%
#     paste0(., " + ")
#
#   # Create formula
#   formula <- paste0(outcome, " ~ ", exp, interaction, covar) %>% as.formula()
#   # Run model
#   lme_nlme <- nlme::lme(fixed = formula,
#                         random = ~1|cdenumber,
#                         data = df,
#                         method = "REML")
#   tictoc::toc()
#   nobs <- broom.mixed::glance(lme_nlme) %$% nobs
#   lme_nlme_tidy <- broom.mixed::tidy(lme_nlme) %>%
#     dplyr::mutate(nobs = nobs,
#                   outcome = outcome,
#                   grade = grade,
#                   exp_name = exp_name,
#                   ef = ef)
# }
#
# lme4_str <- map_df %>% purrr::pmap(build_str_nlme_model)
# rm(map_df)


# * * Categorical ---------------------------------------------------------

outcome <- c("mathscalescore", "elascalescore")
exp <- c("ggs_ndvi_landsat_0250 + sgs_ndvi_landsat_0250",
         "ggs_ndvi_modis_0250 + sgs_ndvi_modis_0250",
         "ggs_ndvi_nlcd_0250 + sgs_ndvi_nlcd_0250")
covar <- c("r_SE_nat + ruca")
ef_str <- c("testscore_gender", "c5_SE_stt", "ruca_str")
map_df <- tidyr::crossing(outcome, exp, covar, ef_str, df = analysis_bygrade_jitter)
rm(outcome, exp, covar, ef_str)

# outcome <- "elascalescore"
# exp <- "ggs_ndvi_landsat_0250 + sgs_ndvi_landsat_0250"
# covar <- "r_SE_nat + ruca"
# ef_str <- "ruca_str"
# df <- analysis_bygrade_jitter[[1]]

build_strcat_nlme_model <- function(outcome, exp, covar, ef_str, df) {
  grade <- df$grade[1]
  tictoc::tic(paste0(grade, " ", ef_str, " ",
                     outcome, " ~ ", exp, covar))
  # get model info
  exp_name <- exp %>%
    stringr::str_split("ggs_ndvi_", simplify = TRUE) %>% dplyr::nth(2) %>%
    stringr::str_split("_", simplify = TRUE) %>% dplyr::first()
  # Update covar
  covar_chr <- covar %>% stringr::str_split(pattern = "\\+") %>%
    dplyr::first() %>% stringr::str_trim(side = "both")
  if (ef_str == "c5_SE_stt") {covar_chr <- covar_chr %>% purrr::discard(~.x %in% c("r_SE_nat", "ruca"))}
  if (ef_str == "ruca_str") {covar_chr <- covar_chr %>% purrr::discard(~.x == "ruca")}
  covar <- paste0(covar_chr, collapse = " + ")
  if (covar != "") {covar <- paste0(" + ", covar)}
  # Create vector of covar and exp
  covar_chr <- covar %>% stringr::str_split(pattern = "\\+") %>%
    dplyr::first() %>% stringr::str_trim(side = "both")
  exp_chr <- exp %>% stringr::str_split(pattern = "\\+") %>%
    dplyr::first() %>% stringr::str_trim(side = "both") %>%
    purrr::discard(~.x == "")
  # Remove NA observations
  df <- df %>%
    dplyr::select(c(tidyselect::all_of(c(outcome)),
                    tidyselect::any_of(covar_chr),
                    tidyselect::all_of(exp_chr),
                    tidyselect::all_of(ef_str),
                    "X", "Y", "cdenumber")) %>%
    tidyr::drop_na()
  # Create group of df by ef_str
  df_list <- df %>%
    dplyr::group_split(dplyr::across(tidyselect::all_of(ef_str)))
  # Create formula
  formula <- paste0(outcome, " ~ ", exp, covar) %>% as.formula()
  # Run model
  lme_nlme <- df_list %>%
    purrr::map(~nlme::lme(fixed = formula,
                          random = ~1|cdenumber,
                          data = .x,
                          method = "REML"))
  lme_nlme_glance <- lme_nlme %>% purrr::map(broom.mixed::glance)
  nschool <- lme_nlme %>% purrr::map(~summary(.x)[["dims"]][["ngrps"]][[1]] %>%
                                       tibble::as_tibble() %>%
                                       dplyr::rename(nschool = value))
  ef_str_value <- df_list %>%
    purrr::map(~.x %>%
                 dplyr::select(tidyselect::all_of(ef_str)) %>%
                 dplyr::first() %>%
                 dplyr::first()
    )
  lme_nlme_tidy <- lme_nlme %>%
    purrr::map(~.x %>%
                 broom.mixed::tidy() %>%
                 dplyr::mutate(outcome = outcome,
                               grade = grade,
                               exp_name = exp_name,
                               ef_str = ef_str)) %>%
    purrr::map2(ef_str_value, ~.x %>% dplyr::mutate(ef_str_value = .y)) %>%
    purrr::map2(nschool, ~.x %>% dplyr::bind_cols(.y)) %>%
    purrr::map2(lme_nlme_glance, ~.x %>% dplyr::bind_cols(.y)) %>%
    dplyr::bind_rows()
  tictoc::toc()
  lme_nlme_tidy
}


lme_str_c5_SE_stt <- map_df %>%
  dplyr::filter(ef_str == "c5_SE_stt") %>%
  purrr::pmap(build_strcat_nlme_model) %>%
  dplyr::bind_rows()

lme_str_gender <- map_df %>%
  dplyr::filter(ef_str == "testscore_gender") %>%
  purrr::pmap(build_strcat_nlme_model) %>%
  dplyr::bind_rows()

lme_str_ruca_landsatmodis <- map_df %>%
  dplyr::filter(ef_str == "ruca_str") %>%
  dplyr::filter(exp != "ggs_ndvi_nlcd_0250 + sgs_ndvi_nlcd_0250") %>%
  purrr::pmap(build_strcat_nlme_model) %>%
  dplyr::bind_rows()

lme_str <- list(lme_str_c5_SE_stt, lme_str_gender, lme_str_ruca_landsatmodis)





# * Figure X --------------------------------------------------------------
abbrs <- tibble::tibble(abv = c("F", "M", "landsat", "modis", "nlcd",
                                "Very Low", "Low", "Moderate", "High", "Very High",
                                "elascalescore", "mathscalescore",
                                "1", "2+"),
                        long = c("Female", "Male", "Landsat", "MODIS", "NLCD",
                                 "1: Very Low", "2: Low", "3: Moderate", "4: High", "5: Very High",
                                 "ELA", "Math",
                                 "1", "2+"))

df <- lme_str_c5_SE_stt %>%
# df <- lme_str_gender %>%
# df <- lme_str_ruca_landsatmodis %>%
  dplyr::filter(effect == "fixed" & term!= "(Intercept)") %>%
  dplyr::mutate(exparea = ifelse(stringr::str_sub(term, 1, 3) == "ggs", "residential", "school")) %>%
  dplyr::mutate(lb = estimate-1.96*std.error,
                ub = estimate+1.96*std.error) %>%
  dplyr::mutate(Grade = (as.numeric(grade)/10) %>% as.character()) %>%
  dplyr::left_join(abbrs %>% dplyr::rename(exp_name = abv, Greenness = long)) %>%
  dplyr::left_join(abbrs %>% dplyr::rename(ef_str_value = abv, EF = long)) %>%
  dplyr::left_join(abbrs %>% dplyr::rename(outcome = abv, Outcome = long)) %>%
  dplyr::group_by(exp_name, grade, outcome) %>%
  dplyr::mutate(modelid = dplyr::cur_group_id()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(term %in% c("(Intercept)",
                            "ggs_ndvi_landsat_0250",
                            "sgs_ndvi_landsat_0250",
                            "ggs_ndvi_modis_0250",
                            "sgs_ndvi_modis_0250",
                            "ggs_ndvi_nlcd_0250",
                            "sgs_ndvi_nlcd_0250"
                            ))

# https://konstantinkashin.com/2013/using-ggplot2-to-plot-regression-coefficients-with-confidence-intervals/
pd <- ggplot2::position_dodge(width=0.2)
ggplot2::ggplot(data = df %>%
                  # dplyr::filter(ef_str_value == "F") %>%
                  dplyr::filter(exparea == "residential"),
                ggplot2::aes(x = Grade, y = estimate, color = Outcome)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept=0), linetype = "dashed", color = "red") +
  ggplot2::geom_point(ggplot2::aes(shape=Outcome),size=2, position=pd) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=lb,ymax=ub),width=0.1,position=pd) +
  ggplot2::scale_color_brewer(palette = "Dark2") +
  ggplot2::scale_y_continuous(paste0("\U03B2", " coefficient")) +
  ggplot2::theme_() +
  ggplot2::theme(legend.position="bottom", legend.direction = "horizontal") +
  ggplot2::facet_grid(cols = dplyr::vars(EF), rows = dplyr::vars(Greenness))

ggplot2::ggsave(
  "outputs/figures/Aim3/str_ses.jpg",
  # "outputs/figures/Aim3/str_gender.jpg",
  # "outputs/figures/Aim3/str_ruca.jpg",
  width = 8, height = 5, units = "in")


# * TableX ----------------------------------------------------------------

table_str <- lme_str %>%
  purrr::map(function(x) {
    beta <- x %>%
      dplyr::filter(effect == "fixed") %>%
      dplyr::select(term, estimate, std.error, p.value, ef_str, ef_str_value, outcome, grade, exp_name) %>%
      dplyr::mutate(ci95 = paste0("(", sprintf("%.1f", estimate-1.96*std.error),
                                      ", ", sprintf("%.1f", estimate+1.96*std.error),
                                      ")"
                                      )) %>%
      dplyr::mutate(p = dplyr::case_when(p.value > 0.05 ~"",
                                         p.value %>% dplyr::between(0.01, 0.05) ~ "*",
                                         p.value %>% dplyr::between(0.001, 0.01) ~ "**",
                                         p.value < 0.001 ~"***"),
                    beta = sprintf("%.1f", estimate) %>% paste0(p)) %>%
      dplyr::select(-c("std.error", "estimate", "p", "p.value")) %>%
      tidyr::pivot_wider(names_from = ef_str_value, values_from = c(beta, ci95)) %>%
      dplyr::left_join(varname_df, by = "term") %>%
      dplyr::mutate(variable_label = ifelse(is.na(variable_label), term, variable_label)) %>%
      dplyr::mutate(outcome = dplyr::recode(outcome, "elascalescore" = "ELA", "mathscalescore" = "Math")) %>%
      janitor::clean_names()
    beta <- beta %>%
      dplyr::select(variable_label, grade,
                    tidyselect::starts_with("beta_"),
                    tidyselect::starts_with("ci95_"),
                    outcome)
  }
               )

table_str_ses <- table_str[[1]] %>%
  dplyr::select(variable_label, grade,
                beta_very_low, ci95_very_low,
                beta_low, ci95_low,
                beta_moderate, ci95_moderate,
                beta_high, ci95_high,
                beta_very_high, ci95_very_high,
                outcome)
table_str_ses <-
  dplyr::bind_rows(tibble::tibble(variable_label= c("Variable", NA, NA), grade=NA,
                                  beta_very_low = c("ELA","Very Low", "\U03B2"),
                                  ci95_very_low= c(NA, NA, "95% CI"),
                                  beta_low= c(NA,"Low","\U03B2"),
                                  ci95_low= c(NA, NA, "95% CI"),
                                  beta_moderate= c(NA,"Moderate","\U03B2"),
                                  ci95_moderate= c(NA, NA, "95% CI"),
                                  beta_high= c(NA,"High","\U03B2"),
                                  ci95_high= c(NA, NA, "95% CI"),
                                  beta_very_high=c(NA,"Very High","\U03B2"),
                                  ci95_very_high= c(NA, NA, "95% CI"),
                                  outcome=NA),
                   table_str_ses %>% dplyr::filter(outcome == "ELA"),
                   tibble::tibble(variable_label= c("Variable", NA, NA), grade=NA,
                                  beta_very_low = c("Math","Very Low", "\U03B2"),
                                  ci95_very_low= c(NA, NA, "95% CI"),
                                  beta_low= c(NA,"Low","\U03B2"),
                                  ci95_low= c(NA, NA, "95% CI"),
                                  beta_moderate= c(NA,"Moderate","\U03B2"),
                                  ci95_moderate= c(NA, NA, "95% CI"),
                                  beta_high= c(NA,"High","\U03B2"),
                                  ci95_high= c(NA, NA, "95% CI"),
                                  beta_very_high=c(NA,"Very High","\U03B2"),
                                  ci95_very_high= c(NA, NA, "95% CI"),
                                  outcome=NA),
                   table_str_ses %>% dplyr::filter(outcome == "Math"))

# Data-driven approach ----------------------------------------------------





