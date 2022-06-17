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
  dplyr::filter(grade %in% c("50", "80", "90"))

analysis_bygrade <- analysis %>%
  dplyr::bind_cols(data.frame(sf::st_coordinates(analysis))) %>%
  dplyr::group_split(grade) %>%
  purrr::map(~.x %>% dplyr::distinct(studentkey, .keep_all = TRUE)) %>%
  `names<-`(c("50", "80", '90'))



# Correlation Matrix ------------------------------------------------------

# select variable for correlation matrix
analysis_cor <- analysis %>%
  dplyr::select(where(is.numeric),
                -c("id_dao", "endyear"),
                -tidyselect::starts_with("per_"),
                -tidyselect::starts_with("pct_"),
                -z_SE_nat
                ) %>%
  sf::st_drop_geometry()

# Create df with all r values
corr_matrix <-  analysis_cor %>% cor(use = "pairwise.complete.obs", method = "pearson")
corr_matrix_melt <- corr_matrix %>%
  as.matrix() %>%
  reshape2::melt() %>%
  dplyr::filter(Var1 != Var2) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(sort = paste0(sort(c(Var1, Var2)), collapse = "")) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(value_abs = abs(value)) %>%
  dplyr::arrange(-abs(value)) %>%
  dplyr::distinct(sort, .keep_all = TRUE) %>%
  dplyr::select(-sort)


# Create correlation matrix using the package `corrr`
# https://corrr.tidymodels.org/index.html
analysis_cor %>%
  corrr::correlate() %>%
  corrr::network_plot(min_cor = .7)

# SES variables
analysis_cor %>%
  dplyr::select(-tidyselect::starts_with("sgs_"),
                -tidyselect::starts_with("ggs_"),
                -tidyselect::ends_with("scalescore"),
                -testscore_instructionday,
                -tidyselect::starts_with("testscore_total"),
                -ruca,
                -r_SE_nat,
                -school_student_enrollment_avg,
                -ses_unemployed,
                -ses_uninsured_6to18) %>%
  corrr::correlate() %>%
  corrr::network_plot(min_cor = .7)

# Greenness variables
analysis_cor %>%
  dplyr::select(tidyselect::starts_with("sgs_"),
                tidyselect::starts_with("ggs_")) %>%
  corrr::correlate() %>%
  corrr::network_plot(min_cor = .7)



# SES
# ses_uninsured_all
# ses_renter_withkid_6to17

# Only include
# ses_edu_highschoolmore, ses_married_6to17, ses_medianhhincome_log10, ses_uninsured_6to18, testscore_ethnicity, testscore_gender, testscore_gifted, testscore_special_ed, testscore_totalunexcuseddays, testscore_totaldaysmissed
# ses_crowding_cat, ses_poverty_all_cat, ses_renter_all_cat, ses_unemployed_cat




# Check mixed effect model assumptions ------------------------------------

outcome <- c("mathscalescore", "elascalescore")
var <- c("ggs_ndvi_landsat_0250", "sgs_ndvi_landsat_0250",
         "ggs_ndvi_modis_0250", "sgs_ndvi_modis_0250",
         "ggs_ndvi_nlcd_0250", "sgs_ndvi_nlcd_0250",
         "r_SE_nat", "ruca",
         "school_pct_frl_avg", "school_student_enrollment_avg",
         "ses_married_6to17", "ses_married_less18",
         "ses_edu_highschoolmore",
         "ses_poverty_all", "ses_poverty_6to17",
         "ses_medianhhincome", "ses_medianhhincome_log10",
         "ses_medianfamincome", "ses_medianfamincome_withkid",
         "ses_unemployed",
         "ses_renter_all", "ses_renter_withkid_6to17",
         "ses_crowding",
         "ses_uninsured_6to18", "ses_uninsured_all")
df <- analysis_bygrade
name_df <- tidyr::crossing(outcome, var, grade = c("50", "80", "90"))
map_df <- tidyr::crossing(outcome, var, df = df)
rm(outcome, var, df)

create_dxplots <- function(outcome, var, df) {
  name <- paste0(outcome, " - ", var, " - ", df[1,]$grade)
  tictoc::tic(name)
  # run bivar lmer
  df <- df %>%
    dplyr::filter(!is.na({{outcome}}) & !is.na({{var}}))
  formula <- paste0(outcome, " ~ ", var, " + (1|cdenumber)") %>% as.formula()
  lme <- lme4::lmer(formula, data = df, REML = TRUE)
  # creat dx plots
  p_redres <- redres::plot_redres(lme)
  p_resqq <- redres::plot_resqq(lme)
  p_ranef <- redres::plot_ranef(lme)
  plist <- list(p_redres, p_resqq, p_ranef)
  p <- gridExtra::arrangeGrob(plist[[1]], plist[[2]], plist[[3]],
                              nrow=2, ncol=2)
  ggplot2::ggsave(plot = p,
                  paste0("outputs/figures/Aim3/lme4_dxplots_bivar_", name, ".jpg"),
                  device = "jpeg",
                  width = 6,
                  height = 6,
                  units = "in")
  tictoc::toc()
  plist
}


dxplots <- map_df %>% purrr::pmap(create_dxplots)



# Bivariate ---------------------------------------------------------------

run_bivar_lmer <- function(outcome, var, df) {
  name <- paste0(outcome, " - ", var, " - ", df[1,]$grade)
  grade <- df[1,]$grade
  tictoc::tic(name)
  # run bivar lmer
  df <- df %>%
    dplyr::filter(!is.na({{outcome}}) & !is.na({{var}}))
  formula <- paste0(outcome, " ~ ", var, " + (1|cdenumber)") %>% as.formula()
  lme <- lme4::lmer(formula, data = df, REML = TRUE)
  tictoc::toc()
  lme
}

lme <- map_df %>%
  dplyr::filter(var == "ruca") %>%
  purrr::pmap(run_bivar_lmer)

get_lmer_coef <- function(model, grade){
  # get coeff
  formula_cat <- model@call %>% deparse %>% paste0(collapse = "")
  outcome <- formula_cat %>%
    stringr::str_split(patter = "=") %>% purrr::map(dplyr::nth, 2) %>%
    stringr::str_split(patter = "~") %>% purrr::map(dplyr::first) %>%
    as.character() %>% stringr::str_trim("both")
  coeff <- model %>%
    broomExtra::tidy_parameters() %>%
    dplyr::mutate(n = nrow(model@frame),
                  outcome = outcome,
                  grade = grade) %>%
    dplyr::mutate(formula = formula_cat)
}

coeff <- purrr::map2(lme,
                     name_df %>% dplyr::filter(var == "ruca") %$% grade,
                     get_lmer_coef) %>%
  dplyr::bind_rows()


# Remove ruca
# Arrive at a similar list of variables for first stage analysis as AIM 1










