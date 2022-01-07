# ---------------------------------------------------------------------------- #
# Title: Script for modelling school-level data
# Author: Hanh Dung Dao
# Purpose: To model outcome (test scores) with SES variables at the school level
# ---------------------------------------------------------------------------- #


# Preparation -------------------------------------------------------------
rm(list = ls())


# * Load sources ----------------------------------------------------------
# A `source()` file is run to execute its code.
# source()

# * Load packages ---------------------------------------------------------
# The function `package.check` will check if each package is on the local machine.
# If a package is installed, it will be loaded. If any are not, they will be installed and loaded.
# r load_packages
packages <- c("tidyverse", "magrittr")

package_check <- lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x, dependencies = TRUE)
  library(x, character.only = TRUE)
})
# * Declare globals -------------------------------------------------------

# Load data ---------------------------------------------------------------
load_analysis_school <- readr::read_rds("DATA/Processed/Aim1/aim1_analysis_school.rds") %>%
  dplyr::ungroup()


# Clean data --------------------------------------------------------------
analysis <- load_analysis_school %>%
  # Limit to elementary schools
  dplyr::filter(grade %in% (c("30", "40", "50")))

# Descriptive table
table1_student <- skimr::skim(analysis %>% dplyr::select(tidyselect:::where(is.numeric))) %>%
  # dplyr::mutate_if(is.numeric, round, digits = 2) %>%
  dplyr::rename(covar = skim_variable) %>%
  dplyr::mutate(iqr = numeric.p75 - numeric.p25,
                range = numeric.p100 - numeric.p0)

# Outcome SD as threshold dataset
outcome_sd <- analysis %>%
  dplyr::group_by(grade) %>%
  dplyr::summarise_at(c("math_mean", "ela_mean"), sd, na.rm = TRUE) %>%
  tidyr::pivot_longer(!grade, names_to = "o", values_to = "outcome_sd") %>%
  dplyr::mutate(g = as.integer(as.numeric(grade)+2))



# Correlation -------------------------------------------------------------
dat.corr <- analysis %>%
  dplyr::select(tidyselect::starts_with(c("school_", "ieq_", "school_",
                                          "gender_", "ethnicity_", "gifted_", "specialed_",
                                          "ses_"))) %>%
  dplyr::select(where(is.numeric))


# CHECK THE r VALUES
corr.matrix <-  dat.corr %>% cor(use = "pairwise.complete.obs", method = "pearson")
corr.matrix.melt <- arrange(reshape2::melt(as.matrix(corr.matrix)), -abs(value)) %>%
  dplyr::distinct() %>%
  dplyr::filter(Var1 != Var2)

# Remove by correlation
# specialed_no_iep: r=1.00 with specialed_iep
# ses_medianfamincome: r=0.9996657 with ses_medianhhincome
# ses_medianfamincome_withkid: r=0.9990126 with ses_medianhhincome
# ses_married_less18: r=0.9638401 with ses_married_6to17

# Remove as reference in categorical variables
# gender_m: r=1.00 with gender_f
# ethnicity_white
# gifted_not_identified_as_gifted_talented

analysis <- analysis %>%
  # dplyr::select(-c(gender_m, specialed_no_iep, ethnicity_white, gifted_not_identified_as_gifted_talented,
  #                  ses_medianfamincome, ses_medianfamincome_withkid, ses_married_less18))
  dplyr::select(-c(gender_m, specialed_no_iep,
                   ses_medianfamincome, ses_medianfamincome_withkid, ses_married_less18))



# Create data by grade ----------------------------------------------------

# Create a list object with analysis data by grade
analysis_grade <- analysis %>%
  dplyr::group_by(grade) %>%
  dplyr::group_split()

# analysis_grade_nested <- analysis %>% dplyr::group_by(grade) %>% tidyr::nest()

# Create a data frame for variable names and types
vars_list <- purrr::map_df(analysis, class) %>% t %>% tibble::as_tibble(rownames = NA) %>% tibble::rownames_to_column() %>%
  dplyr::rename(varname = rowname, varclass = V1) %>%
  dplyr::mutate(vartype = dplyr::case_when(varname %in% c("id_dao", "cdenumber", "studentkey", "GEOID") ~ "id",
                                           endsWith(varname, suffix = "_mean") ~ "score",
                                           startsWith(varname, prefix = "school_") ~ "school",
                                           startsWith(varname, prefix = "gender_") ~ "school",
                                           startsWith(varname, prefix = "ethnicity_") ~ "school",
                                           startsWith(varname, prefix = "gifted_") ~ "school",
                                           startsWith(varname, prefix = "specialed_") ~ "school",
                                           startsWith(varname, prefix = "ieq_") ~ "ieq",
                                           startsWith(varname, prefix = "ses_") ~ "ses",
                                           TRUE ~ ""),
                varcatnum = dplyr::case_when(startsWith(varname, prefix = "school_") ~ "numeric",
                                             startsWith(varname, prefix = "ieq") ~ "numeric",
                                             startsWith(varname, prefix = "gender_") ~ "numeric",
                                             startsWith(varname, prefix = "specialed_") ~ "numeric",
                                             startsWith(varname, prefix = "ses") ~ "numeric",
                                             startsWith(varname, prefix = "ethnicity_") ~ "categorical",
                                             startsWith(varname, prefix = "gifted") ~ "categorical",
                                             TRUE ~ "")
                )

# Create a vector with covariate names
covar <- vars_list %>%
  dplyr::filter(vartype %in% c("ses", "school", "ieq")) %>%
  dplyr::arrange(varname) %$% as.vector(varname)


covar_num <- vars_list %>%
  dplyr::filter(varname %in% covar & varclass == "numeric") %>%
  dplyr::arrange(varname) %$% as.vector(varname)


# Resources ---------------------------------------------------------------
# Model Dx
# https://jhudatascience.org/tidyversecourse/model.html#model-diagnostics
# http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/
# https://rpubs.com/aryn999/LinearRegressionAssumptionsAndDiagnosticsInR

# Model assumptions -------------------------------------------------------

build_simple_linear_model <- function(varname, outcome, grade, ...){
    paste0(outcome, " ~ ", varname) %>%
    as.formula %>%
    stats::lm(data = analysis_grade[[grade-2]])
}


# Prepare COVAR_SIMPLE to run simple linear model
covar_ethnicity <- purrr::keep(covar, function(x) base::startsWith(x, prefix = "ethnicity")) %>%
  purrr::discard(~ .x == "ethnicity_white") %>%
  base::paste0(collapse = " + ")
covar_gifted <- purrr::keep(covar, function(x) base::startsWith(x, prefix = "gifted")) %>%
  purrr::discard(~ .x == "gifted_not_identified_as_gifted_talented") %>%
  base::paste0(collapse = " + ")

covar_simple <- c(covar, covar_ethnicity, covar_gifted)


# Create all combinations for explanatory variables + outcome + grade
combos <- tidyr::crossing(v = covar_simple,
                          o = c("math_mean", "ela_mean"),
                          g = 3:5) %>%
  tibble::rownames_to_column() %>%
  dplyr::mutate(multiple = stringr::str_detect(v, stringr::fixed("+")),
                group = stringr::str_split(v, "_", 2) %>% map(extract2, 1) %>% as.character) %>%
  dplyr::mutate(name = dplyr::case_when(multiple == FALSE ~ v,
                                        multiple == TRUE ~ group))

test <- stringr::str_split(combos$v, "_", 2) %>% map(extract2, 1) %>% as.character



# Get multivariable model results
simple_linear_model <- purrr::pmap(combos, build_simple_linear_model)


# Get model augment values
simple_linear_augment <- simple_linear_model %>%
  purrr::map(broomExtra::augment)


# Model assumptions & dx

create_dx_plots <- function(model_num){
  model <- combos[combos$rowname == model_num, ]
  outcome <- model %$% as.character(o)
  grade <- model %$% as.numeric(g)
  name <- model %$% as.character(name)
  grDevices::jpeg(filename = paste("outputs/figures/school_simple_dx",
                                    name, outcome, grade, ".jpg",
                                    sep = "_"),
                  width = 1500,
                  height = 1000,
                  unit = "px",
                  pointsize = 18)
  graphics::par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))
  graphics::plot(simple_linear_model[[model_num]], sub = "")
  graphics::hist(simple_linear_augment[[model_num]] %$% as.vector(.resid), main = "", xlab = ".resid")
  graphics::plot(simple_linear_augment[[model_num]][ ,1:2])
  title(paste0("Grade ", grade, " - ", outcome, " - ", name),
        outer = TRUE)

  grDevices::dev.off()

}

# Run loop to save the dx plots
for(i in 1:nrow(combos)) {create_dx_plots(i)}



# Get model performance & coefficients
simple_linear_perf <- simple_linear_model %>%
  purrr::map(broomExtra::glance_performance) %>%
  purrr::map_df(dplyr::bind_rows) %>%
  dplyr::bind_cols(combos)


simple_linear_coeff <- simple_linear_model %>%
  purrr::map(broomExtra::tidy_parameters) %>%
  purrr::map_df(dplyr::bind_rows, .id = "rowname") %>%
  dplyr::left_join(combos ,
                   by = "rowname") %>%
  dplyr::left_join(table1_student %>% dplyr::select(covar, iqr, range) %>%
                     dplyr::rename(term = covar),
                   by = "term") %>%
  dplyr::left_join(outcome_sd %>% dplyr::select(o, outcome_sd, g),
                   by = c("o", "g")) %>%
  dplyr::mutate(effect_iqr = estimate * iqr,
                effect_range = estimate * range) %>%
  dplyr::mutate(effect_iqr_sd = (effect_iqr >= outcome_sd),
                effect_range_sd = (effect_range >= outcome_sd))

simple_linear_list <- list(simple_linear_coeff, simple_linear_perf)

openxlsx::write.xlsx(simple_linear_list, "outputs/simple_linear.xlsx",
                     firstRow = TRUE,
                     overwrite = TRUE)


# Resources ---------------------------------------------------------------
# Nonparametric
# https://rcompanion.org/handbook/F_12.html

# Quantile regression
# https://fanwangecon.github.io/R4Econ/regnonlin/quantreg/htmlpdfr/fs_quantreg_intro.html
# https://www.r-bloggers.com/2019/01/quantile-regression-in-r-2/
# https://rpubs.com/ibn_abdullah/rquantile
# https://data.library.virginia.edu/getting-started-with-quantile-regression/


# Quantile regression
quantile_model <- quantreg::rq(ela_mean ~ ieq_visual,
             data = analysis_grade[[5-2]],
             tau = 0.5)
summary(quantile_model)

quantile_model_null <- quantreg::rq(ela_mean ~ 1,
                data = analysis_grade[[5-2]],
                tau = 0.5)
anova(quantile_model, quantile_model_null)

rcompanion::nagelkerke(quantile_model)
rcompanion::accuracy(list(quantile_model))





# multiple var model





