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
packages <- c("magrittr", "quantreg")

lapply(packages, function(x) {
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
table1 <- skimr::skim(analysis %>% dplyr::select(tidyselect:::where(is.numeric))) %>%
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
corr.matrix.melt <- dplyr::arrange(reshape2::melt(as.matrix(corr.matrix)), -abs(value)) %>%
  dplyr::distinct() %>%
  dplyr::filter(Var1 != Var2) %>%
  dplyr::mutate(abs_value = abs(value))

# openxlsx::write.xlsx(corr.matrix.melt, "outputs/aim1_schoolmodel_corrmatrixmelt.xlsx",
                     # firstRow = TRUE)


df <- corr.matrix.melt
wb <- openxlsx::buildWorkbook(df)
# conditional format p-value column
openxlsx::conditionalFormatting(wb, "Sheet 1", cols = 4, rows = 1:nrow(df),
                                rule = ">=0.80",
                                style = openxlsx::createStyle(fontColour = "#006100", bgFill = "#C6EFCE"))
# Autofit column width
openxlsx::setColWidths(wb, "Sheet 1", cols = 1:ncol(df), widths = rep("auto", ncol(df)))
# Add a data filter
openxlsx::addFilter(wb, "Sheet 1", rows = 1, cols = 1:ncol(df))

# save workbook
openxlsx::saveWorkbook(wb, "outputs/aim1_schoolmodel_corrmatrixmelt.xlsx", TRUE)
rm(df)






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
  #                  ses_medianfamincome, ses_medianfamincome_withkid))
  dplyr::select(-c(gender_m, specialed_no_iep,
                   ses_medianfamincome, ses_medianfamincome_withkid))





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


# Resources - Model Dx ----------------------------------------------------
# Model Dx
# https://jhudatascience.org/tidyversecourse/model.html#model-diagnostics
# http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/
# https://rpubs.com/aryn999/LinearRegressionAssumptionsAndDiagnosticsInR

# # Model assumptions -------------------------------------------------------
#
# build_simple_linear_model <- function(varname, outcome, grade, ...){
#     paste0(outcome, " ~ ", varname) %>%
#     as.formula %>%
#     stats::lm(data = analysis_grade[[grade-2]])
# }
#
#
# # Prepare COVAR_SIMPLE to run simple linear model
# covar_ethnicity <- purrr::keep(covar, function(x) base::startsWith(x, prefix = "ethnicity")) %>%
#   purrr::discard(~ .x == "ethnicity_white") %>%
#   base::paste0(collapse = " + ")
# covar_gifted <- purrr::keep(covar, function(x) base::startsWith(x, prefix = "gifted")) %>%
#   purrr::discard(~ .x == "gifted_not_identified_as_gifted_talented") %>%
#   base::paste0(collapse = " + ")
#
# covar_simple <- c(covar, covar_ethnicity, covar_gifted)
#
#
# # Create all combinations for explanatory variables + outcome + grade
# combos <- tidyr::crossing(v = covar_simple,
#                           o = c("math_mean", "ela_mean"),
#                           g = 3:5) %>%
#   tibble::rownames_to_column() %>%
#   dplyr::mutate(multiple = stringr::str_detect(v, stringr::fixed("+")),
#                 group = stringr::str_split(v, "_", 2) %>% map(magrittr::extract2, 1) %>% as.character) %>%
#   dplyr::mutate(name = dplyr::case_when(multiple == FALSE ~ v,
#                                         multiple == TRUE ~ group))
#
# test <- stringr::str_split(combos$v, "_", 2) %>% map(extract2, 1) %>% as.character
#
#
# # Get multivariable model results
# simple_linear_model <- purrr::pmap(combos, build_simple_linear_model)
#
#
# # Get model augment values
# simple_linear_augment <- simple_linear_model %>%
#   purrr::map(broomExtra::augment)
#
#
# # Model assumptions & dx
#
# create_dx_plots <- function(model_num){
#   model <- combos[combos$rowname == model_num, ]
#   outcome <- model %$% as.character(o)
#   grade <- model %$% as.numeric(g)
#   name <- model %$% as.character(name)
#   grDevices::jpeg(filename = paste("outputs/figures/school_simple_dx",
#                                     name, outcome, grade, ".jpg",
#                                     sep = "_"),
#                   width = 1500,
#                   height = 1000,
#                   unit = "px",
#                   pointsize = 18)
#   graphics::par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))
#   graphics::plot(simple_linear_model[[model_num]], sub = "")
#   graphics::hist(simple_linear_augment[[model_num]] %$% as.vector(.resid), main = "", xlab = ".resid")
#   graphics::plot(simple_linear_augment[[model_num]][ ,1:2])
#   title(paste0("Grade ", grade, " - ", outcome, " - ", name),
#         outer = TRUE)
#
#   grDevices::dev.off()
#
# }
#
# # Run loop to save the dx plots
# for(i in 1:nrow(combos)) {create_dx_plots(i)}
#
#
#
# # Get model performance & coefficients
# simple_linear_perf <- simple_linear_model %>%
#   purrr::map(broomExtra::glance_performance) %>%
#   purrr::map_df(dplyr::bind_rows) %>%
#   dplyr::bind_cols(combos)
#
#
# simple_linear_coeff <- simple_linear_model %>%
#   purrr::map(broomExtra::tidy_parameters) %>%
#   purrr::map_df(dplyr::bind_rows, .id = "rowname") %>%
#   dplyr::left_join(combos ,
#                    by = "rowname") %>%
#   dplyr::left_join(table1_student %>% dplyr::select(covar, iqr, range) %>%
#                      dplyr::rename(term = covar),
#                    by = "term") %>%
#   dplyr::left_join(outcome_sd %>% dplyr::select(o, outcome_sd, g),
#                    by = c("o", "g")) %>%
#   dplyr::mutate(effect_iqr = estimate * iqr,
#                 effect_range = estimate * range) %>%
#   dplyr::mutate(effect_iqr_sd = (effect_iqr >= outcome_sd),
#                 effect_range_sd = (effect_range >= outcome_sd))
#
# simple_linear_list <- list(simple_linear_coeff, simple_linear_perf)
#
# openxlsx::write.xlsx(simple_linear_list, "outputs/simple_linear.xlsx",
#                      firstRow = TRUE,
#                      overwrite = TRUE)


# Resources - Nonparametric & quantile regression ------------------------
# Nonparametric
# https://rcompanion.org/handbook/F_12.html

# Quantile regression
# https://fanwangecon.github.io/R4Econ/regnonlin/quantreg/htmlpdfr/fs_quantreg_intro.html
# https://www.r-bloggers.com/2019/01/quantile-regression-in-r-2/
# https://rpubs.com/ibn_abdullah/rquantile
# https://data.library.virginia.edu/getting-started-with-quantile-regression/



# Quantile regression -----------------------------------------------------

# https://rcompanion.org/handbook/F_12.html

# While traditional linear regression models the conditional mean of the dependent variable, quantile regression models the conditional median or other quantile. Medians are most common, but for example, if the factors predicting the highest values of the dependent variable are to be investigated, a 95th percentile could be used.  Likewise, models for several quantiles, e.g. 25th , 50th, 75th percentiles, could be investigated simultaneously.
# Quantile regression makes no assumptions about the distribution of the underlying data, and is robust to outliers in the dependent variable.  It does assume the dependent variable is continuous.  However, there are functions for other types of dependent variables in the qtools package.  The model assumes that the terms are linearly related. Quantile regression is sometimes considered “semiparametric”.
# Quantile regression is very flexible in the number and types of independent variables that can be added to the model.  The example, here, however, confines itself to a simple case with one independent variable and one dependent variable.

# Create all combinations for explanatory variables + outcome + dataset_grade
analysis_grade_df <- analysis_grade %>%
  as.data.frame() %>%
  tibble::as_tibble() %>%
  dplyr::rename(dataset = 1)
combos <- tidyr::crossing(v = covar,
                          o = c("math_mean", "ela_mean"),
                          dataset = analysis_grade_df$dataset)


# Create a function to build quantile regression model
build_quantile_model_50 <- function(v, o, dataset){
  v %>%
    base::paste0(o, " ~ ",.) %>%
    stats::as.formula() %>%
    rq(data = dataset, tau = 0.5)
}



# Build quantile model
quantile_model <- purrr::pmap(combos, build_quantile_model_50)

# Edit dataset combos to include model id & grade
combos_edit <- combos %>%
  tibble::rownames_to_column() %>%
  dplyr::bind_cols(g = rep(c(3, 4, 5), times = length(quantile_model)/3)) %>%
  dplyr::select(-dataset)

# Get model coefficients
quantile_coeff <- quantile_model %>%
  purrr::map(broomExtra::tidy_parameters) %>%
  purrr::map_df(dplyr::bind_rows, .id = "rowname") %>%
  dplyr::left_join(combos_edit ,
                   by = "rowname") %>%
  dplyr::left_join(table1 %>% dplyr::select(covar, iqr, range) %>%
                     dplyr::rename(term = covar),
                   by = "term") %>%
  dplyr::left_join(outcome_sd %>% dplyr::select(o, outcome_sd, g),
                   by = c("o", "g")) %>%
  dplyr::mutate(effect_iqr = estimate * iqr,
                effect_range = estimate * range) %>%
  dplyr::mutate(effect_iqr_sd = (abs(effect_iqr) >= outcome_sd),
                effect_range_sd = (abs(effect_range) >= outcome_sd))



# Efron’s pseudo r-squared
quantile_r2 <- purrr::map_df(quantile_model, function(x){rcompanion::accuracy(list(x)) %>% purrr::pluck(2)}) %>%
  tibble::rownames_to_column() %>%
  dplyr::left_join(combos_edit, by = "rowname")

# Prepare & Export
test <- quantile_coeff %>%
  dplyr::left_join(quantile_r2 %>% dplyr::select(rowname, Efron.r.squared),
                   by = "rowname")
quantile_coeff_edit <- test %>%
  dplyr::left_join(test %>%
                     dplyr::group_by(v) %>%
                     dplyr::summarise(mean_r = mean(Efron.r.squared, na.rm = TRUE)),
                   by = "v")
rm(test)



# quantile_list <- list(quantile_coeff_edit, quantile_r2)
# openxlsx::write.xlsx(quantile_list, "outputs/quantile.xlsx",
                     # firstRow = TRUE)


# Create excel file for review
df <- quantile_coeff_edit

col_pvalue <- which(colnames(df)=="p.value")
col_effect <- which(colnames(df)=="effect_iqr_sd")

wb <- openxlsx::buildWorkbook(df %>% dplyr::arrange(v, o, g))
# conditional format p-value column
openxlsx::conditionalFormatting(wb, "Sheet 1", cols = col_pvalue, rows = 1:nrow(df),
                                  rule = "<0.05",
                                  style = openxlsx::createStyle(fontColour = "#006100", bgFill = "#C6EFCE"))
# condional format effect>sd columns
openxlsx::conditionalFormatting(wb, "Sheet 1", cols = col_effect:(col_effect+1), rows = 1:nrow(df),
                                rule = "TRUE",
                                style = openxlsx::createStyle(fontColour = "#006100", bgFill = "#C6EFCE"),
                                type = "contains")
# Autofit column width
openxlsx::setColWidths(wb, "Sheet 1", cols = 1:ncol(df), widths = rep("auto", ncol(df)))
# hide column rowname
openxlsx::setColWidths(wb, "Sheet 1", cols = 1,
                       widths = 8.43, hidden = TRUE)
# Add a data filter
openxlsx::addFilter(wb, "Sheet 1", rows = 1, cols = 1:ncol(df))

# save workbook
openxlsx::saveWorkbook(wb, "outputs/quantile.xlsx", TRUE)

# Quantile - Multiple variables -------------------------------------------



covar_multivar <- c(
  # "ses_edu_highschoolmore",
  # "ses_medianhhincome_log10",
  # "ethnicity_african_american",
  # "ethnicity_asian",
  # "ethnicity_hispanic",
  # "ethnicity_native_american",
  # "ethnicity_pacific_islander",
  # "ethnicity_two_or_more",
  # "ethnicity_white",
  # "gender_f",
  # "gifted_both",
  # "gifted_ela",
  # # "gifted_math",
  # "gifted_not_identified_as_gifted_talented",
  # # "gifted_others",
  # "specialed_iep",
  # "school_totalunexcuseddays",
  # "school_totaldaysmissed",
  # "school_pct_frl_avg"
  # "school_student_enrollment_avg"
  # "ieq_thermal",
  # "ieq_acoustics"
  # "ieq_visual_cat",
  # "ieq_indoor_cat"
)

covar_multivar <- c(
  "school_pct_frl_avg",
  "ethnicity_african_american",
  "ethnicity_asian",
  "ethnicity_hispanic",
  # "ethnicity_native_american",
  "ethnicity_two_or_more",
  # "ethnicity_pacific_islander",
  # "ethnicity_others1",
  "ethnicity_others2",
  # "ethnicity_white",
  "specialed_iep",
  # "ses_medianhhincome_log10",
  # "ses_poverty_6to17",
  # "school_student_enrollment_avg"
  # "ieq_thermal"
  # "ieq_acoustics"
  # "ieq_visual"
  "ieq_indoor"
)



# Create all combinations for explanatory variables + outcome + grade
combos <- tidyr::crossing(v = covar_multivar %>% paste0(collapse = " + "),
                          o = c("math_mean", "ela_mean"),
                          dataset = analysis_grade_df$dataset)


# Build quantile model
quantile_multi_model <- purrr::pmap(combos, build_quantile_model_50)




stargazer::stargazer(quantile_multi_model,
                     # rq.se = "boot",
                     column.labels = rep(c("Grade 3", "Grade 4", "Grade 5"), times = 2),
                     # covariate.labels = ,
                     dep.var.labels = c("ELA", "Math"),
                     # omit = c("Constant"),
                     model.numbers = FALSE,
                     model.names =  FALSE,
                     keep.stat = c('n'),
                     type ='text',
                     style = "default",
                     # out = "outputs/tables/Aim1/school_model_ieq_thermal.html",
                     # out = "outputs/tables/Aim1/school_model_ieq_acoustics.html",
                     # out = "outputs/tables/Aim1/school_model_ieq_visual.html",
                     # out = "outputs/tables/Aim1/school_model_ieq_indoor.html",
                     summary = TRUE)


# Table X  ----------------------------------------------------------------

analysis_grade <- load_analysis_school %>%
  dplyr::mutate(grade = grade %>% as.character()) %>%
  dplyr::filter(grade %in% c("30", "40", "50")) %>%
  dplyr::group_split(grade) %>%
  `names<-`(c("30", "40", "50"))

varname_df <- tibble::tibble(
  term = c("ieq_indoor", "ieq_thermal", "ieq_acoustics", "ieq_visual",
           "ieq_indoor_cat2", "ieq_visual_cat2", "ieq_visual_cat3",
           "ses_medianhhincome_log10",
           "testscore_ethnicityAfrican American", "testscore_ethnicityAsian",
           "testscore_ethnicityHispanic", "testscore_ethnicityNative American",
           "testscore_ethnicityPacific Islander", "testscore_ethnicityTwo or more",
           "testscore_genderFemale",
           "testscore_giftedGifted in Both Math and ELA", "testscore_giftedGifted in ELA",
           "testscore_giftedGifted in Math", "testscore_giftedGifted in Other Fields",
           "testscore_special_edIEP",
           "testscore_totalunexcuseddays_cat2", "testscore_totalunexcuseddays_cat3",
           "testscore_totalunexcuseddays_cat4",
           "testscore_totaldaysmissed",
           "school_pct_frl_avg"),
  variable_label = c("Indoor Air Quality", "Thermal Comfort", "Acoustics", "Visual Quality",
                     "   >80",
                     "   >50 - 60",
                     "   >60",
                     "Log10 median household income",
                     "   African American", "   Asian",
                     "   Hispanic", "   Native American",
                     "   Pacific Islander", "   Two or more",
                     "   Female",
                     "   Gifted in Both Math and ELA", "   Gifted in ELA",
                     "   Gifted in Math", "   Gifted in Other Fields",
                     "   IEP",
                     "   >0 - 2", "   >2 - 5", "   >5",
                     "Missed days",
                     "School % free or reduced-price lunch"))

outcome <- c("math_mean", "ela_mean")
exp <- c("ieq_indoor", "ieq_thermal", "ieq_acoustics", "ieq_visual")
# covar <- c("r_SE_nat")
covar <- c("school_pct_frl_avg + ethnicity_african_american + ethnicity_asian + ethnicity_hispanic + ethnicity_two_or_more + ethnicity_others2 + specialed_iep")
map_df <- tidyr::crossing(outcome, exp, covar,  df = analysis_grade)
rm(outcome, exp, covar)

# outcome <- "math_mean"
# exp <- "ieq_indoor"
# covar <- c("school_pct_frl_avg + ethnicity_african_american + ethnicity_asian + ethnicity_hispanic + ethnicity_two_or_more + ethnicity_others2 + specialed_iep + ses_medianhhincome_log10")
# df <- analysis_grade[[1]]

build_quantile_model_median <- function(outcome, exp, covar, df){
  grade <- df$grade[1]
  tictoc::tic(paste0(grade, " ", outcome, " ~ ", exp, covar))
  # get model info
  exp_name <- exp
  # Create vector of covar & exp
  covar_chr <- covar %>% stringr::str_split(pattern = "\\+") %>%
    dplyr::first() %>% stringr::str_trim(side = "both")
  exp_chr <- exp %>% stringr::str_split(pattern = "\\+") %>%
    dplyr::first() %>% stringr::str_trim(side = "both") %>%
    purrr::discard(~.x == "")
  # # Remove NA observations
  # df <- df %>%
  #   dplyr::select(c(tidyselect::all_of(c(outcome)),
  #                   tidyselect::all_of(covar_chr),
  #                   tidyselect::any_of(exp_chr),
  #                   "cdenumber")) %>%
  #   tidyr::drop_na()
  # Create formula
  formula <- paste0(outcome, " ~ ", exp, " + ", covar) %>% stats::as.formula()
  # Run model
  model <- df %>% quantreg::rq(formula, data = ., tau = 0.5)
  nobs <- nrow(df)
  model_p <- quantreg::summary.rq(model, se = 'boot')$coefficients[,4] %>% as.numeric()
  model_tidy <- model %>%
    broom::tidy() %>%
    dplyr::mutate(outcome = outcome,
                  grade = grade,
                  exp_name = exp_name,
                  nobs = nobs,
                  p.value = model_p)
  tictoc::toc()
  model_tidy
}

quantile <- map_df %>% purrr::pmap(build_quantile_model_median)

table_main <- quantile %>%
  dplyr::bind_rows() %>%
  dplyr::group_split(outcome, grade) %>%
  purrr::map(function(x){
    table <- x %>%
      dplyr::filter(term %in% c(
        # "(Intercept)",
        "ieq_indoor", "ieq_thermal", "ieq_acoustics", "ieq_visual")) %>%
      dplyr::mutate(ci95 = paste0("(", sprintf("%.2f", conf.low),
                                  ", ", sprintf("%.2f", conf.high),
                                  ")"
      )) %>%
      dplyr::mutate(p = dplyr::case_when(p.value > 0.05 ~"",
                                         p.value %>% dplyr::between(0.01, 0.05) ~ "*",
                                         p.value %>% dplyr::between(0.001, 0.01) ~ "**",
                                         p.value < 0.001 ~"***"),
                    beta = sprintf("%.2f", estimate) %>% paste0(p)) %>%
      dplyr::left_join(varname_df, by = "term") %>%
      dplyr::mutate(variable_label = ifelse(is.na(variable_label), term, variable_label)) %>%
      dplyr::mutate(model = paste0(outcome, "|", grade, "|", exp_name))
    table <- table[c(1, 3, 2, 4), ]
  }
  )

table_main_wide <- table_main
table_main_wide <- c(list(table_main_wide[1:3]), list(table_main_wide[4:6])) %>%
  purrr::map_depth(2, function(x){
    schoollevel <- ifelse(x$grade[1] == "30", "Grade 3",
                          ifelse(x$grade[1] == "40", "Grade 4", "Grade 5"))
    label <- paste0(schoollevel, " (n=", scales::label_comma(accuracy = 1)(x$nobs[1]), ")")
    beta <- x %>%
      dplyr::select(variable_label, beta, ci95, model) %>%
      dplyr::bind_rows(tibble::tibble(variable_label = c("Variable", NA),
                                      beta = c(label, "\U03B2"),
                                      ci95 = c(NA, "95% CI"),
                                      model = NA),.)
    nrow <- (table_main_wide %>% purrr::map_dbl(~nrow(.x)) %>% max()) +2

    if(nrow(beta) <  nrow) {
      beta <- beta %>%
        dplyr::bind_rows(tibble::tibble(variable_label = "   >60",
                                        beta = NA, ci95 = NA, model = NA))
    }
    beta <- beta %>% dplyr::rename_with(~paste0("x", x$grade[1], .))
  }) %>%
  purrr::map(~.x %>%
               dplyr::bind_cols() %>%
               dplyr::select(-tidyselect::ends_with("model"),
                             -c("x40variable_label", "x50variable_label"))
  ) %>%
  dplyr::bind_rows()

readr::write_csv(table_main_wide,
                 "outputs/tables/Aim1/table_school_model.csv")








# Testing codes -----------------------------------------------------------





# Edit dataset combos to include model id & grade
combos_edit <- combos %>%
  tibble::rownames_to_column() %>%
  dplyr::bind_cols(g = rep(c(3, 4, 5), times = length(quantile_multi_model)/3)) %>%
  dplyr::select(-dataset)

# Get model coefficients
quantile_multi_coeff <- quantile_multi_model %>%
  purrr::map(broomExtra::tidy_parameters) %>%
  purrr::map_df(dplyr::bind_rows, .id = "rowname") %>%
  dplyr::left_join(combos_edit ,
                   by = "rowname") %>%
  dplyr::left_join(table1 %>% dplyr::select(covar, iqr, range) %>%
                     dplyr::rename(term = covar),
                   by = "term") %>%
  dplyr::left_join(outcome_sd %>% dplyr::select(o, outcome_sd, g),
                   by = c("o", "g")) %>%
  dplyr::mutate(effect_iqr = estimate * iqr,
                effect_range = estimate * range) %>%
  dplyr::mutate(effect_iqr_sd = (abs(effect_iqr) >= outcome_sd),
                effect_range_sd = (abs(effect_range) >= outcome_sd))



# Efron’s pseudo r-squared
quantile_multi_r2 <- purrr::map_df(quantile_multi_model, function(x){rcompanion::accuracy(list(x)) %>% purrr::pluck(2)}) %>%
  tibble::rownames_to_column() %>%
  dplyr::left_join(combos_edit, by = "rowname")

# Prepare & Export
test <- quantile_multi_coeff %>%
  dplyr::left_join(quantile_multi_r2 %>% dplyr::select(rowname, Efron.r.squared),
                   by = "rowname")
quantile_multi_coeff_edit <- test %>%
  dplyr::left_join(test %>%
                     dplyr::group_by(v) %>%
                     dplyr::summarise(mean_r = mean(Efron.r.squared, na.rm = TRUE)),
                   by = "v")
rm(test)


# Create a table with only variable passing the test
test <- quantile_multi_coeff_edit %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(effect_range_sd_num = dplyr::case_when(effect_range_sd  ~1,
                                                       !effect_range_sd ~0),
                effect_iqr_sd_num = dplyr::case_when(effect_iqr_sd  ~1,
                                                     !effect_iqr_sd ~0))
test1a <- test %>%
  dplyr::select(rowname, term, effect_range_sd_num) %>%
  tidyr::pivot_wider(names_from = rowname, values_from = effect_range_sd_num)
test1b <- test %>%
  dplyr::select(rowname, term, effect_iqr_sd_num) %>%
  tidyr::pivot_wider(names_from = rowname, values_from = effect_iqr_sd_num)
test2 <- dplyr::bind_rows(test1a, test1b) %>%
  dplyr::group_by(term) %>%
  dplyr::summarise_all(.funs = base::sum)

quantile_multi_coeff_effect <- covar_multivar %>%
  tibble::as_tibble() %>%
  dplyr::rename(term = value) %>%
  dplyr::left_join(test2, by = "term")
rm(test, test1a, test1b, test2)

write.csv(quantile_multi_coeff_effect, "outputs/tables/Aim1/school_model_effect_ieq_indoor.csv")


















# Create excel file for review
df <- quantile_multi_coeff_edit

col_pvalue <- which(colnames(df)=="p.value")
col_effect <- which(colnames(df)=="effect_iqr_sd")

wb <- openxlsx::buildWorkbook(df %>% dplyr::arrange(v, o, g))
# conditional format p-value column
openxlsx::conditionalFormatting(wb, "Sheet 1", cols = col_pvalue, rows = 1:nrow(df),
                                rule = "<0.05",
                                style = openxlsx::createStyle(fontColour = "#006100", bgFill = "#C6EFCE"))
# condional format effect>sd columns
openxlsx::conditionalFormatting(wb, "Sheet 1", cols = col_effect:(col_effect+1), rows = 1:nrow(df),
                                rule = "TRUE",
                                style = openxlsx::createStyle(fontColour = "#006100", bgFill = "#C6EFCE"),
                                type = "contains")
# Autofit column width
openxlsx::setColWidths(wb, "Sheet 1", cols = 1:ncol(df), widths = rep("auto", ncol(df)))
# hide column rowname
openxlsx::setColWidths(wb, "Sheet 1", cols = 1,
                       widths = 8.43, hidden = TRUE)
# Add a data filter
openxlsx::addFilter(wb, "Sheet 1", rows = 1, cols = 1:ncol(df))

# save workbook
openxlsx::saveWorkbook(wb, "outputs/quantile_multi.xlsx", TRUE)








# https://lachlandeer.github.io/post/quantile-regression-with-tidyverse/

# Estimating One Quantile Regression
# Quantile regression is going to allow our model to have different average effects along the distribution of the dependent variable (in our case ltotexp). quantreg’s rq() function will allow us to estimate these regressions. If we are interested in the model around one quantile, for example around the median, we can estimate the model as follows:

quant_reg <- quantreg::rq(ela_mean ~ ieq_visual,
                          data = analysis_grade[[5-2]],
                          tau = 0.5
                          )

summary(quant_reg)

# Estimating Multiple Quantiles
# Typically we aren’t only interested in estimating a quantile regression around one point in the distribution, but instead across multiple quantiles. We can to this by creating a vector with the quantiles we are interested in and then estimate the model for each quantile. We are going to do this using purrr’s map() function:
# Since we want to use the function `stargazer::stargazer` later, we have to load the library `quantreg` and run `quantreg::rq()` without referring to the package, so only `rq()`
quants <- c(0.1, 0.25, 0.5, 0.75, 0.9)
qr_res <- purrr::map(quants, ~rq(ela_mean ~ ieq_visual,
                          data = analysis_grade[[5-2]],
                          tau = 0.5)
              )


# The above snippet says that for each element of the vector quants estimate the quantile regression using each element as the tau value (i.e. quantile of interest).1 The output of the map command is a list:
typeof(qr_res)

# There’s 5 elements to of the list:
length(qr_res)

# Each element is the output of a quantile regression, one for each of the quantiles we wanted. For example:
summary(qr_res[[1]])

# And:
summary(qr_res[[3]])


# Summarising Regression Output
# Now that we have the results in hand we want to present them in a readable way. We are going to look at two ways - a regression table and a coefficient plot.

# The Regression Table
# We are going to use stargazer to produce the estimates table.2 We want the OLS results alongside the quantile regression estimates so we pass these across as the first two arguments (stargazer will unpack the list of models inside qr_res for us). We must specify how we want the standard errors of the quantile regression to be computed (or accept a default), we’ve gone with bootstrapped standard errors. The rest of the lines arguments tidy up the table a little. type = 'text' prints the resulting table as plain text, switch to ‘latex’ if you want to put the table in a LaTex or Markdown document.

stargazer::stargazer(qr_res,
                     rq.se = "boot",
                     column.labels = c(paste("tau = ", quants)),
                     # covariate.labels = c("Supplementary Insurance == 1",
                     #                      "# Chronic Health Conditions",
                     #                      "Age",
                     #                      "Female",
                     #                      "White"),
                     dep.var.labels = "test scores",
                     # omit = c("Constant"),
                     model.numbers = FALSE,
                     model.names =  FALSE,
                     # keep.stat = c('n'),
                     type ='html',
                     style = "default",
                     summary = TRUE,
                     out = "outputs/test.html")


summary(qr_res[[1]])







# multiple var model





