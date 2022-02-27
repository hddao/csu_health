# ---------------------------------------------------------------------------- #
# Title: Script for first stage modelling
# Author: Hanh Dung Dao
# Purpose: To model outcome (test scores) with SES variables,
# accounting for clustering by school
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
packages <- c("tidyverse", "magrittr", "tidyselect", "lme4", "jtools", "lmerTest", "gridExtra", "ggsci")

package_check <- lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x, dependencies = TRUE)
  library(x, character.only = TRUE)
})
# * Declare globals -------------------------------------------------------

# Load data ---------------------------------------------------------------
load_analysis <- readr::read_rds("DATA/Processed/Aim1/aim1_analysis.rds")


# Clean data --------------------------------------------------------------

analysis <- load_analysis %>% sf::st_drop_geometry()
str(analysis)

# Create a list object with analysis data by grade
analysis_grade <- analysis %>%
  dplyr::group_by(grade) %>%
  dplyr::group_split()


# Create a data frame for variable names and types
vars_list <- purrr::map_df(analysis, class) %>% t %>% as_tibble(rownames = NA) %>% rownames_to_column() %>%
  dplyr::rename(varname = rowname, varclass = V1) %>%
  dplyr::mutate(vartype = dplyr::case_when(
    varname %in% c("id_dao", "cdenumber", "studentkey", "GEOID") ~ "id",
    endsWith(varname, suffix = "scalescore") ~ "score",
    startsWith(varname, prefix = "testscore_") ~ "student",
    startsWith(varname, prefix = "school_") ~ "school",
    startsWith(varname, prefix = "ieq_") ~ "ieq",
    startsWith(varname, prefix = "ses_") ~ "ses",
    startsWith(varname, prefix = "i") ~ "index",
    # varname %in% c("i1", "i2", "i3", "i4") ~ "index",
    # varname %in% c() ~ "",
    # varname %in% c() ~ "",
    TRUE ~ ""
  ))

# Create a vector with covariate names
covar <- vars_list %>%
  dplyr::filter(vartype %in% c("student", "ses", "school", "ieq")) %>%
  dplyr::filter(!(varname %in% c("ses_medianhhincome", "ses_crowding",
                                 "ses_poverty_all", "ses_renter_all",
                                 "ses_unemployed", "testscore_totalunexcuseddays",
                                 "school_student_enrollment_avg",
                                 "ieq_visual",
                                 "ieq_indoor"))) %>%
  dplyr::arrange(varname) %$% as.vector(varname)


covar_num <- vars_list %>%
  dplyr::filter(varname %in% covar & varclass == "numeric") %>%
  dplyr::arrange(varname) %$% as.vector(varname)


# Descriptive table
table1_student <- skimr::skim(analysis %>% dplyr::select(all_of(covar_num))) %>%
  # dplyr::mutate_if(is.numeric, round, digits = 2) %>%
  dplyr::rename(covar = skim_variable) %>%
  dplyr::mutate(iqr = numeric.p75 - numeric.p25,
                range = numeric.p100 - numeric.p0)

table1_student_g3 <- skimr::skim(analysis %>%
                                   dplyr::filter(grade == "30") %>%
                                   dplyr::select(all_of(covar_num))) %>%
  # dplyr::mutate_if(is.numeric, round, digits = 2) %>%
  dplyr::rename(covar = skim_variable) %>%
  dplyr::mutate(iqr = numeric.p75 - numeric.p25,
                range = numeric.p100 - numeric.p0)

# Outcome SD as threshold dataset
test <- analysis %>% summarise_at(c("mathscalescore", "elascalescore"), sd, na.rm = TRUE)
outcome_sd <- test %>% t %>% tibble::as_tibble() %>%
  dplyr::mutate(outcome = colnames(test)) %>%
  dplyr::rename(outcome_sd = V1)
rm(test)




# Resources ---------------------------------------------------------------

# http://www.rensenieuwenhuis.nl/r-sessions-16-multilevel-model-specification-lme4/
# https://cran.r-project.org/web/packages/multilevelTools/vignettes/lmer-vignette.html
# https://benwhalley.github.io/just-enough-r/fitting-models.html
# https://www.rensvandeschoot.com/tutorials/lme4/
# https://rpubs.com/rslbliss/r_mlm_ws
# https://quantdev.ssri.psu.edu/tutorials/r-bootcamp-introduction-multilevel-model-and-interactions

# https://www.youtube.com/watch?v=8r9bUKUVecc


# Intercept model ---------------------------------------------------------

m0_math_5 <- lme4::lmer(mathscalescore ~ 1 + (1 | cdenumber) + (1| GEOID), data = analysis_grade[[5-2]])
summary(m0_math_5)
jtools::summ(m0_math_5)
lmerTest::ranova(m0_math_5)
rm(m0_math_5)

# ICC for both cdenumber and GEOID was >0. Should be included as random effects
# p-value < 0.05

# Model assumptions -------------------------------------------------------

# Create a function to build bivariate models
build_bivariate_model <- function(varname, outcome, dataset) {
  varname %>%
    sprintf(" ~ %s + (1 | cdenumber) + (1 | GEOID)", .) %>%
    paste0(outcome, .) %>%
    as.formula %>%
    lme4::lmer(data=dataset)
}



# Create a function to save dx plots
create_dx_plots <- function(outcome, grade, varorder) {
  result_res <- broomExtra::augment(bivar_model[[outcome]][[grade-2]][[varorder]]) %>%
    dplyr::mutate(.observed = .fitted + .resid,
                  .stdresid = base::scale(.resid),
                  .absresid = abs(.resid))
  # Scatter plot/boxplot
  if (vars_list[vars_list$varname == covar[varorder], "varclass"] == "numeric") {
    p1 <- ggplot2::ggplot(result_res, aes_string(x = covar[varorder], y = ".observed")) +
      ggplot2::geom_point(aes(colour = (cdenumber)),
                          shape = "circle open",
                          show.legend = FALSE) +
      ggplot2::geom_smooth(method = "loess", se = TRUE)
  } else {
    p1 <- ggplot2::ggplot(result_res, aes_string(x = covar[varorder], y = ".observed")) +
      ggplot2::geom_boxplot(aes(colour = (cdenumber)),
                            shape = "circle open",
                            show.legend = FALSE)
  }
  # Residual plot: .resid  & .fitted
  p2 <- ggplot2::ggplot(result_res, aes(x = .fitted, y = .resid)) +
    ggplot2::geom_point(shape = "circle open") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_smooth(method = "loess", se = FALSE)
  # Residual plot: .resid & covar
  p3 <- ggplot2::ggplot(result_res, aes_string(x = covar[varorder], y = ".resid")) +
    ggplot2::geom_point(shape = "circle open") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_smooth(method = "loess", se = FALSE)
  # Residual plot: .absresid & .fitted
  p4 <- ggplot2::ggplot(result_res, aes(x = .fitted, y = .absresid)) +
    ggplot2::geom_point(shape = "circle open") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_smooth(method = "loess", se = FALSE)
  # Residual histogram
  p5 <- ggplot(result_res, aes(x=.resid)) +
    geom_histogram()
  # QQ plot
  p6 <- ggplot2::ggplot(result_res, aes(sample = .stdresid)) +
    qqplotr::geom_qq_band() +
    ggplot2::stat_qq(shape = "circle open") +
    ggplot2::stat_qq_line()
  # Export
  p <- gridExtra::arrangeGrob(p1, p2, p3, p4, p5, p6,
                              nrow=2, ncol=3,
                              top = grid::textGrob(paste(paste0("Grade ", grade),
                                                         outcome,
                                                         covar[varorder],
                                                         sep = " - ")))
  ggsave(filename = paste0(paste("outputs/figures/bivar_dx_GEOID", covar[varorder], outcome, grade, sep = "_"), ".jpg"),
         plot = p, device = "jpeg",
         width = 9,
         height = 6,
         units = "in")
  p
}


# covar <- c("school_student_enrollment_avg_cat", "ieq_indoor_cat", "ieq_visual_cat")
# covar <- c("school_student_enrollment_avg_cat")
# covar <- c("ieq_indoor_cat")
# covar <- c("ieq_visual_cat")

# test <- analysis %>% dplyr::group_by(grade, ieq_visual_cat) %>% dplyr::summarise(n = n())
# rm(test)

# Run loop to get results for bivariate models
bivar_model <- list(NULL)
start_time <- Sys.time()
for (o in c("mathscalescore", "elascalescore")){
  result_grade <- list(NULL)
  for (g in (3:11)) {
    result <- covar %>%
      map(build_bivariate_model, o, dataset = analysis_grade[[g-2]])
    result_grade[[g-2]] <- result
  }
  bivar_model[[o]] <- result_grade
}
end_time <- Sys.time()
end_time - start_time #1.265887 mins
rm(result, result_grade, o, g)

# a <- broomExtra::augment(bivar_model[["mathscalescore"]][[1]][[2]])
# b <- broomExtra::tidy_parameters(bivar_model[["mathscalescore"]][[1]][[2]])
# c <- broomExtra::glance_performance(bivar_model[["mathscalescore"]][[1]][[2]])

# build_bivariate_model("ieq_visual_cat", "mathscalescore", analysis_grade[[5-2]])
# create_dx_plots("mathscalescore", 3, base::match("testscore_totalunexcuseddays_cat", covar))

# Run loop to save the dx plots
start_time <- Sys.time()
for (outcome in c("mathscalescore", "elascalescore")){
  for (grade in (3:11)) {
    for (varorder in 1:length(covar)) {
      if (covar[varorder] %in% covar){
        create_dx_plots(outcome, grade, varorder)
      }
    }
  }
}
end_time <- Sys.time()
end_time - start_time #1.045368 hours
rm(outcome, grade, varorder)


# Bivariate Analysis ------------------------------------------------------


# Create a function to get statistics from model results
get_stats <- function(result){
  result$coeftable[ , ] %>%
    tibble::as_tibble(rownames = NA) %>%
    tibble::rownames_to_column() %>%
    dplyr::rename(covar = rowname) %>%
    dplyr::filter(covar != "(Intercept)") %>%
    dplyr::mutate(n = nrow(result$model@frame))
}

# Run loop to get stats for multiple outcomes and grades

bivar_coeff_list <- list(NULL)
start_time <- Sys.time()
for (o in c("mathscalescore", "elascalescore")){
  result_grade <- list(NULL)
  for (g in (3:11)) {
    result <- covar %>%
      purrr::map(build_bivariate_model, o, dataset = analysis_grade[[g-2]]) %>%
      purrr::map(jtools::summ) %>%
      purrr::map(get_stats) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(outcome = o,
                    grade = g)
    result_grade[[g-2]] <- result
  }
  bivar_coeff_list[[o]] <- result_grade
}
bivar_coeff <- purrr::map_df(purrr::map(bivar_coeff_list[-1], bind_rows), bind_rows)
end_time <- Sys.time()
end_time - start_time # Time difference of 3.150187 mins
rm(result, result_grade)




# Create result table with all the beta coefficients
bivar_coeff <- bivar_coeff %>%
  dplyr::left_join(table1_student, by = "covar") %>%
  dplyr::left_join(outcome_sd, by = "outcome") %>%
  dplyr::mutate(effect_range = Est. * range)

# Create result table with  median effect size for only numeric variables
bivar_effect <- bivar_coeff %>%
  dplyr::select(covar, outcome, effect_range) %>%
  dplyr::group_by(covar, outcome) %>%
  dplyr::summarise_at(c("effect_range"), median, na.rm = TRUE) %>%
  # dplyr::filter(!is.na(effect_range)) %>%
  dplyr::left_join(outcome_sd, by = "outcome") %>%
  dplyr::mutate(effect_range_greater_outcome_sd = abs(effect_range) >= outcome_sd,
                effect_range_greater_half_outcome_sd = abs(effect_range) >= outcome_sd/2) %>%
  dplyr::mutate(normality = NA, linearity = NA, constantvariance = NA)

bivar_list <- list(bivar_coeff, bivar_effect)

openxlsx::write.xlsx(bivar_list, "outputs/bivar_list_GEOID_ses_student_school_ieqs.xlsx",
                     firstRow = TRUE,
                     overwrite = TRUE)

# Multivariable -----------------------------------------------------------


# Create a function to build multivariable models
build_multivariable_model <- function(varname, outcome, grade, ...) {
  varname %>%
    base::sprintf(" ~ %s + (1|cdenumber) + (1 | GEOID)", .) %>%
    base::paste0(outcome, .) %>%
    stats::as.formula() %>%
    lme4::lmer(data = analysis_grade[[grade-2]])
}

# Create a function to get multivariable models' beta coefficients
get_multivariable_coeff <- function(varname, outcome, grade, ...) {
 model <- varname %>%
   base::sprintf(" ~ %s + (1|cdenumber) + (1 | GEOID)", .) %>%
   base::paste0(outcome, .) %>%
   stats::as.formula() %>%
   lme4::lmer(data = analysis_grade[[grade-2]])
 formula_cat <- model@call %>% deparse %>% paste0(collapse = "")
 coeff <- broomExtra::tidy_parameters(model) %>%
   dplyr::mutate(n = nrow(model@frame),
                 outcome = {{outcome}},
                 grade = {{grade}}) %>%
   dplyr::mutate(formula = formula_cat)
}


# List of variables to be considered
covar_multivar <- c(
  "ses_edu_highschoolmore",
                    "ses_medianhhincome_log10",
                    "testscore_ethnicity",
                    "testscore_gender",
                    "testscore_gifted",
                    "testscore_special_ed",
                    "testscore_totalunexcuseddays_cat",
                    "testscore_totaldaysmissed",
                    "school_pct_frl_avg",
                    "school_student_enrollment_avg_cat",
                    "ieq_thermal",
                    "ieq_acoustics",
                    "ieq_visual_cat",
                    "ieq_indoor_cat"

                    )

covar_multivar <- c(
  # "ses_edu_highschoolmore",
  "ses_medianhhincome_log10",
  "testscore_ethnicity",
  "testscore_gender",
  "testscore_gifted",
  "testscore_special_ed",
  "testscore_totalunexcuseddays_cat",
  "testscore_totaldaysmissed",
  "school_pct_frl_avg",
  # "school_student_enrollment_avg_cat"
  # "ieq_thermal"
  # "ieq_acoustics"
  "ieq_visual_cat"
  # "ieq_indoor_cat"
)

# Create all combinations for explanatory variables + outcome + grade
combos <- tidyr::crossing(v = covar_multivar %>% paste0(collapse = " + "),
                          o = c("mathscalescore", "elascalescore"),
                          g = 4:9)

# Get multivariable model results
multivar_model <- purrr::pmap(combos, build_multivariable_model)

multivar_perf <- multivar_model %>%
  purrr::map(broomExtra::glance_performance) %>%
  purrr::map_df(dplyr::bind_rows) %>%
  dplyr::bind_cols(combos)

multivar_coeff <- purrr::pmap(combos, get_multivariable_coeff) %>%
  purrr::map_df(dplyr::bind_rows) %>%
  dplyr::left_join(table1_student %>% dplyr::select(covar, iqr, range) %>%
                     dplyr::rename(term = covar),
                   by = "term")

multivar_list <- list(multivar_coeff, multivar_perf)


openxlsx::write.xlsx(multivar_list, "outputs/tables/Aim1/student_model_ses_student_school_ieq_visual.xlsx",
                     firstRow = TRUE,
                     overwrite = TRUE)

stargazer::stargazer(multivar_model[c(2,5,6,8,11,12)],
                     column.labels = rep(c("Grade 5", "Grade 8", "Grade 9"), times = 2),
                     # covariate.labels = ,
                     dep.var.labels = c("ELA", "Math"),
                     # omit = c("Constant"),
                     model.numbers = FALSE,
                     model.names =  FALSE,
                     keep.stat = c('n'),
                     type ='html',
                     style = "default",
                     out = "outputs/tables/Aim1/student_model_ses_student_school_ieq_visual.html",
                     summary = TRUE)








# Save to disk ------------------------------------------------------------
save_data <- function(dataset.name, file.location, file.location.arc){
  readr::write_csv(dataset.name, paste0(file.location, ".csv")) # Save CSV
  readr::write_csv(dataset.name, paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".csv")) # Archived CSV
  saveRDS(dataset.name, file = paste0(file.location, ".rds")) # Save RDS
  saveRDS(dataset.name, file = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".rds")) # ARchived RDS
}

save_data(bivar_coeff, "DATA/Processed/Aim1/aim1_bivar_coeff", "DATA/Processed/Aim1/Archived/aim1_bivar_coeff")
save_data(bivar_effect, "DATA/Processed/Aim1/aim1_bivar_effect", "DATA/Processed/Aim1/Archived/aim1_bivar_effect")






