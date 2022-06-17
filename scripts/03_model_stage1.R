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

analysis_bygrade <- analysis %>%
  dplyr::mutate(grade = grade %>% as.character()) %>%
  dplyr::filter(grade %in% c("50", "80", "90")) %>%
  dplyr::group_split(grade) %>%
  purrr::map(~.x %>% dplyr::distinct(studentkey, .keep_all = TRUE)) %>%
  `names<-`(c("50", "80", '90'))

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
  dplyr::filter(!(varname %in% c("ses_crowding",
                                 "ses_poverty_all", "ses_renter_all",
                                 "ses_unemployed", "testscore_totalunexcuseddays",
                                 "school_student_enrollment_avg",
                                 "ieq_visual",
                                 "ieq_indoor"))) %>%
  dplyr::arrange(varname) %$% as.vector(varname)


covar_num <- vars_list %>%
  dplyr::filter(varname %in% covar & varclass == "numeric") %>%
  dplyr::arrange(varname) %$% as.vector(varname)


# covar_num <- c("ses_medianhhincome")


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
  if (vars_list[vars_list$varname == covar_num[varorder], "varclass"] == "numeric") {
    p1 <- ggplot2::ggplot(result_res, aes_string(x = covar_num[varorder], y = ".observed")) +
      ggplot2::geom_point(aes(colour = (cdenumber)),
                          shape = "circle open",
                          show.legend = FALSE) +
      ggplot2::geom_smooth(method = "loess", se = TRUE)
  } else {
    p1 <- ggplot2::ggplot(result_res, aes_string(x = covar_num[varorder], y = ".observed")) +
      ggplot2::geom_boxplot(aes(colour = (cdenumber)),
                            shape = "circle open",
                            show.legend = FALSE)
  }
  # Residual plot: .resid  & .fitted
  p2 <- ggplot2::ggplot(result_res, aes(x = .fitted, y = .resid)) +
    ggplot2::geom_point(shape = "circle open") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_smooth(method = "loess", se = FALSE)
  # Residual plot: .resid & covar_num
  p3 <- ggplot2::ggplot(result_res, aes_string(x = covar_num[varorder], y = ".resid")) +
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
                                                         covar_num[varorder],
                                                         sep = " - ")))
  ggsave(filename = paste0(paste("outputs/figures/Aim1/bivar_dx_GEOID", covar_num[varorder], outcome, grade, sep = "_"), ".jpg"),
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
    result <- covar_num %>%
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
    for (varorder in 1:length(covar_num)) {
      if (covar_num[varorder] %in% covar_num){
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
  "r_SE_nat",
  # "pct_school_administration",
  # "pct_operations_maintenance",
  "per_school_administration",
  "per_operations_maintenance",
  # "ses_edu_highschoolmore",
  # "ses_medianhhincome",
  # "ses_medianhhincome_log10",
  # "testscore_ethnicity",
  # "testscore_gender",
  # "testscore_gifted",
  # "testscore_special_ed",
  # "testscore_totalunexcuseddays_cat",
  # "testscore_totaldaysmissed",
  # "school_pct_frl_avg",
  # "school_student_enrollment_avg_cat"
  "ieq_thermal"
  # "ieq_acoustics"
  # "ieq_visual_cat"
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


openxlsx::write.xlsx(multivar_list, "outputs/tables/Aim1/student_model_ses_student_school_ieq_thermal.xlsx",
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
                     out = "outputs/tables/Aim1/student_model_ses_student_school_ieq_thermal.html",
                     summary = TRUE)









# Binary outcome ----------------------------------------------------------

# * Intercept model -------------------------------------------------------
df <- analysis %>% dplyr::filter(!is.na(mathperf))
m <- lme4::glmer(mathperf ~ 1 + (1 | cdenumber) + (1| GEOID), data = df,
                 family = binomial,
                 control = lme4::glmerControl(optimizer = "bobyqa"),
                 nAGQ = 1)
summary(m)
jtools::summ(m)



# * Bivariate -------------------------------------------------------------

outcome <- c("mathperf", "elaperf")
exp <- c("ieq_indoor", "ieq_thermal", "ieq_acoustics", "ieq_visual",
         "r_SE_nat")
map_df <- tidyr::crossing(outcome, exp)
rm(outcome, exp)

build_glmer_bivar <- function(outcome, exp){
  tictoc::tic(paste0(outcome, "-", exp))
  formula <- paste0(outcome, " ~ ", exp, "+ (1 | cdenumber) + (1| GEOID)") %>% as.formula()
  df <- df %>%
    dplyr::filter(!is.na({{outcome}}) &
                    !is.na({{exp}}))
  m <- lme4::glmer(formula,
                   data = df,
                   family = binomial,
                   control = lme4::glmerControl(optimizer = "bobyqa"),
                   nAGQ = 1)
  tictoc::toc()
  m
}

df <- analysis
bivar <- map_df %>% purrr::pmap(build_glmer_bivar)


# Multivariate ------------------------------------------------------------

outcome <- c("mathperf", "elaperf")
exp <- c("ieq_indoor", "ieq_thermal", "ieq_acoustics", "ieq_visual")
covar <- c(" + r_SE_nat")
map_df <- tidyr::crossing(outcome, exp, covar)
rm(outcome, exp, covar)

build_glmer_multi <- function(outcome, exp, covar) {
  # tictoc::tic(paste0(outcome, "-", exp))
  formula <- paste0(outcome, " ~ ", exp, covar, "+ (1 | cdenumber) + (1| GEOID)") %>% as.formula()
  # df <- df %>%
  #   dplyr::filter(!is.na(.data[[{{outcome}}]]) &
  #                   !is.na(.data[[{{exp}}]]) &
  #                   !is.na(r_SE_nat) &
  #                   !is.na(per_operations_maintenance) &
  #                   !is.na(per_school_administration))
  m <- lme4::glmer(formula,
                   data = df,
                   family = binomial,
                   # control = lme4::glmerControl(optimizer = "bobyqa"),
                   nAGQ = 1)
  # tictoc::toc()
  m
}

multivar <- map_df %>% purrr::pmap(build_glmer_multi)



# Table X Main ------------------------------------------------------------

outcome <- c("mathscalescore", "elascalescore")
exp <- c("ieq_indoor_cat", "ieq_thermal", "ieq_acoustics", "ieq_visual_cat")
# exp <- c("")
# covar <- c("r_SE_nat")
# covar <- c("ses_medianhhincome_log10 + testscore_ethnicity + testscore_gender + testscore_gifted + testscore_special_ed + testscore_totalunexcuseddays_cat + testscore_totaldaysmissed + school_pct_frl_avg")
covar <- c("")
map_df <- tidyr::crossing(outcome, exp, covar,  df = analysis_bygrade)
rm(outcome, exp, covar)

# outcome <- "mathscalescore"
# exp <- "ieq_indoor_cat"
# covar <- "ses_medianhhincome_log10 + testscore_ethnicity + testscore_gender + testscore_gifted + testscore_special_ed + testscore_totalunexcuseddays_cat + testscore_totaldaysmissed + school_pct_frl_avg"
# df <- analysis_bygrade[[1]]


build_nlme_model <- function(outcome, exp, covar, df) {
  grade <- df$grade[1]
  tictoc::tic(paste0(grade, " ", outcome, " ~ ", exp, covar))
  # get model info
  exp_name <- exp
  # Create vector of covar and exp
  covar_chr <- covar %>% stringr::str_split(pattern = "\\+") %>%
    dplyr::first() %>% stringr::str_trim(side = "both")
  exp_chr <- exp
  # Remove NA observations
  df <- df %>%
    dplyr::select(c(tidyselect::all_of(c(outcome)),
                    tidyselect::any_of(covar_chr),
                    tidyselect::any_of(exp_chr),
                    "cdenumber", "GEOID")) %>%
    tidyr::drop_na()
  # Create formula
  # formula <- paste0(outcome, " ~ ", exp, " + ", covar) %>% as.formula()
  formula <- paste0(outcome, " ~ ", exp) %>% as.formula()
    # formula <- paste0(outcome, " ~ ", exp, " + ", covar, " + (1|cdenumber) + (1 | GEOID)") %>% as.formula()
  # Run model
  lme_nlme <- df %>%
    nlme::lme(fixed = formula,
              # random = ~1|cdenumber/GEOID,
              random=list(~1|cdenumber, ~1|GEOID),
              data = .,
              method = "ML")
  # lme4::lmer(formula, data = .)
  lme_nlme_glance <- lme_nlme %>%  broom.mixed::glance() %>% dplyr::select(nobs)
  lme_nlme_tidy <- summary(lme_nlme) %>% coef() %>%
    tibble::as_tibble(rownames = NA) %>% tibble::rownames_to_column("term") %>%
    dplyr::bind_cols(lme_nlme_glance) %>%
    dplyr::rename(estimate = Value, std.error = Std.Error, p.value = `p-value`) %>%
    dplyr::mutate(outcome = outcome,
                  grade = grade,
                  exp_name = exp_name)
  tictoc::toc()
  lme_nlme_tidy
}

lme_nlme <- map_df %>% purrr::pmap(build_nlme_model)

table_main <- lme_nlme %>%
  dplyr::bind_rows() %>%
  dplyr::group_split(outcome, grade) %>%
  purrr::map(function(x) {
    table <- x %>%
      dplyr::filter(term %in% c(
        # "(Intercept)",
        "ieq_indoor_cat2", "ieq_thermal", "ieq_acoustics",
        "ieq_visual_cat2", "ieq_visual_cat3")) %>%
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
    table <- dplyr::bind_rows(
      table[c(1, 3), ],
      tibble::tibble(variable_label = c("Indoor Air Quality", "   0-80 (Reference group)"),
                     beta = c(NA, "1.0"),
                     model = table$model[1]),
      table[2, ],
      tibble::tibble(variable_label = c("Visual Quality", "   0-50 (Reference group)"),
                     beta = c(NA, "1.0"),
                     model = table$model[1]),
      table[4:nrow(table), ]
    )
  }
  )



table_ses <- lme_nlme %>%
  dplyr::bind_rows() %>%
  dplyr::group_split(outcome, grade) %>%
  purrr::map(function(x) {
    table <- x %>%
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
    table <- dplyr::bind_rows(
      table[1:2, ],
      tibble::tibble(variable_label = c("Race/ethnicity", "   White (Reference group)"),
                     beta = c(NA, "1.0"),
                     model = table$model[1]),
      table[3:8, ],
      tibble::tibble(variable_label = c("Gender", "   Male (Reference group)"),
                     beta = c(NA, "1.0"),
                     model = table$model[1]),
      table[9, ],
      tibble::tibble(variable_label = c("Gifted/talented program participation status", "   Not Identified as Gifted (Reference group)"),
                     beta = c(NA, "1.0"),
                     model = table$model[1]),
      table[10:13, ],
      tibble::tibble(variable_label = c("IEP participation status", "   No IEP (Reference group)"),
                     beta = c(NA, "1.0"),
                     model = table$model[1]),
      table[14, ],
      tibble::tibble(variable_label = c("Unexcused days", "   0 (Reference group)"),
                     beta = c(NA, "1.0"),
                     model = table$model[1]),
      table[15:nrow(table), ]) %>%
      tibble::as_tibble()
    }
    )



# table_main_wide <- table_ses
table_main_wide <- table_main
table_main_wide <- c(list(table_main_wide[1:3]), list(table_main_wide[4:6])) %>%
  purrr::map_depth(2, function(x){
    schoollevel <- ifelse(x$grade[1] == "50", "Grade 5 Elementary",
                          ifelse(x$grade[1] == "80", "Grade 8 Middle", "Grade 9 High"))
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
                             -c("x80variable_label", "x90variable_label"))
  ) %>%
  dplyr::bind_rows()



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
# openxlsx::saveWorkbook(wb, "outputs/tables/Aim1/table_main_DAG_wide_formatted.xlsx", TRUE)
# openxlsx::saveWorkbook(wb, "outputs/tables/Aim1/table_main_datadriven_wide_formatted.xlsx", TRUE)
# openxlsx::saveWorkbook(wb, "outputs/tables/Aim1/table_main_ses_wide_formatted.xlsx", TRUE)
openxlsx::saveWorkbook(wb, "outputs/tables/Aim1/table_main_unadj_wide_formatted.xlsx", TRUE)

rm(table_main, table_main_wide)
rm(lme_nlme)

# Save to disk ------------------------------------------------------------
save_data <- function(dataset.name, file.location, file.location.arc){
  readr::write_csv(dataset.name, paste0(file.location, ".csv")) # Save CSV
  readr::write_csv(dataset.name, paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".csv")) # Archived CSV
  saveRDS(dataset.name, file = paste0(file.location, ".rds")) # Save RDS
  saveRDS(dataset.name, file = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".rds")) # ARchived RDS
}

save_data(bivar_coeff, "DATA/Processed/Aim1/aim1_bivar_coeff", "DATA/Processed/Aim1/Archived/aim1_bivar_coeff")
save_data(bivar_effect, "DATA/Processed/Aim1/aim1_bivar_effect", "DATA/Processed/Aim1/Archived/aim1_bivar_effect")






