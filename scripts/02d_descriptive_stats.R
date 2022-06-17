# ---------------------------------------------------------------------------- #
# Title: Script to create descriptive stats for Aim 1
# Author: Hanh Dung Dao
# Purpose: Create descriptive stats of variables for exposure (measured indoor
# environmental quality, IEQ), outcome (student standardized test scores) and
# covariate (TBA, tk)
# ---------------------------------------------------------------------------- #



# Preparation -------------------------------------------------------------
rm(list = ls())


# * Load sources ----------------------------------------------------------
source("scripts/Functions/save_data.R")


# * Declare globals -------------------------------------------------------



# Load data ---------------------------------------------------------------
load_analysis <- readr::read_rds("DATA/Processed/Aim1/aim1_analysis.rds") %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(grade = as.character(grade)) %>%
  dplyr::filter(grade %in% c("50", "80", "90"))
varname_df <- readr::read_csv("DATA/Raw/VariableDescriptions.csv") %>%
  janitor::clean_names() %>%
  dplyr::rename(Variable = variable_name)



# List of variables by level ----------------------------------------------

varlist_studentkey <- varname_df %>%
  dplyr::filter(data_level == "Student") %$%
  Variable
varlist_studentkey
# [1] "mathscalescore"
# [2] "elascalescore"
# [3] "studentkey"
# [4] "testscore_gender"
# [5] "testscore_ethnicity"
# [6] "grade"
# [7] "endyear"
# [8] "testscore_totalunexcuseddays"
# [9] "testscore_totalunexcuseddays_cat"
# [10] "testscore_totaldaysmissed"
# [11] "testscore_instructionday"
# [12] "testscore_gifted"
# [13] "testscore_special_ed"
# [14] "geometry

varlist_cdenumber <- varname_df %>%
  dplyr::filter(data_level == "School") %$%
  Variable
varlist_cdenumber
# [1] "ieq_indoor"
# [2] "ieq_indoor_cat"
# [3] "ieq_thermal"
# [4] "ieq_acoustics"
# [5] "ieq_visual"
# [6] "ieq_visual_cat"
# [7] "cdenumber"
# [8] "school_pct_frl_avg"
# [9] "school_student_enrollment_avg"
# [10] "school_student_enrollment_avg_cat"
# [11] "geometry_school"

varlist_GEOID <- varname_df %>%
  dplyr::filter(data_level == "Census tract") %$%
  Variable
varlist_GEOID
# [1] "ses_edu_highschoolmore"      "ses_edu_bachelormore"
# [3] "ses_married_6to17"           "ses_married_less18"
# [5] "ses_poverty_all"             "ses_poverty_all_cat"
# [7] "ses_poverty_6to17"           "ses_medianhhincome"
# [9] "ses_medianhhincome_log10"    "ses_medianfamincome"
# [11] "ses_medianfamincome_withkid" "ses_unemployed"
# [13] "ses_unemployed_cat"          "ses_crowding"
# [15] "ses_crowding_cat"            "ses_uninsured_all"
# [17] "ses_uninsured_6to18"         "ses_renter_all"
# [19] "ses_renter_all_cat"          "ses_renter_withkid_6to17"


# Split data by level -----------------------------------------------------

analysis_bygrade <- load_analysis %>% dplyr::group_split(grade)

analysis_studentkey <- analysis_bygrade %>%
  purrr::map(
    ~.x %>%
      dplyr::select(studentkey,
                    tidyselect::all_of(varlist_studentkey[!varlist_studentkey=="geometry"])) %>%
      dplyr::distinct(studentkey, .keep_all = TRUE))

analysis_cdenumber <- analysis_bygrade %>%
  purrr::map(
    ~.x %>%
      dplyr::select(cdenumber,
                    tidyselect::all_of(varlist_cdenumber[!varlist_cdenumber=="geometry_school"])) %>%
      dplyr::distinct(cdenumber, .keep_all = TRUE))

analysis_geoid <- analysis_bygrade %>%
  purrr::map(
    ~.x %>%
      dplyr::select(GEOID, tidyselect::starts_with("ses_")) %>%
      dplyr::distinct(GEOID, .keep_all = TRUE))


# * Table a Student -------------------------------------------------------

# construct_desc_table_num <- function(df) {
#   df %>%
#     vtable::st(out = "csv",
#                add.median = TRUE) %>%
#     dplyr::mutate_at(-1, as.numeric) %>%
#     dplyr::left_join(varname_df %>% dplyr::select(Variable, variable_label),
#                      by = "Variable") %>%
#     dplyr::mutate(Variable = variable_label,
#                   Mean = sprintf("%.1f", Mean),
#                   SD = sprintf("%.1f", `Std. Dev.`),
#                   Min = sprintf("%.1f", Min),
#                   Q1 = sprintf("%.1f", `Pctl. 25`),
#                     Median = sprintf("%.1f", `Pctl. 50`),
#                   Q3 = sprintf("%.1f", `Pctl. 75`),
#                   Max = sprintf("%.1f", Max)) %>%
#     dplyr::select(Variable, N, Mean, SD, Min, Q1, Median, Q3, Max)
#   }

format_num <- function(x){scales::label_comma(accuracy = .1)(x)}
format_num_0 <- function(x){scales::label_comma(accuracy = 1)(x)}


construct_desc_table_num <- function(df) {
  df %>%
    # dplyr::select(-grade) %>%
    vtable::st(out = "csv", add.median = TRUE) %>%
    dplyr::mutate_at(-1, as.numeric) %>%
    # dplyr::mutate(`    Mean+_SD` = paste0(sprintf("%.1f", Mean), "\u00b1", sprintf("%.1f", `Std. Dev.`)),
    #               `    Median` = sprintf("%.1f", `Pctl. 50`),
    #               `    Q1-Q3` = paste0(sprintf("%.1f", `Pctl. 25`), "-", sprintf("%.1f", `Pctl. 75`)),
    #               `    Median (IQR)` = paste0( `    Median`, " (", `    Q1-Q3`, ")"),
    #               `    Min-Max` = paste0(sprintf("%.3f", Min), "-", sprintf("%.3f", Max)),
    #               `    Min` = sprintf("%.1f", Min),
    #               `    Max` = sprintf("%.1f", Max),
    #               `    n` = N %>% scales::label_comma(accuracy = 1)(.)
    # ) %>%
    dplyr::mutate(`    Mean+_SD` = paste0(format_num(Mean), "\u00b1", format_num(`Std. Dev.`)),
                  `    Median` = format_num(`Pctl. 50`),
                  `    Q1-Q3` = paste0(format_num(`Pctl. 25`), "-", format_num(`Pctl. 75`)),
                  `    Median (IQR)` = paste0( `    Median`, " (", `    Q1-Q3`, ")"),
                  `    Min-Max` = paste0(format_num(Min), "-", format_num(Max)),
                  `    Min` = Min %>% format_num(),
                  `    Max` = Max %>% format_num(),
                  `    n` = N %>% format_num_0()
    ) %>%
    dplyr::select(Variable,
                  `    n`, `    Mean+_SD`, `    Median (IQR)`, `    Min`, `    Max`) %>%
    tidyr::pivot_longer(everything(),
                        names_to = "stats",
                        values_to = "value") %>%
    dplyr::rename(Variable = stats) %>%
    dplyr::mutate(Variable = ifelse(Variable == "Variable", value, Variable),
                  value = ifelse(Variable == value, "", value))
}

construct_desc_table_num_0 <- function(df) {
  df %>%
    # dplyr::select(-grade) %>%
    vtable::st(out = "csv", add.median = TRUE) %>%
    dplyr::mutate_at(-1, as.numeric) %>%
    # dplyr::mutate(`    Mean+_SD` = paste0(sprintf("%.1f", Mean), "\u00b1", sprintf("%.1f", `Std. Dev.`)),
    #               `    Median` = sprintf("%.1f", `Pctl. 50`),
    #               `    Q1-Q3` = paste0(sprintf("%.1f", `Pctl. 25`), "-", sprintf("%.1f", `Pctl. 75`)),
    #               `    Median (IQR)` = paste0( `    Median`, " (", `    Q1-Q3`, ")"),
    #               `    Min-Max` = paste0(sprintf("%.3f", Min), "-", sprintf("%.3f", Max)),
    #               `    Min` = sprintf("%.1f", Min),
    #               `    Max` = sprintf("%.1f", Max),
    #               `    n` = N %>% scales::label_comma(accuracy = 1)(.)
    # ) %>%
    dplyr::mutate(`    Mean+_SD` = paste0(format_num_0(Mean), "\u00b1", format_num_0(`Std. Dev.`)),
                  `    Median` = format_num_0(`Pctl. 50`),
                  `    Q1-Q3` = paste0(format_num_0(`Pctl. 25`), "-", format_num_0(`Pctl. 75`)),
                  `    Median (IQR)` = paste0( `    Median`, " (", `    Q1-Q3`, ")"),
                  `    Min-Max` = paste0(format_num_0(Min), "-", format_num_0(Max)),
                  `    Min` = Min %>% format_num_0(),
                  `    Max` = Max %>% format_num_0(),
                  `    n` = N %>% format_num_0()
    ) %>%
    dplyr::select(Variable,
                  `    n`, `    Mean+_SD`, `    Median (IQR)`, `    Min`, `    Max`) %>%
    tidyr::pivot_longer(everything(),
                        names_to = "stats",
                        values_to = "value") %>%
    dplyr::rename(Variable = stats) %>%
    dplyr::mutate(Variable = ifelse(Variable == "Variable", value, Variable),
                  value = ifelse(Variable == value, "", value))
}

clean_desc_table_num <- function(list){
  list %>%
    purrr::map2(c("Grade 5 Elementary",
                  "Grade 8 Middle",
                  "Grade 9 High"),
                ~.x %>% dplyr::rename({{.y}} := value)) %>%
    dplyr::bind_cols() %>%
    dplyr::select(-c(3,5)) %>%
    dplyr::rename(Variable = `Variable...1`) %>%
    dplyr::mutate(Variable = Variable %>% dplyr::recode("    Mean+_SD" = "    Mean\u00b1SD"))
}

desc_table_studentkey_cat <- analysis_studentkey %>%
  purrr::map(~.x %>%
               dplyr::select(testscore_gender, testscore_ethnicity,
                             testscore_gifted, testscore_special_ed) %>%
               vtable::st(out = "csv") %>%
               dplyr::mutate(value = paste0(N %>%
                                                as.numeric() %>%
                                                scales::label_comma(accuracy = 1)(.),
                                              " (", Percent, ")")) %>%
               dplyr::mutate(value = ifelse(Percent == "", "", value)) %>%
               dplyr::mutate(Variable = Variable %>% stringr::str_replace("... ", "    ")) %>%
               dplyr::select(Variable, value)
             ) %>%
  clean_desc_table_num()

desc_table_studentkey_num <- analysis_studentkey %>%
  purrr::map(~.x %>%
               dplyr::select(testscore_totalunexcuseddays, testscore_totaldaysmissed,
                             testscore_instructionday,
                             mathscalescore, elascalescore)) %>%
  purrr::map(construct_desc_table_num) %>%
  clean_desc_table_num()


# * Table b School --------------------------------------------------------

desc_table_cdenumber <- analysis_cdenumber %>%
  purrr::map(~.x %>% dplyr::select(ieq_indoor, ieq_thermal, ieq_acoustics,
                                   ieq_visual, school_pct_frl_avg )) %>%
  purrr::map(construct_desc_table_num) %>%
  clean_desc_table_num()

desc_table_cdenumber_0 <- analysis_cdenumber %>%
  purrr::map(~.x %>% dplyr::select(school_student_enrollment_avg)) %>%
  purrr::map(construct_desc_table_num_0) %>%
  clean_desc_table_num()


# * Table c Census tract --------------------------------------------------

desc_table_geoid_0 <- analysis_geoid %>%
  purrr::map(~.x %>%
               dplyr::select(-GEOID) %>%
               dplyr::select(ses_medianhhincome)) %>%
  purrr::map(construct_desc_table_num_0) %>%
  clean_desc_table_num()

desc_table_geoid <- analysis_geoid %>%
  purrr::map(~.x %>%
               dplyr::select(-GEOID) %>%
               dplyr::select(ses_medianhhincome_log10,
                             ses_edu_highschoolmore, ses_married_6to17,
                             ses_uninsured_6to18)) %>%
  purrr::map(construct_desc_table_num) %>%
  clean_desc_table_num()



# Combine Tables a b c  ---------------------------------------------------

desc_table <- dplyr::bind_rows(
  tibble::tibble(Variable = "",
                 `Grade 5 Elementary` = "n (%)",
                 `Grade 8 Middle` = "n (%)",
                 `Grade 9 High` = "n (%)"),
  # tibble::tibble(Variable = "Student Characteristics (Student-Level)",
  tibble::tibble(Variable = "STUDENT CHARACTERISTICS (STUDENT-LEVEL)",
                 `Grade 5 Elementary` = "",
                 `Grade 8 Middle` = "",
                 `Grade 9 High` = ""),
  desc_table_studentkey_cat,
  # tibble::tibble(Variable = "",
  #                `Grade 5 Elementary` = "statistics",
  #                `Grade 8 Middle` = "statistics",
  #                `Grade 9 High` = "statistics"),
  desc_table_studentkey_num,
  # tibble::tibble(Variable = "School Characteristics (School-Level)",
  tibble::tibble(Variable = "SCHOOL CHARACTERISTICS (SCHOOL-LEVEL)",
                 `Grade 5 Elementary` = "",
                 `Grade 8 Middle` = "",
                 `Grade 9 High` = ""),
  desc_table_cdenumber,
  desc_table_cdenumber_0,
  # tibble::tibble(Variable = "Socioeconomic Characteristics (Census-Tract Level)",
  tibble::tibble(Variable = "SOCIOECONOMIC CHARACTERISTICS (CENSUS-TRACT LEVEL)",
                 `Grade 5 Elementary` = "",
                 `Grade 8 Middle` = "",
                 `Grade 9 High` = ""),
  desc_table_geoid_0,
  desc_table_geoid) %>%
  # Get the variable label
  dplyr::left_join(varname_df %>% dplyr::select(Variable, variable_label),
                   by = "Variable") %>%
  dplyr::mutate(variable_label = ifelse(is.na(variable_label), Variable, variable_label)) %>%
  dplyr::select(variable_label,`Grade 5 Elementary`, `Grade 8 Middle`, `Grade 9 High`) %>%
  dplyr::rename(Variable = variable_label)


save_data(desc_table,
          "outputs/tables/Aim1/desc_table",
          "outputs/tables/Aim1/Archived/desc_table",
          xlsx = TRUE)



emptyrow_index <- desc_table$`Grade 5 Elementary` %>%
  {which(. == "")} %>%
  purrr::map_dbl(~.x + 1)

section_index <- emptyrow_index %>%
  base::diff(lag = 1, differences = 1) %>%
  {which(. == 1)} %>%
  emptyrow_index[.]

# Open an xlsx workbook
wb <- openxlsx::buildWorkbook(desc_table)
# Merge columns by emptyrow_index
for (i in emptyrow_index) {
  openxlsx::mergeCells(wb, sheet = "Sheet 1", cols = 1:4, rows = i)
}
# Merge cells for Variable
openxlsx::mergeCells(wb, sheet = "Sheet 1", cols = 1, rows = 1:2)

# Column widths
openxlsx::setColWidths(wb, "Sheet 1", cols = 1, widths = 26)
openxlsx::setColWidths(wb, "Sheet 1", cols = 2, widths = 20)
openxlsx::setColWidths(wb, "Sheet 1", cols = 3, widths = 18)
openxlsx::setColWidths(wb, "Sheet 1", cols = 4, widths = 22)

# Edit Style for row 1, column 1:4
openxlsx::addStyle(wb, sheet = "Sheet 1",
                   rows = 1, cols = 1:4,
                   gridExpand = TRUE,
                   style = openxlsx::createStyle(textDecoration = "bold",
                                                 halign = "center",
                                                 valign = "center",
                                                 border = "top",
                                                 borderStyle = "thick"))
# Edit Style for row 2, column 1:4
openxlsx::addStyle(wb, sheet = "Sheet 1",
                   rows = 2, cols = 1:4,
                   gridExpand = TRUE,
                   style = openxlsx::createStyle(halign = "center",
                                                 valign = "center",
                                                 border = "top",
                                                 borderStyle = "thin"))
# Edit Style for row 3, column 1:4
openxlsx::addStyle(wb, sheet = "Sheet 1",
                   rows = 3, cols = 1:4,
                   gridExpand = TRUE,
                   style = openxlsx::createStyle(halign = "left",
                                                 valign = "center",
                                                 border = "top",
                                                 borderStyle = "thin"))
# Edit Style for statistics
openxlsx::addStyle(wb, sheet = "Sheet 1",
                   rows = 4:(nrow(desc_table) + 1), cols = 2:4,
                   gridExpand = TRUE,
                   style = openxlsx::createStyle(halign = "center",
                                                 valign = "center"))
# Add thin borders for section
for (i in section_index) {openxlsx::addStyle(wb, sheet = "Sheet 1",
                                             rows = i, cols = 1:4,
                                             gridExpand = TRUE,
                                             style = openxlsx::createStyle(textDecoration = "bold",
                                                                           border = "top",
                                                                           borderStyle = "thin"))}
# Edit Style for last row, column 1
openxlsx::addStyle(wb, sheet = "Sheet 1",
                   rows = nrow(desc_table) + 1, cols = 1,
                   gridExpand = TRUE,
                   style = openxlsx::createStyle(border = "bottom",
                                                 borderStyle = "thick"))
# Edit Style for last row, column 2:4
openxlsx::addStyle(wb, sheet = "Sheet 1",
                   rows = nrow(desc_table) + 1, cols = 2:4,
                   gridExpand = TRUE,
                   style = openxlsx::createStyle(halign = "center",
                                                 border = "bottom",
                                                 borderStyle = "thick"))
# save workbook
openxlsx::saveWorkbook(wb, "outputs/tables/Aim1/desc_table_formatted.xlsx", TRUE)

# Intext Stats ------------------------------------------------------------

load_analysis <- readr::read_rds("DATA/Processed/Aim1/aim1_analysis.rds")
ses <- readr::read_rds("DATA/Processed/Aim1/aim1_ses.rds")



# No students 37295
load_analysis$studentkey %>% unique() %>% length()
# No schools 47
load_analysis$cdenumber %>% unique() %>% length()
# No GEOID/census tracts 235
load_analysis$GEOID %>% unique() %>% length()



analysis_school <- load_analysis %>%
  dplyr::select(cdenumber, school_pct_frl_avg, school_student_enrollment_avg,
                ieq_thermal, ieq_acoustics, ieq_visual, ieq_indoor) %>%
  dplyr::distinct(cdenumber, .keep_all = TRUE)

desc_text_school <- analysis_school %>%
  vtable::st(out = "csv",
             file = "outputs/tables/Aim1/desc_text_school.csv")

analysis_ct <- load_analysis %>%
  dplyr::select(GEOID, tidyselect::starts_with("ses_")) %>%
  dplyr::distinct(GEOID, .keep_all = TRUE)

desc_text_ct <- analysis_ct %>%
  dplyr::select(-GEOID) %>%
  vtable::st(out = "csv")

# Test code ---------------------------------------------------------------



data.ieq <- readr::read_rds("DATA/Processed/Aim1/final_ieq_20210928.rds")
data.student <- readr::read_rds("DATA/Processed/Aim1/final_student_aim1_20210928.rds")
##
data <- dplyr::left_join(data.student, data.ieq, by = 'cdenumber')
##
## Number of students by grade
data %>% dplyr::filter(!is.na(ELAScaleScore)) %>% dplyr::count(Grade)
data %>% dplyr::filter(!is.na(mathScaleScore)) %>% dplyr::count(Grade)
## Number of school by grade
data %>% dplyr::group_by(Grade) %>% dplyr::summarise(n = dplyr::n_distinct(cdenumber))
## Descriptive Stats
## Using package `tableone`
## List numerically coded categorical variables
factorVars <- c("gender", "gifted_talented", "iep", "ethnic_code")
## Create a variable list. Use dput(names(data))
## vars <- c(dput(names(data)))
vars <- c("energy", "thermal", "acoustics", "visual", "indoor",
          "gender", "ethnic_code",
          ## "gifted_talented", "iep",
          "ELAScaleScore", "mathScaleScore")
##
#Then the TableOne object can be created, and examined as follows.
## Create Table 1 stratified by trt (omit strata argument for overall table)
test <- tableone::CreateTableOne(vars = vars, strata = "Grade", data = data, factorVars = factorVars)
## Just typing the object name will invoke the print.TableOne method
## Tests are by oneway.test/t.test for continuous, chisq.test for categorical
test
