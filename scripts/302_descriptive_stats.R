# Clean the environment ---------------------------------------------------
rm(list = ls())

# Functions ---------------------------------------------------------------

source("scripts/Functions/save_data.R")
source("scripts/Functions/create_folder.R")


# Create folders ----------------------------------------------------------


# Load Data ---------------------------------------------------------------
load_analysis <- readr::read_rds("DATA/Processed/Aim3/aim3_analysis.rds") %>%
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
varlist_studentkey <- c("mathscalescore",
                        "elascalescore",
                        "testscore_gender",
                        "testscore_ethnicity",
                        "testscore_gifted",
                        "testscore_special_ed",
                        "ggs_ndvi_landsat_0250",
                        "ggs_ndvi_modis_0250",
                        "ggs_ndvi_nlcd_0250")

varlist_cdenumber <- varname_df %>%
  dplyr::filter(data_level == "School") %$%
  Variable
varlist_cdenumber
varlist_cdenumber <- c("school_pct_frl_avg",
                       "school_student_enrollment_avg",
                       "sgs_ndvi_landsat_0250",
                       "sgs_ndvi_modis_0250",
                       "sgs_ndvi_nlcd_0250")

varlist_GEOID <- varname_df %>%
  dplyr::filter(data_level == "Census tract") %$%
  Variable
varlist_GEOID
varlist_GEOID <- c("r_SE_nat",
                   "ruca",
                   "ses_medianhhincome",
                   "ses_medianhhincome_log10",
                   "ses_edu_highschoolmore",
                   "ses_married_6to17",
                   "ses_uninsured_6to18")


# Split data by level -----------------------------------------------------

analysis_bygrade <- load_analysis %>%
  dplyr::group_split(grade) %>%
  `names<-`(c("50", "80", '90'))

analysis_studentkey <- analysis_bygrade %>%
  purrr::map(
    ~.x %>%
      dplyr::select(studentkey,
                    tidyselect::all_of(varlist_studentkey)) %>%
      dplyr::distinct(studentkey, .keep_all = TRUE))

analysis_cdenumber <- analysis_bygrade %>%
  purrr::map(
    ~.x %>%
      dplyr::select(cdenumber,
                    tidyselect::all_of(varlist_cdenumber)) %>%
      dplyr::distinct(cdenumber, .keep_all = TRUE))

analysis_geoid <- analysis_bygrade %>%
  purrr::map(
    ~.x %>%
      dplyr::select(GEOID,
                    tidyselect::all_of(varlist_GEOID)) %>%
      dplyr::distinct(GEOID, .keep_all = TRUE))


# * Table a Student -------------------------------------------------------



construct_desc_table_num <- function(df, digitf) {
  format_num <- function(x){scales::label_comma(accuracy = digitf %>% as.numeric())(x)}
  df %>%
    vtable::st(out = "csv", add.median = TRUE) %>%
    dplyr::mutate_at(-1, as.numeric) %>%
    dplyr::mutate(`    Mean+_SD` = paste0(format_num(Mean), "\u00b1", format_num(`Std. Dev.`)),
                  `    Median` = format_num(`Pctl. 50`),
                  `    Q1-Q3` = paste0(format_num(`Pctl. 25`), "-", format_num(`Pctl. 75`)),
                  `    Median (IQR)` = paste0( `    Median`, " (", `    Q1-Q3`, ")"),
                  `    Min-Max` = paste0(format_num(Min), "-", format_num(Max)),
                  `    Min` = Min %>% format_num(),
                  `    Max` = Max %>% format_num(),
                  `    n` = scales::label_comma(accuracy = 1)(N)
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

desc_table_studentkey_num_1 <- analysis_studentkey %>%
  purrr::map(~.x %>%
               dplyr::select(mathscalescore, elascalescore)) %>%
  purrr::map(~construct_desc_table_num(.x, digitf = ".1")) %>%
  clean_desc_table_num()

desc_table_studentkey_num_2 <- analysis_studentkey %>%
  purrr::map(~.x %>%
               dplyr::select(ggs_ndvi_landsat_0250,
                             ggs_ndvi_modis_0250,
                             ggs_ndvi_nlcd_0250)) %>%
  purrr::map(~construct_desc_table_num(.x, digitf = ".001")) %>%
  clean_desc_table_num()


# * Table b School --------------------------------------------------------

desc_table_cdenumber_1 <- analysis_cdenumber %>%
  purrr::map(~.x %>% dplyr::select(sgs_ndvi_landsat_0250,
                                   sgs_ndvi_modis_0250,
                                   sgs_ndvi_nlcd_0250)) %>%
  purrr::map(~construct_desc_table_num(.x, digitf = ".001")) %>%
  clean_desc_table_num()

desc_table_cdenumber_2 <- analysis_cdenumber %>%
  purrr::map(~.x %>% dplyr::select(school_pct_frl_avg )) %>%
  purrr::map(~construct_desc_table_num(.x, digitf = ".1")) %>%
  clean_desc_table_num()

desc_table_cdenumber_3 <- analysis_cdenumber %>%
  purrr::map(~.x %>% dplyr::select(school_student_enrollment_avg)) %>%
  purrr::map(~construct_desc_table_num(.x, digitf = "1")) %>%
  clean_desc_table_num()


# * Table c Census tract --------------------------------------------------

desc_table_geoid_1 <- analysis_geoid %>%
  purrr::map(~.x %>%
               dplyr::select(-GEOID) %>%
               dplyr::select(ses_medianhhincome)) %>%
  purrr::map(~construct_desc_table_num(.x, digitf = "1")) %>%
  clean_desc_table_num()

desc_table_geoid_2 <- analysis_geoid %>%
  purrr::map(~.x %>%
               dplyr::select(-GEOID) %>%
               dplyr::select(ses_medianhhincome_log10,
                             ses_edu_highschoolmore, ses_married_6to17,
                             ses_uninsured_6to18,
                             r_SE_nat,
                             ruca)) %>%
  purrr::map(~construct_desc_table_num(.x, digitf = ".1")) %>%
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
  desc_table_studentkey_num_1,
  desc_table_studentkey_num_2,
  # tibble::tibble(Variable = "School Characteristics (School-Level)",
  tibble::tibble(Variable = "SCHOOL CHARACTERISTICS (SCHOOL-LEVEL)",
                 `Grade 5 Elementary` = "",
                 `Grade 8 Middle` = "",
                 `Grade 9 High` = ""),
  desc_table_cdenumber_1,
  desc_table_cdenumber_2,
  desc_table_cdenumber_3,
  # tibble::tibble(Variable = "Socioeconomic Characteristics (Census-Tract Level)",
  tibble::tibble(Variable = "SOCIOECONOMIC CHARACTERISTICS (CENSUS-TRACT LEVEL)",
                 `Grade 5 Elementary` = "",
                 `Grade 8 Middle` = "",
                 `Grade 9 High` = ""),
  desc_table_geoid_1,
  desc_table_geoid_2) %>%
  # Get the variable label
  dplyr::left_join(varname_df %>% dplyr::select(Variable, variable_label),
                   by = "Variable") %>%
  dplyr::mutate(variable_label = ifelse(is.na(variable_label), Variable, variable_label)) %>%
  dplyr::select(variable_label,`Grade 5 Elementary`, `Grade 8 Middle`, `Grade 9 High`) %>%
  dplyr::rename(Variable = variable_label)


save_data(desc_table,
          "outputs/tables/Aim3/desc_table",
          "outputs/tables/Aim3/Archived/desc_table",
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
openxlsx::saveWorkbook(wb, "outputs/tables/Aim3/desc_table_formatted.xlsx", TRUE)


# Intext Stats ------------------------------------------------------------

load_analysis <- readr::read_rds("DATA/Processed/Aim3/aim3_analysis.rds")

# No students 37295
load_analysis$studentkey %>% unique() %>% length()
# No schools 47
load_analysis$cdenumber %>% unique() %>% length()
# No GEOID/census tracts 235
load_analysis$GEOID %>% unique() %>% length()


intext_student <- load_analysis %>% dplyr::distinct(studentkey, .keep_all = TRUE) %>%
  dplyr::mutate(testscore_ethnicity = testscore_ethnicity %>% as.factor()) %>%
  dplyr::select(testscore_gender, testscore_ethnicity, testscore_gifted, testscore_special_ed,
                testscore_instructionday, testscore_totaldaysmissed, testscore_totalunexcuseddays,
                mathscalescore, elascalescore,
                ggs_ndvi_landsat_0250, ggs_ndvi_modis_0250, ggs_ndvi_nlcd_0250) %>%
  vtable::st(out = "csv")

intext_school <- load_analysis %>% dplyr::distinct(cdenumber, .keep_all = TRUE) %>%
  dplyr::select(school_student_enrollment_avg, school_pct_frl_avg,
                sgs_ndvi_landsat_0250, sgs_ndvi_modis_0250, sgs_ndvi_nlcd_0250)%>%
  vtable::st(out = "csv")

intext_geoid <- load_analysis %>%
  dplyr::distinct(GEOID, .keep_all = TRUE) %>%
  dplyr::select(ses_medianhhincome, ses_edu_highschoolmore, ses_unemployed,
                ses_renter_all, ses_crowding, ses_uninsured_6to18) %>%
  vtable::st(out = "csv")


