# Clean the environment ---------------------------------------------------
rm(list = ls())

# Functions ---------------------------------------------------------------

source("scripts/Functions/save_data.R")
source("scripts/Functions/create_folder.R")


# Create folders ----------------------------------------------------------


# Load Data ---------------------------------------------------------------
analysis <- readr::read_rds("DATA/Processed/Aim3/aim3_analysis.rds") %>%
  sf::st_as_sf()

analysis_bygrade <- analysis %>%
  dplyr::mutate(grade = grade %>% as.character()) %>%
  dplyr::filter(grade %in% c("50", "80", "90")) %>%
  dplyr::group_split(grade)


# Create spatial weight matrix --------------------------------------------


# https://r-spatial.github.io/spdep/articles/nb.html
# https://r-spatial.github.io/spdep/articles/nb_sf.html

# https://rpubs.com/natalie_coleman/278910

# analysis
# 1069965-926488.5
# 430014.8-298991.1


# test <- analysis_bygrade[[3]] %>% dplyr::distinct(geometry)
test <- analysis_bygrade[[1]]

# Lagged scatterplots
# https://rpubs.com/natalie_coleman/278910

gstat::hscat(elascalescore ~ 1,
             data = test %>% dplyr::filter(!is.na(elascalescore)), breaks = seq(0, 150000 , by = 10000))

# seq(0, 150000 , by = 10000)
# The lagged scatterplots show that there is strong spatial autocorrelation at distances shorter than tk meters at which point the trend switches to very weakly negative autocorrelation. This correlated to tk





# VARIOBRAM
# https://rpubs.com/natalie_coleman/278910
# variogramcloud <- gstat::variogram(elascalescore ~ 1, data = test %>% dplyr::filter(!is.na(elascalescore)),
#                                    cloud = TRUE)
# plot(variogramcloud,pch=20,cex=1.5,col="black",
#      ylab=expression("Semivariance ("*gamma*")"),
#      xlab="Distance (m)",main = "Test Scores")



create_variogram <- function(outcome, df, grade, width) {
  variogramcloud <- outcome %>%
    sprintf("%s ~ 1", .) %>%
    as.formula %>%
    gstat::variogram(. , data = df,
                     width = width,
                     cloud = FALSE)
  ggplot2::ggplot(variogramcloud, ggplot2::aes(x=dist,y=gamma)) +
    ggplot2::geom_point() +
    ggplot2::xlab("Distance (m)") +
    ggplot2::ylab(expression("Semivariance ("*gamma*")")) +
    ggplot2::ggtitle(outcome) +
    ggplot2::theme_minimal()

  ggplot2::ggsave(paste0("outputs/figures/Aim3/variogram_",
                         outcome, "_",
                         grade, "_",
                         width %>% stringr::str_pad(pad = 0, 3),
                         ".jpeg"),
                  device = "jpeg",
                  width = 5,
                  height = 3,
                  units = "in")
  # jpeg(file = paste0("outputs/figures/Aim3/variogram_", outcome, "_", grade, ".jpeg"))
  # plot(variogramcloud,pch=20,cex=1.5,col="black",
  #      ylab=expression("Semivariance ("*gamma*")"),
  #      xlab="Distance (m)",main = "Test Scores")
  # dev.off()
}

create_variogram("mathscalescore",
                 test %>% dplyr::filter(!is.na(mathscalescore)),
                 "5",
                 10)


map_df <- tidyr::crossing(grade = c("50", "80", "90"),
                          outcome = c("mathscalescore", "elascalescore"),
                          width = c(1, 2, 5, 10))
df <- map_df %>%
  purrr::pmap(function (grade, outcome, width) {
    analysis %>%
      dplyr::filter({{grade}} == grade) %>%
      dplyr::filter(!is.na(.data[[{{outcome}}]]))})

map_df <- map_df %>%
  dplyr::mutate(df = df) %>% 
  dplyr::select(outcome, df, grade, width)

rm(df)

purrr::pwalk(map_df, create_variogram)




variogramcloud <- gstat::variogram(mathscalescore ~ 1, data = test %>% dplyr::filter(!is.na(mathscalescore)),
                                   width = 1,
                                   cloud = FALSE)
plot(variogramcloud,pch=20,cex=1.5,col="black",
     ylab=expression("Semivariance ("*gamma*")"),
     xlab="Distance (m)",main = "Test Scores")

# Grade 9: The variograms show that the sill appears at roughly 12,500 m where the semi variance starts to scatter.



# https://gsp.humboldt.edu/olm/R/04_01_Variograms.html
variogram <- gstat::variogram(elascalescore ~ 1, data = test %>% dplyr::filter(!is.na(elascalescore)))
plot(variogram)






spm_knn4 <- spdep::knearneigh(test, k = 4)
nb_knn4 <- spm_knn4 %>% spdep::knn2nb()





# Morans' I ---------------------------------------------------------------


