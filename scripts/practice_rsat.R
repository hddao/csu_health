
# 2. Download -------------------------------------------------------------
# https://docs.ropensci.org/rsat/articles/rsat2_download.html


# Download
# Downloading implies acquiring and saving the list of satellite images in a records on your machine. This demo builds on the showcase from the search vignette and so, the first section reviews the most important code from the previous vignette. The second section explains how to obtain satellite images with rsat. The last section mentions how rtois are designed to favor collaborative efforts to save downloading time.


# * Review ----------------------------------------------------------------

# As a first step of rsat’s workflow is specifying the credentials for the the web services:
rsat::set_credentials("rsat.package","UpnaSSG.2021")

# The showcase aims at assessing the effect of the [Snowstorm Filomena](https://en.wikipedia.org/wiki/2020%E2%80%9321_European_windstorm_season#Storm_Filomena) on the Iberian peninsula during January \(10^{th}\) and \(15^{th}\), \(2021\). Hence, the roi and toi correspond to an sf polygon around the peninsula (ip) and a vector of dates (toi) covering the time-span:
ip <- sf::st_sf(sf::st_as_sfc(sf::st_bbox(c(
  xmin = -9.755859,
  xmax =  4.746094,
  ymin = 35.91557,
  ymax = 44.02201 
), crs = 4326)))
toi <- seq(as.Date("2021-01-10"),as.Date("2021-01-15"),1)




# The folders for the database and dataset can be created programmatically as follows:
db.path <- "DATA/Raw/Aim2/database"
ds.path <- "DATA/Raw/Aim2/datasets"
dir.create(db.path)
dir.create(ds.path)

# The minimum information to generate a new rtoi is the name, a polygon of the roi, and the paths to database and dataset:
filomena <- rsat::new_rtoi(name = "filomena",
                           region = ip,
                           db_path = db.path,
                           rtoi_path = ds.path
)
# To limit the amount of data and processing times, the assessment is conducted over MODIS imagery. A total number of \(24\) images are found for the region over the \(6\)-day period:
rsat::rsat_search(region = filomena, product = c("mod09ga"), dates = toi)


# * Image acquisition -----------------------------------------------------

# Downloading is straightforward with the function `rsat_download()`. The simplest way to use this function is passing the rtoi as an input. Depending on the speed of the internet connection, the following instruction may take from few to several minutes to run:
rsat::rsat_download(filomena)

# The function saves the satellite images automatically in the database. The path to the database is provided by the rtoi:
list.files(rsat::get_database(filomena), recursive = TRUE)

# Another way to download images is using a records. This variant requires defining a path for saving the resulting files (out.dir). The next line is equivalent to the rtoi version but using its records class object:
rsat::rsat_download(rsat::records(filomena), out.dir = rsat::get_database(filomena))

# This second time, the message reveals that the function reads the database first and checks which images in the rtoi are already available in the destination path. If it is available, the function skips its download. This feature becomes handy when teams share a common database.


# * Collaborative rtois ---------------------------------------------------

# The rtoi leverages the collective use of the package by different working groups within the same or different institutions. Teams working on separate studies or rtois can refer to the same database increasing its size over time. The database can progressively turn into a local repository. Eventually, new rtois may find the requested information in the local database, skipping their download, and saving processing time. We encourage rsat users to develop common databases when possible on shared machines.

# The following vignette explains how to customize the satellite images to turn raw data into valuable information for the aim of the analysis.




# rsat: MODIS -------------------------------------------------------------

set_credentials("rsat.package","UpnaSSG.2021", "scihub")
set_credentials("rsat.package","UpnaSSG.2021", "earthdata")



ip <- bbox_sf
toi <- seq(as.Date("2015-01-01"), as.Date("2019-12-31"), 1)

# The folders for the database and dataset can be created programmatically as follows:
db.path <- "DATA/Raw/Aim2/MODIS/database"
ds.path <- "DATA/Raw/Aim2/MODIS/datasets"
dir.create(db.path)
dir.create(ds.path)


# The minimum information to generate a new rtoi is the name, a polygon of the roi, and the paths to database and dataset:
adams <- rsat::new_rtoi(name = "adams",
                        region = ip,
                        db_path = db.path,
                        rtoi_path = ds.path
)

# To limit the amount of data and processing times, the assessment is conducted over MODIS imagery. A total number of \(24\) images are found for the region over the \(6\)-day period:
rsat::rsat_search(region = adams, product = c("LANDSAT_8_C1"), dates = toi)