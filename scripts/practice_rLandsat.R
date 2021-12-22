# https://github.com/atlanhq/rLandsat



# * Load packages ---------------------------------------------------------

# remotes::install_github("atlanhq/rLandsat")

packages <- c("tidyverse", "magrittr", "tigris", "sf", "rLandsat")

package_check <- lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x, dependencies = TRUE)
  library(x, character.only = TRUE)
})





# -------------------------------------------------------------------------


# get all the product IDs for India, alternatively can define path and row
# result = landsat_search(min_date = "2018-01-01", max_date = "2018-01-16", country = "India")
result <- landsat_search(min_date = "2018-01-01", max_date = "2018-01-16", country = "United States of America")



# inputting espa creds
espa_creds("hanhdungdao", "ZH35tWD.TzMp_8")

# getting available products
prods <-  espa_products(result$product_id)
prods <-  prods$master

# placing an espa order
result_order <- espa_order(result$product_id, product = c("sr"),
                          projection = "lonlat",
                          order_note = "All India Jan 2018")
order_id <- result_order$order_details$orderid

# getting order status
durl <- espa_status(order_id = order_id, getSize = TRUE)
downurl <- durl$order_details

# download; after the order is complete
landsat_download(download_url = downurl$product_dload_url, dest_file = getwd())