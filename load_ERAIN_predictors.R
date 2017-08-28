library(loadeR.ECOMS)
library(transformeR)

lon_iberia <- c(-10, 5)
lat_iberia <- c(35, 45)

lon_germany <- c(2, 16)
lat_germany <- c(47, 58)

source("~/workspace/udg_credentials.R")
loginUDG(username, password)

era.dataset <- "http://meteo.unican.es/tds5/dodsC/interim/daily/interim20_daily.ncml"

## PREDICTORS FOR IBERIA -------------------------------------------------------

t850.iberia <- loadGridData(dataset = era.dataset,
                            var = "T850",
                            lonLim = lon_iberia,
                            latLim = lat_iberia,
                            years = 1979:2008,
                            dictionary = FALSE)
q850.iberia <- loadGridData(dataset = era.dataset,
                            var = "Q850",
                            lonLim = lon_iberia,
                            latLim = lat_iberia,
                            years = 1979:2008,
                            dictionary = FALSE)
z850.iberia <- loadGridData(dataset = era.dataset,
                            var = "Z850",
                            lonLim = lon_iberia,
                            latLim = lat_iberia,
                            years = 1979:2008,
                            dictionary = FALSE)

# save(list = ls(pattern = "\\.iberia$"),
#      file = "gh/TFM_Rscripts/data/era_interim/predictors_iberia.Rdata",
#      compress = "xz")

## PREDICTORS FOR GERMANY ------------------------------------------------------

t850.germany <- loadGridData(dataset = era.dataset,
                            var = "T850",
                            lonLim = lon_germany,
                            latLim = lat_germany,
                            years = 1979:2008,
                            dictionary = FALSE)
q850.germany <- loadGridData(dataset = era.dataset,
                            var = "Q850",
                            lonLim = lon_germany,
                            latLim = lat_germany,
                            years = 1979:2008,
                            dictionary = FALSE)
z850.germany <- loadGridData(dataset = era.dataset,
                            var = "Z850",
                            lonLim = lon_germany,
                            latLim = lat_germany,
                            years = 1979:2008,
                            dictionary = FALSE)

# save(list = ls(pattern = "\\.germany$"),
#      file = "gh/TFM_Rscripts/data/era_interim/predictors_germany.Rdata",
#      compress = "xz")



