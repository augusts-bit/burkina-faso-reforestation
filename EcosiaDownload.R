# set wd
setwd("~/Studie/MGI/Advanced Earth Observation/Paper")

# pkgTest is a helper function to load packages and install packages only when they are not installed yet.
pkgTest <- function(x)
{
  if (x %in% rownames(installed.packages()) == FALSE) {
    install.packages(x, dependencies= TRUE)
  }
  library(x, character.only = TRUE)
}
neededPackages <- c("zoo", "bfast", "terra", "raster", "leaflet", "MODISTools")
for (package in neededPackages){pkgTest(package)}

# Utility function to create time series object from a numeric vector
# val_array: data array for one single pixel (length is number of time steps)
# time_array: array with Dates at which raster data is recorded (same length as val_array)
timeser <- function(val_array, time_array) {
  z <- zoo(val_array, time_array) # create zoo object
  yr <- as.numeric(format(time(z), "%Y")) # extract the year numbers
  jul <- as.numeric(format(time(z), "%j")) # extract the day numbers (1-365)
  delta <- min(unlist(tapply(jul, yr, diff))) # calculate minimum time difference (days) between observations
  zz <- aggregate(z, yr + (jul - 1) / delta / 23) # aggregate into decimal year timestamps
  (tso <- as.ts(zz)) # convert into timeseries object
  return(tso)
}

## Lilengo
# Downloading the NDVI data, from 2001-01-01 to 2022-01-01
Lilengo_NDVI <- mt_subset(product = "MOD13Q1",
                 lat = 14.494986, # coordinates of Lilengo 
                 lon = -0.108897,
                 band = "250m_16_days_NDVI",
                 start = "2001-01-01",
                 end = "2022-01-01",
                 km_lr = 2,
                 km_ab = 2,
                 progress = TRUE)

# Downloading the pixel reliability data, from 2001-01-01 to 2022-01-01
Lilengo_QA <- mt_subset(product = "MOD13Q1",
                lat = 14.494986, # coordinates of Lilengo 
                lon = -0.108897,
                band = "250m_16_days_pixel_reliability",
                start = "2001-01-01",
                end = "2022-01-01",
                km_lr = 2,
                km_ab = 2,
                progress = TRUE)

## Béléhédé
# Downloading the NDVI data, from 2001-01-01 to 2022-01-01
Belehede_NDVI <- mt_subset(product = "MOD13Q1",
                          lat = 14.084705, # coordinates of Béléhédé 
                          lon = -1.291897,
                          band = "250m_16_days_NDVI",
                          start = "2001-01-01",
                          end = "2022-01-01",
                          km_lr = 2,
                          km_ab = 2,
                          progress = TRUE)

# Downloading the pixel reliability data, from 2001-01-01 to 2022-01-01
Belehede_QA <- mt_subset(product = "MOD13Q1",
                        lat = 14.084705, # coordinates of Béléhédé 
                        lon = -1.291897,
                        band = "250m_16_days_pixel_reliability",
                        start = "2001-01-01",
                        end = "2022-01-01",
                        km_lr = 2,
                        km_ab = 2,
                        progress = TRUE)

## Aribinda
# Downloading the NDVI data, from 2001-01-01 to 2022-01-01
Aribinda_NDVI <- mt_subset(product = "MOD13Q1",
                           lat = 14.226179, # coordinates of Aribinda
                           lon = -0.862255,
                           band = "250m_16_days_NDVI",
                           start = "2001-01-01",
                           end = "2022-01-01",
                           km_lr = 2,
                           km_ab = 2,
                           progress = TRUE)

# Downloading the pixel reliability data, from 2001-01-01 to 2022-01-01
Aribinda_QA <- mt_subset(product = "MOD13Q1",
                         lat = 14.226179, # coordinates of Aribinda
                         lon = -0.862255,
                         band = "250m_16_days_pixel_reliability",
                         start = "2001-01-01",
                         end = "2022-01-01",
                         km_lr = 2,
                         km_ab = 2,
                         progress = TRUE)

# convert df to raster
## Lilengo
Lilengo_NDVI_r <- rast(mt_to_raster(df = Lilengo_NDVI))
Lilengo_QA_r <- rast(mt_to_raster(df = Lilengo_QA))

## Béléhédé
Belehede_NDVI_r <- rast(mt_to_raster(df = Belehede_NDVI))
Belehede_QA_r <- rast(mt_to_raster(df = Belehede_QA))

## Aribinda
Aribinda_NDVI_r <- rast(mt_to_raster(df = Aribinda_NDVI))
Aribinda_QA_r <- rast(mt_to_raster(df = Aribinda_QA))

# clean the data
# create mask on pixel reliability flag set all values <0 or >1 NA
## Lilengo
Lilengo_m <- Lilengo_QA_r
Lilengo_m[(Lilengo_QA_r < 0 | Lilengo_QA_r > 1)] <- NA # continue working with QA 0 (good data), and 1 (marginal data)

## Béléhédé
Belehede_m <- Belehede_QA_r
Belehede_m[(Belehede_QA_r < 0 | Belehede_QA_r > 1)] <- NA # continue working with QA 0 (good data), and 1 (marginal data)

## Aribinda
Aribinda_m <- Aribinda_QA_r
Aribinda_m[(Aribinda_QA_r < 0 | Aribinda_QA_r > 1)] <- NA # continue working with QA 0 (good data), and 1 (marginal data)

# apply the mask to the NDVI raster
## Lilengo
Lilengo_NDVI_m <- mask(Lilengo_NDVI_r, Lilengo_m, maskvalue=NA, updatevalue=NA)

## Béléhédé
Belehede_NDVI_m <- mask(Belehede_NDVI_r, Belehede_m, maskvalue=NA, updatevalue=NA)

## Aribinda
Aribinda_NDVI_m <- mask(Aribinda_NDVI_r, Aribinda_m, maskvalue=NA, updatevalue=NA)

# write rasters to save
## Lilengo
writeRaster(Lilengo_NDVI_m,'Data/Ecosia/Lilengo/Lilengo.tiff')
writeRaster(Lilengo_NDVI_m,'Data/Ecosia/Lilengo/Lilengo2.tif') # backup

## Béléhédé
writeRaster(Belehede_NDVI_m,'Data/Ecosia/Béléhédé/Belehede.tiff')
writeRaster(Belehede_NDVI_m,'Data/Ecosia/Béléhédé/Belehede2.tif') # backup

## Aribinda
writeRaster(Aribinda_NDVI_m,'Data/Ecosia/Aribinda/Aribinda.tiff')
writeRaster(Aribinda_NDVI_m,'Data/Ecosia/Aribinda/Aribinda2.tif') # backup