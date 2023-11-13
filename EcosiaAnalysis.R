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

# assign rasters
## Lilengo
Lilengo <- rast('Data/Ecosia/Lilengo/Lilengo.tiff')

## Béléhédé
Belehede <- rast('Data/Ecosia/Béléhédé/Belehede.tiff')

## Aribinda
Aribinda <- rast('Data/Ecosia/Aribinda/Aribinda.tiff')

# plot rasters on map
library(leaflet)
r1 <- raster(Lilengo[[1]]) # Select only the first layer (as a RasterLayer)
pal1 <- colorNumeric(c("#ffffff", "#4dff88", "#004d1a"), values(r1),
                     na.color = "transparent")
r2 <- raster(Belehede[[1]]) 
pal2 <- colorNumeric(c("#ffffff", "#4dff88", "#004d1a"), values(r2),
                     na.color = "transparent")
r3 <- raster(Aribinda[[1]])
pal3 <- colorNumeric(c("#ffffff", "#4dff88", "#004d1a"), values(r3),
                     na.color = "transparent")

map <- leaflet() %>% addTiles() %>%
  addRasterImage(r1, colors = pal1, opacity = 0.8) %>%
  addRasterImage(r2, colors = pal2, opacity = 0.8) %>%
  addRasterImage(r3, colors = pal3, opacity = 0.8) %>%
  addLegend(pal = pal1, values = values(r1),
            title = "NDVI")
map

# extract data from the cleaned raster for selected pixels
## Lilengo
plot(Lilengo, 1)
#click(Lilengo, id=TRUE, xy=TRUE, cell=TRUE, n=1)

## Béléhédé
plot(Belehede, 1)
#click(Belehede, id=TRUE, xy=TRUE, cell=TRUE, n=1)

## Aribinda
plot(Aribinda, 1)
#click(Aribinda, id=TRUE, xy=TRUE, cell=TRUE, n=1)

# pixel selection

N_px <- 129 # pixel slightly north from the town (~250 m)
S_px <- 163 # pixel slightly south from the town (~250 m)
W_px <- 145 # pixel slightly west from the town (~250 m)
E_px <- 165 # pixel slightly east from the town (~250 m)

N_px <- 112 # pixel slightly north from the town (~500 m)
S_px <- 180 # pixel slightly south from the town (~500 m)
W_px <- 144 # pixel slightly west from the town (~500 m)
E_px <- 166 # pixel slightly east from the town (~500 m)
Belehede_px <- 214

# making the time-series
## Lilengo
Lilengo_N_tspx <- timeser(unlist(Lilengo[N_px]),as.Date(names(Lilengo), "X%Y.%m.%d")) # convert pixel "px" to a time series
Lilengo_S_tspx <- timeser(unlist(Lilengo[S_px]),as.Date(names(Lilengo), "X%Y.%m.%d")) 
Lilengo_W_tspx <- timeser(unlist(Lilengo[W_px]),as.Date(names(Lilengo), "X%Y.%m.%d")) 
Lilengo_E_tspx <- timeser(unlist(Lilengo[E_px]),as.Date(names(Lilengo), "X%Y.%m.%d")) 

plot(Lilengo_N_tspx, main = 'NDVI north Lilengo') # NDVI time series cleaned using the "reliability information"
plot(Lilengo_S_tspx, main = 'NDVI south Lilengo')
plot(Lilengo_W_tspx, main = 'NDVI west Lilengo')
plot(Lilengo_E_tspx, main = 'NDVI east Lilengo')

## Béléhédé
Belehede_N_tspx <- timeser(unlist(Belehede[N_px]),as.Date(names(Belehede), "X%Y.%m.%d"))
Belehede_S_tspx <- timeser(unlist(Belehede[S_px]),as.Date(names(Belehede), "X%Y.%m.%d")) 
Belehede_W_tspx <- timeser(unlist(Belehede[W_px]),as.Date(names(Belehede), "X%Y.%m.%d")) 
Belehede_E_tspx <- timeser(unlist(Belehede[E_px]),as.Date(names(Belehede), "X%Y.%m.%d"))

plot(Belehede_N_tspx, main = 'NDVI north Belehede') 
plot(Belehede_S_tspx, main = 'NDVI south Belehede')
plot(Belehede_W_tspx, main = 'NDVI west Belehede')
plot(Belehede_E_tspx, main = 'NDVI east Belehede')

## Aribinda
Aribinda_N_tspx <- timeser(unlist(Aribinda[N_px]),as.Date(names(Aribinda), "X%Y.%m.%d"))
Aribinda_S_tspx <- timeser(unlist(Aribinda[S_px]),as.Date(names(Aribinda), "X%Y.%m.%d")) 
Aribinda_W_tspx <- timeser(unlist(Aribinda[W_px]),as.Date(names(Aribinda), "X%Y.%m.%d")) 
Aribinda_E_tspx <- timeser(unlist(Aribinda[E_px]),as.Date(names(Aribinda), "X%Y.%m.%d")) 

plot(Belehede_N_tspx, main = 'NDVI north Aribinda') 
plot(Belehede_S_tspx, main = 'NDVI south Aribinda')
plot(Belehede_W_tspx, main = 'NDVI west Aribinda')
plot(Belehede_E_tspx, main = 'NDVI east Aribinda')

# BFAST Lite
## Lilengo
Lilengo_N_breaks <- bfastlite(Lilengo_N_tspx, response ~ trend + harmon, order = 3, breaks = "BIC")
Lilengo_S_breaks <- bfastlite(Lilengo_S_tspx, response ~ trend + harmon, order = 3, breaks = "BIC")
Lilengo_W_breaks <- bfastlite(Lilengo_W_tspx, response ~ trend + harmon, order = 3, breaks = "BIC")
Lilengo_E_breaks <- bfastlite(Lilengo_E_tspx, response ~ trend + harmon, order = 3, breaks = "BIC")

# breaks
plot(Lilengo_N_breaks, main = 'Breaks north Lilengo')
Lilengo_N_breaks
plot(Lilengo_S_breaks, main = 'Breaks south Lilengo')
Lilengo_S_breaks
plot(Lilengo_W_breaks, main = 'Breaks west Lilengo')
Lilengo_W_breaks
plot(Lilengo_E_breaks, main = 'Breaks east Lilengo')
Lilengo_E_breaks

# trend
Lilengo_N_bft <- bfastlite(Lilengo_N_tspx, formula=response~trend)# + harmon) # more accurate without harmon
coef(Lilengo_N_bft$breakpoints)
coef(Lilengo_N_bft$breakpoints, breaks="LWZ")

Lilengo_S_bft <- bfastlite(Lilengo_S_tspx, formula=response~trend)# + harmon) 
coef(Lilengo_S_bft$breakpoints)
coef(Lilengo_S_bft$breakpoints, breaks="LWZ")

Lilengo_W_bft <- bfastlite(Lilengo_W_tspx, formula=response~trend)# + harmon) 
coef(Lilengo_W_bft$breakpoints)
coef(Lilengo_W_bft$breakpoints, breaks="LWZ")

Lilengo_E_bft <- bfastlite(Lilengo_E_tspx, formula=response~trend)# + harmon) 
coef(Lilengo_E_bft$breakpoints)
coef(Lilengo_E_bft$breakpoints, breaks="LWZ")

## Béléhédé
Belehede_N_breaks <- bfastlite(Belehede_N_tspx, response ~ trend + harmon, order = 3, breaks = "BIC")
Belehede_S_breaks <- bfastlite(Belehede_S_tspx, response ~ trend + harmon, order = 3, breaks = "BIC")
Belehede_W_breaks <- bfastlite(Belehede_W_tspx, response ~ trend + harmon, order = 3, breaks = "BIC")
Belehede_E_breaks <- bfastlite(Belehede_E_tspx, response ~ trend + harmon, order = 3, breaks = "BIC")

# breaks
plot(Belehede_N_breaks, main = 'Breaks north Béléhédé')
Belehede_N_breaks
plot(Belehede_S_breaks, main = 'Breaks south Béléhédé')
Belehede_S_breaks
plot(Belehede_W_breaks, main = 'Breaks west Béléhédé')
Belehede_W_breaks
plot(Belehede_E_breaks, main = 'Breaks east Béléhédé')
Belehede_E_breaks

# trend
Belehede_N_bft <- bfastlite(Belehede_N_tspx, formula=response~trend)# + harmon) 
coef(Belehede_N_bft$breakpoints)
coef(Belehede_N_bft$breakpoints, breaks="LWZ")

Belehede_S_bft <- bfastlite(Belehede_S_tspx, formula=response~trend)# + harmon) 
coef(Belehede_S_bft$breakpoints)
coef(Belehede_S_bft$breakpoints, breaks="LWZ")

Belehede_W_bft <- bfastlite(Belehede_W_tspx, formula=response~trend)# + harmon) 
coef(Belehede_W_bft$breakpoints)
coef(Belehede_W_bft$breakpoints, breaks="LWZ")

Belehede_E_bft <- bfastlite(Belehede_E_tspx, formula=response~trend)# + harmon) 
coef(Belehede_E_bft$breakpoints)
coef(Belehede_E_bft$breakpoints, breaks="LWZ")

## Aribinda
Aribinda_N_breaks <- bfastlite(Aribinda_N_tspx, response ~ trend + harmon, order = 3, breaks = "BIC")
Aribinda_S_breaks <- bfastlite(Aribinda_S_tspx, response ~ trend + harmon, order = 3, breaks = "BIC")
Aribinda_W_breaks <- bfastlite(Aribinda_W_tspx, response ~ trend + harmon, order = 3, breaks = "BIC")
Aribinda_E_breaks <- bfastlite(Aribinda_E_tspx, response ~ trend + harmon, order = 3, breaks = "BIC")

# breaks
plot(Aribinda_N_breaks, main = 'Breaks north Aribinda')
Aribinda_N_breaks
plot(Aribinda_S_breaks, main = 'Breaks south Aribinda')
Aribinda_S_breaks
plot(Aribinda_W_breaks, main = 'Breaks west Aribinda')
Aribinda_W_breaks
plot(Aribinda_E_breaks, main = 'Breaks east Aribinda')
Aribinda_E_breaks

# trend
Aribinda_N_bft <- bfastlite(Aribinda_N_tspx, formula=response~trend)# + harmon)
coef(Aribinda_N_bft$breakpoints)
coef(Aribinda_N_bft$breakpoints, breaks="LWZ")

Aribinda_S_bft <- bfastlite(Aribinda_S_tspx, formula=response~trend)# + harmon) 
coef(Aribinda_S_bft$breakpoints)
coef(Aribinda_S_bft$breakpoints, breaks="LWZ")

Aribinda_W_bft <- bfastlite(Aribinda_W_tspx, formula=response~trend)# + harmon) 
coef(Aribinda_W_bft$breakpoints)
coef(Aribinda_W_bft$breakpoints, breaks="LWZ")

Aribinda_E_bft <- bfastlite(Aribinda_E_tspx, formula=response~trend)# + harmon) 
coef(Aribinda_E_bft$breakpoints)
coef(Aribinda_E_bft$breakpoints, breaks="LWZ")

# The code for getting a date from above, in a function
# index is which breakpoint to list, tspx is the original time series
IndexToDate <- function(index, tspx, breaks) {
  dates.no.na <- as.numeric(time(tspx))
  dates.no.na[is.na(tspx)] <- NA
  dates.no.na <- na.omit(dates.no.na)
  dates.no.na[breaks$breakpoints$breakpoints[index]]
}

bflRaster <- function(pixels, dates, timeser, IndexToDate) {
  library(zoo)
  library(bfast)
  tspx <- timeser(pixels, dates)
  breaks <- bfastlite(tspx, response ~ trend + harmon, order = 3, breaks = "BIC")
  
  # If no break, return NAs
  if (is.na(breaks$breakpoints$breakpoints))
    return(c(NA,NA))
  
  # Get break with highest magnitude
  mags <- magnitude(breaks$breakpoints)
  maxMag <- which.max(mags$Mag[,"RMSD"])
  
  return(c(IndexToDate(maxMag, tspx, breaks), mags$Mag[maxMag, "RMSD"]))
}

# assign break & magnitude rasters
## Lilengo
Lilengo_bflR <- rast('Data/Ecosia/Lilengo/Lilengo_magnitude.tiff')

## Béléhédé
Belehede_bflR <- rast('Data/Ecosia/Béléhédé/Belehede_magnitude.tiff')

## Aribinda
Aribinda_bflR <- rast('Data/Ecosia/Aribinda/Aribinda_magnitude.tiff')

# plot magnitudes of whole areas
## Lilengo
names(Lilengo_bflR) <- c('time of break', 'magnitude of change')
plot(Lilengo_bflR)

## Béléhédé
names(Belehede_bflR) <- c('time of break', 'magnitude of change')
plot(Belehede_bflR)

## Aribinda
names(Aribinda_bflR) <- c('time of break', 'magnitude of change')
plot(Aribinda_bflR)

##########################----------------------------- (If needed) Download break & magnitude rasters

# Best to use all cores! But then you have to include the definition of `timeser()` in `bflRaster`.
#system.time({
  ### Lilengo
#  Lilengo_bflR <- app(Lilengo, bflRaster, dates=as.Date(names(Lilengo), "X%Y.%m.%d"), timeser=timeser, IndexToDate=IndexToDate, cores=4)
  
  ### Béléhédé
#  Belehede_bflR <- app(Belehede, bflRaster, dates=as.Date(names(Belehede), "X%Y.%m.%d"), timeser=timeser, IndexToDate=IndexToDate, cores=4)
  
  ### Aribinda
#  Aribinda_bflR <- app(Aribinda, bflRaster, dates=as.Date(names(Aribinda), "X%Y.%m.%d"), timeser=timeser, IndexToDate=IndexToDate, cores=4)
#})

# write to save rasters
### Lilengo
#writeRaster(Lilengo_bflR,'Data/Ecosia/Lilengo/Lilengo_magnitude.tiff', overwrite =  TRUE)
#writeRaster(Lilengo_bflR,'Data/Ecosia/Lilengo/Lilengo_magnitude2.tif', overwrite =  TRUE) # backup

### Béléhédé
#writeRaster(Belehede_bflR,'Data/Ecosia/Béléhédé/Belehede_magnitude.tiff', overwrite =  TRUE)
#writeRaster(Belehede_bflR,'Data/Ecosia/Béléhédé/Belehede_magnitude2.tif', overwrite =  TRUE) # backup

### Aribinda
#writeRaster(Aribinda_bflR,'Data/Ecosia/Aribinda/Aribinda_magnitude.tiff', overwrite =  TRUE)
#writeRaster(Aribinda_bflR,'Data/Ecosia/Aribinda/Aribinda_magnitude2.tif', overwrite =  TRUE) # backup