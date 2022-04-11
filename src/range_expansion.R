# Load packages
library(raster)
library(gdistance)
library(maptools)
library(rgdal)
library(jsonlite)
require(geosphere)
require(curl)
#require(parallel)

# Set working directory to directory where the R-script is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # needs installation of package "rstudioapi"

# Load a map
data(wrld_simpl) #use wrld_simpl from the maptools package

# Generate a scaffold for the raster file
world_crs <- crs(wrld_simpl)
world <- wrld_simpl
worldshp <- spTransform(world, world_crs)
ras <- raster(nrow=600, ncol=600)

# Generate a raster file
worldmask <- rasterize(worldshp, ras)
worldras <- is.na(worldmask) # inverse water and land, so ocean becomes 1 and land 0
worldras[worldras==0] <- 999 # set land to 999

# Create a Transition object from the raster
tr <- transition(worldras, function(x) 1/mean(x), 16)
tr = geoCorrection(tr, scl=FALSE)

# Read a species list
sp_list <- read.csv2(file="species_list.csv", check.names=FALSE, sep=",")

for (j in 1:nrow(sp_list)){
  
species <- scan(text = sp_list[j,], what = "")
url1 <- paste("https://api.gbif.org/v1/species/match?name=", species[1], "%20", species[2], sep="")
dat <- fromJSON(url1, flatten = TRUE)

# Get species distribution data from GBIF
url2 <- paste("https://api.gbif.org/v1/occurrence/search?speciesKey=",dat$usageKey,"&limit=1000", sep="")
res = fromJSON(url2, flatten = TRUE)

# Remove occurrences that don't have coordinate information
res$results[, c('decimalLongitude', 'decimalLatitude')]
na <- !is.na(res$results[, 'decimalLongitude'])
notna0 <- res$results[na, ]
notna <- notna0[!duplicated(notna0[, 'decimalLongitude']), ]
  
x <- rep("NA", nrow(notna))
sampling_location <- structure(c(2.922372, 51.237312), .Dim = 1:2) # enter here the coordinates of the sampling site in wgs84

start_time <- Sys.time()
x <- sapply(1:nrow(notna), function(i) {
  gbif_occurrence <- structure(c(notna[i, 'decimalLongitude'], notna[i, 'decimalLatitude']), .Dim = 1:2)
  path <- shortestPath(tr, sampling_location, gbif_occurrence, output = "SpatialLines")
  x[i] <- geosphere::lengthLine(path) 
})
d <- min(as.numeric(x), na.rm=T)/1000
end_time <- Sys.time()
end_time - start_time
print(paste("The shortest distance is", d, "km for", sp_list[j,]))
}


# Some unsuccessful trials to speed up the calculations by parallelization

# For parallelization when running on a linux machine
start_time <- Sys.time()
x <- mclapply(1:nrow(notna), function(i) {
  gbif_occurrence <- structure(c(notna[i, 'decimalLongitude'], notna[i, 'decimalLatitude']), .Dim = 1:2)
  path <- shortestPath(tr, sampling_location, gbif_occurrence, output = "SpatialLines")
  x[i] <- geosphere::lengthLine(path) 
}, mc.cores=6)
min(as.numeric(x), na.rm=T)
end_time <- Sys.time()
end_time - start_time

require(foreach)
require(doParallel)

registerDoParallel(12)

start_time <- Sys.time()
foreach (i=1:nrow(notna), .combine=c) %dopar% {
  gbif_occurrence <- structure(c(notna[i, 'decimalLongitude'], notna[i, 'decimalLatitude']), .Dim = 1:2)
  path <- shortestPath(tr, sampling_location, gbif_occurrence, output = "SpatialLines")
  x[i] <- geosphere::lengthLine(path) 
}
min(as.numeric(x), na.rm=T)
end_time <- Sys.time()
end_time - start_time
