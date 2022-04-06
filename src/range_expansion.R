library(raster)
library(gdistance)
library(maptools)
library(rgdal)
library(jsonlite)
require(geosphere)
require(curl)
require(parallel)

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

# Get species distribution data from GBIF
res = fromJSON("https://api.gbif.org/v1/occurrence/search?speciesKey=5862470&limit=1000", flatten = TRUE)

# Remove occurrences that don't have coordinate information
res$results[, c('decimalLongitude', 'decimalLatitude')]
na <- !is.na(res$results[, 'decimalLongitude'])
notna0 <- res$results[na, ]
notna <- notna0[!duplicated(notna0[, 'decimalLongitude']), ]
  
x <- rep("NA", nrow(notna))
sampling_location <- structure(c(2.922372, 51.237312), .Dim = 1:2)

start_time <- Sys.time()
x <- sapply(1:nrow(notna), function(i) {
  gbif_occurrence <- structure(c(notna[i, 'decimalLongitude'], notna[i, 'decimalLatitude']), .Dim = 1:2)
  path <- shortestPath(tr, sampling_location, gbif_occurrence, output = "SpatialLines")
  x[i] <- geosphere::lengthLine(path) 
})
min(as.numeric(x), na.rm=T)
end_time <- Sys.time()
end_time - start_time



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
