# Load libraries
require(jsonlite)
require(gifski)
require(gganimate)
require(rgbif)

# Login data for rgbif
.user = "pascal_habluetzel"
.pwd = ""
.email = "pascal.hablutzel@vliz.be"

# Set working directory to directory where the R-script is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # requires installation of package "rstudioapi"

# Read a species list
sp_list <- read.csv2(file="species_list.csv", check.names=FALSE, sep=",")

# Make a polygon around the sampling location
long <- 2.922372
lat <- 51.237312
o <- 20
species <- scan(text = sp_list[1,], what = "")

# Write a function to produce animations
range_expansion_animation <- function(longitude, latitude, species, window){  
  polygon <- paste("Polygon ((",longitude-window/2,latitude+window/2,",",longitude+window/2,latitude+window/2,",",longitude+window/2,latitude-window/2,",",longitude-window/2,latitude-window/2,",",longitude-window/2,latitude+window/2,"))", sep=" ")
  url1 <- paste("https://api.gbif.org/v1/species/match?name=", species[1], "%20", species[2], sep="")
  dat <- fromJSON(url1, flatten = TRUE)
  gbif_download <- occ_download(type="and", pred("taxonKey", dat$speciesKey), pred_within(polygon), format = "SIMPLE_CSV", user = .user, pwd = .pwd, email = .email)
  occ_download_wait(gbif_download)
  d <<- occ_download_get(gbif_download, overwrite=T) %>%
    occ_download_import()
  d <- d[complete.cases(d$eventDate),]
  date <- as.POSIXct(d$eventDate)
  year <- as.integer(format(date, format="%Y"))
  
  world = map_data("world")
  p <- ggplot(world, aes(long, lat)) +
    coord_sf(xlim = c(long-o, long+o), ylim = c(lat-o, lat+o)) + 
    geom_polygon(aes(group = group), fill = "white", 
                 color = "gray40", size = .2) +
    geom_jitter(data = d,
                aes(decimalLongitude, decimalLatitude), alpha=0.6, 
                size = 4, color = "red")
  map_with_animation <- p +
    transition_time(year) +
    ggtitle('Year: {frame_time}',
            #subtitle = 'Frame {frame} of {nframes}')
            subtitle = paste(species[1],species[2], sep=" "))
  num_years <- max(year) - min(year) + 1
  #animate(map_with_animation, nframes = num_years)
  
  
  map_with_shadow <- map_with_animation +
    shadow_mark()
  animate(map_with_shadow, nframes = num_years, renderer = gifski_renderer())
  
  anim_save(paste(species[1],"_",species[2],".gif", sep=""), map_with_shadow, nframes = num_years, fps = 2, renderer = gifski_renderer())
  }

# Run the function to produce a gif
range_expansion_animation(longitude=long, latitude=lat, species=species, window=o)