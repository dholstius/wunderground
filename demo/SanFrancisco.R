require(wunderground)

# Use your Weather Underground API key here
Sys.setenv(WUNDERGROUND_API_KEY='4db496439674ad78')

# Get information on stations near "San Francisco"
SanFrancisco <- wundergroundData(features='geolookup', query='CA/San_Francisco')
nearby <- SanFrancisco$location$nearby_weather_stations
pws.data <- data.frame(do.call('rbind', nearby$pws$station))

# Get information on a specific Personal Weather Station (PWS)
stopifnot('KCASANFR58' %in% pws.data$id)
KCASANFR58 <- wundergroundData(features='geolookup', query='pws:KCASANFR58')

# Get information on all PWS near San Francisco
stations <- list()
for (id in pws.data$id) {
	message("Fetching data for ", id)
	stations[[id]] <- wundergroundData(features='geolookup', query=sprintf('pws:%s', id))
}

# Plot PWS locations
require(sp)
latlon <- function(x) with(x$location, as.numeric(c(lat, lon)))
coords <- do.call('rbind', lapply(stations, latlon))
colnames(coords) <- c('latitude', 'longitude')
stations <- SpatialPoints(coords)
plot(stations)