require(wunderground)

# Use your Weather Underground API key here
Sys.setenv(WUNDERGROUND_API_KEY='4db496439674ad78')

# Get historical information for "San Francisco"
Berkeley <- wundergroundData(features='geolookup', query='CA/Berkeley')

# Do a query on every Personal Weather Station (PWS) near San Francisco
nearby <- Berkeley$location$nearby_weather_stations
ids <- sapply(nearby$pws$station, `[[`, 'id')
PWS <- list()
for (i in ids) {
	message("Fetching data for ", i)
	PWS[[i]] <- wundergroundData(features='geolookup', query=sprintf('pws:%s', i))
}

# Plot PWS locations
require(sp)
lonlat <- function(x) with(x$location, as.numeric(c(lon, lat)))
coords <- t(sapply(PWS, lonlat))
colnames(coords) <- c('longitude', 'latitude')
WGS84 <- CRS("+proj=longlat +ellps=WGS84")
stations <- SpatialPointsDataFrame(coords, data.frame(name=row.names(coords)), match.ID=FALSE, proj4string=WGS84)
plot(stations)

# Write locations to a KML file
require(rgdal)
writeOGR(stations, 'Wunderground-Berkeley.kml', 'PWS', driver='KML')

#
# Historical observations
#
addClass <- function(object, x) {
	class(object) <- c(x, class(object))
	return(object)
}

wundergroundHistory <- function(date, ...) {
	feature <- sprintf('history_%s', strftime(date, '%Y%m%d'))
	data <- wundergroundData(feature, ...)
	data <- within(data, {
		history <- addClass(history, "History")
		history$observations <- addClass(history$observations, "Observations")
	})
	return(data)
}

as.data.frame.Observations <- function(x, stringsAsFactors=FALSE, ...) {
	f <- function(observation) {
		with(observation, data.frame(
			Datetime = with(date, ISOdate(year, mon, mday, hour, min, tz=tzname)),
			Temperature = as.numeric(tempm),	# in Celsius
			Dewpoint = as.numeric(dewptm),		# in Celsius
			Humidity = as.numeric(hum),			# in %
			WindSpeed = as.numeric(wspdm),		# in kph
			WindDirection = as.numeric(wdird),	# in degrees
			stringsAsFactors = stringsAsFactors,
			...
		))
	}
	do.call('rbind', lapply(x, f))
}

observations <- function(data) as.data.frame(data$history$observations)

dates <- seq(as.Date('2011-10-01'), by=1, length.out=14)
Berkeley <- list()
for (d in dates) {
	Berkeley[[d]] <- wundergroundHistory(d, query='CA/San_Francisco')
	Sys.sleep(7)	# API rate limit is 10 per minute
}
hourly <- do.call('rbind', lapply(Berkeley, observations))

require(zoo)
require(xts)
z <- with(hourly, as.xts(zoo(Humidity, Datetime)))
plot(z, main="Humidity in Berkeley")

stl(z)