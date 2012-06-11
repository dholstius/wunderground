#' wundergroundKey
#'
#' Look up user's Weather Underground API key. 
#'
#' @note				Defaults to the WUNDERGROUND_API_KEY environment variable.
#' @return				character
#' 
#' @export
wundergroundKey <- function() {
	key <- Sys.getenv('WUNDERGROUND_API_KEY')
	if (key == "") {
		warning("WUNDERGROUND_API_KEY was undefined. Use setKey() to establish a key for this session. Or export WUNDERGROUND_API_KEY from your .bashrc or .bash_profile (for example).")
		key <- NULL
	}
	return(key)
}

#' wundergroundKey
#'
#' Construct a Weather Underground API url. See http://www.wunderground.com/weather/api/d/documentation.html for more.
#'
#' @param	features	character 
#' @param	query		character
#' @param	key			character
#' @param	settings	character
#' @param	format		character
#
#' @return				character
#' @export
wundergroundUrl <- function(features, query, key, settings, format='json') {
	url <- 'http://api.wunderground.com/api'
	url <- sprintf('%s/%s/%s', url, key, paste(features, collapse='/', sep=''))
	if (!missing(settings)) {
		url <- sprintf('%s/%s', url, paste(settings, collapse=':', sep=''))
	}
	url <- sprintf('%s/q/%s.%s', url, query, format)
	return(url)
}

#' wundergroundKey
#'
#' Fetch data from the Weather Underground API. See http://www.wunderground.com/weather/api/d/documentation.html for more.
#'
#' @param	features	character 
#' @param	query		character
#' @param	key			character
#
#' @return				nested list
#' @export
wundergroundData <- function(features, query, key=wundergroundKey(), cache=TRUE) {
	require(rjson)
	url <- wundergroundUrl(features, query, key, format='json')
	response <- httpGet(url, cache=cache)
	result <- fromJSON(response)
	return(result)
}