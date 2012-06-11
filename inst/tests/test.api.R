test_that('key is established', {
	expect_false(is.null(wundergroundKey()))
})

features <- c('geolookup', 'conditions', 'forecast')
query <- 'CA/San_Francisco'
key <- '4db496439674ad78'

test_that('urls are formed correctly', {
	expect_equal(
		wundergroundUrl(features, query, key, format='json'), 
		'http://api.wunderground.com/api/4db496439674ad78/geolookup/conditions/forecast/q/CA/San_Francisco.json'
	)	
})

# test_that('data is returned', {
# 	expect_equal(
# 		wundergroundData(features, query, key), 
# 	)	
# })