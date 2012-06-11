#' httpHeader
#'
#' Construct an object to serve as a request header in an RCurl call
#'
#' @param ...		key-value pairs
#' @return			a list (or NULL if empty)
#' @rdname http
httpHeader <- function(...) {
	if (missing(...)) return(NULL)
	else return(list(...))
}

#' httpGet
#'
#' Wrapper for similar RCurl methods
#'
#' @param url		Weather Underground url (see \link{httpUrl})
#' @param header	see \link{httpHeader}
#' @return			character
#' @rdname http
httpGet <- function(url, header=httpHeader(), curl=getCurlHandle(), ...) {
	require(RCurl)
	if (missing(...)) {
		getURLContent(url, httpheader=header)	
	} else {
		args <- list(uri=url, ..., curl=curl, .opts=list(httpheader=header, verbose=TRUE))
		do.call('getForm', args)
	}
}