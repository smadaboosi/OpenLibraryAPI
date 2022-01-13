search.API <- function(input){
  searchTerms <- stringr::str_replace_all(input, " ", "+")
  api.request <- paste0('http://openlibrary.org/search.json?q=', searchTerms)
  results <- httr::GET(api.request)
  jsonlite::fromJSON(rawToChar(results$content))
}

get.current.page <- function(x){
  as.numeric(substr(x,nchar(x)-1, nchar(x)))
}
