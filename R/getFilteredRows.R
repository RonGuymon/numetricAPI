#' @title Get up to 10,000 rows of a filtered dataset (V2)
#' @description Returns a dataframe with up to 10,000 rows of a dataset after applying a term or range filter.
#'
#' This is especially useful when trying to download a dataset from Numetric that has more than 10,000 rows.
#'
#' This function uses the V2 version of the API.
#' @param apiKey The API key, in quotes. You can find the API key in the settings after logging into Numetric
#' @param bucketVar The name of the dataset column, in quotes, that will be used to bucket the data.
#' @param filterType The type of filter (term or range), in quotes. Defaults to "term", but can also be "range" for a numeric or date range.
#' @param filterField The name of the column, in quotes, to be used as the filter.
#' @param filterValue If the filterType == term, then this should be the value, in quotes, by which to filter the data.
#' @param startRange If the filterType == range, then this is a minimum numeric value or date, in quotes. If it's a date, then it should be in the format of "2017-05-30T00.00.00.000".
#' @param endRange If the filterType == range, then this is a maximum numeric value or date, in quotes. If it's a date, then it should be in the format of "2017-05-30T00.00.00.000".
#' @param size The maximum number of rows of data to return. The default and maximum is 10,000.
#' @param includes Optional. A vector of stings listing the column names that you want to INCLUDE. The default is to include all columns.
#' @param excludes Optional. A vector of stings listing the column names that you want to EXCLUDE. The default is to exclude no columns.
#' @return Returns a dataframe.
#' @export
getFilteredRows <- function(apiKey, datasetId, filterType = "term", filterField = "", filterValue = "", startRange = "", endRange = "", size = 10000, includes, excludes){
  # Includes
  if(missing(includes)){
    includeCols <- ""
  } else{
    includeCols <- paste(includes, collapse = '","')
    includeCols <- paste0(',"includes":["', includeCols, '"]')
  }
  # Excludes
  if(missing(excludes)){
    excludeCols <- ""
  } else{
    excludeCols <- paste(excludes, collapse = '","')
    excludeCols <- paste0(',"excludes":["', excludeCols,'"]')
  }
  if(filterType == "term"){
    body <- paste0(
      '{"filters": [{"filter": "',filterType,'",
      "field": "',filterField,'",
      "value": "',filterValue,'"}],
      "size":',size,
      includeCols,
      excludeCols,
      '}'
    )
  } else if(filterType == "range" & class(startRange) == "numeric"){
    body <- paste0(
      '"filters": [{"filter": "',filterType,'",
      "field": "',filterField,'",
      "gte": ',startRange,',
      "lte": ',endRange,'}],
      "size":',size,
      includeCols,
      excludeCols,
      '}'
    )
  } else if(filterType == "range" & class(startRange) == "character"){
    body <- paste(
      '"filters": [{"filter": "',filterType,'",
      "field": "',filterField,'",
      "gte": "',startRange,'",
      "lte": "',endRange,'"}],
      "size":',size,
      includeCols,
      excludeCols,
      '}'
    )
  }


  r <- POST(paste('https://api.numetric.com/v2/dataset/', datasetId, '/all', sep = ""),
            add_headers("Authorization" = apiKey,
                        "Content-Type" = "application/json"),
            body = body,
            verbose()
  )
  response <- content(r, as = "text") # Saves what was returned as raw text with all the encodings
  response <- paste("[", response, "]", sep = "") # Adds the beginning and closing brackets
  response <- gsub("[\n]", ",", response) # Replaces carriage returns with commas
  response <- gsub(",]", "]", response) # Removes the last comma
  response <- fromJSON(response) %>% as.data.frame() # Converts what was returned to a dataframe
}
