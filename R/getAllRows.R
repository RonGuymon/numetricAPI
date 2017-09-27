#' @title Get up to 10,000 rows of a dataset (V2)
#' @description Returns a dataframe with up to 10,000 rows of a dataset.
#'
#' This function uses the V2 version of the API.
#' @param apiKey The API key, in quotes. You can find the API key in the settings after logging into Numetric
#' @param datasetId The dataset Id, in quotes. You can find the dataset Id by using the getDatasets function, or by navigating to the dataset. The string after the last forward slash in the url is the dataset Id.
#' @param size The maximum number of rows of data to return. The default and maximum is 10,000.
#' @param includes Optional. A vector of stings listing the column names that you want to INCLUDE. The default is to include all columns.
#' @param excludes Optional. A vector of stings listing the column names that you want to EXCLUDE. The default is to exclude no columns.
#' @return Returns a dataframe.
#' @export


getAllRows <- function(apiKey, datasetId, size = 10000, includes, excludes){
  # Size
  sizePart <- paste0('"size":', size)
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
  # Body
  body <- paste0(
    '{', sizePart, includeCols, excludeCols, '}'
  )
  r <- POST(paste0('https://api.numetric.com/v2/dataset/', datasetId, "/all"),
            add_headers("Authorization" = apiKey,
                        "Content-Type" = "application/json"),
            body = body,
            verbose()
  )
  response <- httr::content(r, as = "text") # Saves what was returned as raw text with all the encodings
  response <- paste("[", response, "]", sep = "") # Adds the beginning and closing brackets
  response <- gsub("[\n]", ",", response) # Replaces carriage returns with commas
  response <- gsub(",]", "]", response) # Removes the last comma
  response <- fromJSON(response) %>% as.data.frame() # Converts what was returned to a dataframe
  return(response)
}
