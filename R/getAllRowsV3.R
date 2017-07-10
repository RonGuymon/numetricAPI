#' @title Get all rows of data from a table (V3)
#' @description Returns every single row of a V3 table.
#'
#' This function uses the V3 version of the API.
#' @param apiKey You can find the API key in the settings after logging into Numetric
#' @param datasetId The id of the dataset. This can be found by navigating to the dataset in Numetric, and then referring to the string of characters after the last forward slash in the URL.
#' @return Numetric Id
#' @export


getAllRowsV3 <- function(apiKey, datasetId){
  r <- GET(paste('https://api.numetric.com/v3/table/', datasetId, '/rows', sep = ""),
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
  response
}
