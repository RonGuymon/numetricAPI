#' @title Get the status of a dataset (V2)
#' @description Find out if the index is complete.  This function uses the V2 version of the API.
#' @param apiKey You can find the API key in the settings after logging into Numetric
#' @param datasetId The dataset ID can be found by using the getDatasets function, or by navigating to the dataset in Numetric, and selecting the string after the last forward slash.
#' @return Returns a dataframe.
#' @export
getDatasetStatus <- function(apiKey, datasetId){
  r <- GET(paste0("https://api.numetric.com/v2/dataset/",datasetId),
           add_headers("Authorization" = apiKey,
                       "Content-Type" = "application/json"),
           verbose()
  )
  response <- content(r, as = "text") # Saves what was returned as raw text with all the encodings
  response <- fromJSON(response) # Converts what was returned to a dataframe
}

