#' @title Get up to 10,000 rows of a dataset (V2)
#' @description Returns a dataframe with up to 10,000 rows of a dataset.
#'
#' This function uses the V2 version of the API.
#' @param apiKey The API key, in quotes. You can find the API key in the settings after logging into Numetric
#' @param bucketVar The name of the dataset column, in quotes, that will be used to bucket the data.
#' @return Returns a dataframe.
#' @export
getAllRows <- function(apiKey, datasetId){
  r <- POST(paste0('https://api.numetric.com/v2/dataset/', datasetId, "/all"),
            add_headers("Authorization" = apiKey,
                        "Content-Type" = "application/json"),
            verbose()
  )
  response <- content(r, as = "text") # Saves what was returned as raw text with all the encodings
  response <- paste("[", response, "]", sep = "") # Adds the beginning and closing brackets
  response <- gsub("[\n]", ",", response) # Replaces carriage returns with commas
  response <- gsub(",]", "]", response) # Removes the last comma
  response <- fromJSON(response) %>% as.data.frame() # Converts what was returned to a dataframe
}
