#' @title Get a summary list of tables available to the API user (V3)
#' @description Returns a table with meta-data about all the tables. This function uses the V3 version of the API.
#' @param apiKey You can find the API key in the settings after logging into Numetric
#' @export
getTables <- function(apiKey){
  r <- GET("https://api.numetric.com/v3/table",
           add_headers("Authorization" = apiKey,
                       "Content-Type" = "application/json"),
           verbose()
  )
  response <- httr::content(r, as = "text") # Saves what was returned as raw text with all the encodings
  response <- fromJSON(response) # Converts what was returned to a dataframe
}

