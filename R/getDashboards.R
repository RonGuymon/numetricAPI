#' @title Get a meta-data for all dashboards in an org (V2)
#' @description Returns a dataframe with meta-data about all of the dashboards in an org.
#'
#' This function uses the V2 version of the API.
#' @param apiKey You can find the API key in the settings after logging into Numetric
#' @return Returns a dataframe.
#' @export
getDashboards <- function(apiKey){
  r <- GET("https://api.numetric.com/v2/dashboard",
           add_headers("Authorization" = apiKey),
           verbose()
  )
  response <- httr::content(r, as = "text") # Saves what was returned as raw text with all the encodings
  response <- fromJSON(response) # Converts what was returned to a dataframe
}

