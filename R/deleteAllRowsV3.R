#' @title Delete all Rows of a Table (V3)
#' @description RDeletes all the rows in a table. This function uses the V3 version of the API.
#' @param apiKey You can find the API key in the settings after logging into Numetric
#' @param tableId The id of the table. You can find this by using the getTables function, or by navigating to the table, and then selecting the string after the last forward slash of the url.
#' @export
deleteAllRowsV3 <- function(apiKey, tableId){
  payload <- '{"publish": true}'
  r <- DELETE(paste("https://api.numetric.com/v3/table/", datasetId, "/rows", sep = ""),
              add_headers("Authorization" = apiKey,
                          "Content-Type" = "application/json"),
              body = payload
  )
  return(r$status)
}

