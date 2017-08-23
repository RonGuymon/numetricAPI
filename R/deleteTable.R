#' @title Deletes a table (V3)
#' @description Deletes a table using the API.
#'
#' This function uses the V3 version of the API.
#' @param apiKey You can find the API key in the settings after logging into Numetric
#' @param tableId The table ID, in quotes. You can find the table ID by either (1) using the getTables function, or (2) navigating to the table, and then selecting the string after the last forward slash.
#' @export
deleteTable <- function(apiKey, tableId){
  r <- DELETE(paste0("https://api.numetric.com/v3/table/", tableId),
              add_headers("Authorization" = apiKey),
              verbose()
  )
}
