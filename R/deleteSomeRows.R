#' @title Deletes specific rows from a dataset (V2)
#' @description Deletes rows of a dataset based on a vector of primary keys.
#'
#' This function uses the V2 version of the API.
#' @param apiKey You can find the API key in the settings after logging into Numetric
#' @param datasetId The dataset ID, in quotes. You can find the dataset ID by either (1) using the getDatasets function, or (2) navigating to the dataset, and then selecting the string after the last forward slash.
#' @param rows A vector of rows containing the primary keys of the rows to be deleted.
#' @export
deleteSomeRows <- function(apiKey, datasetId, rows){
  rows <- toJSON(rows) # Converts rows to JSON format: ["id1", "id2", "id3"]
  r <- DELETE(paste("https://api.numetric.com/v2/dataset/", datasetId, "/rows", sep = ""),
              add_headers("Authorization" = apiKey,
                          "Content-Type" = "application/json"),
              body = paste0('{
                            "rows":', rows, '
}'),
              verbose()
              )
}
