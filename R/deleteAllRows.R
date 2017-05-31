#' @title Deletes all rows from a dataset (V2)
#' @description Clears out all the rows of a dataset, but preserves the id and url of the dataset (as opposed to the deleteDataset function).
#'
#' This function uses the V2 version of the API.
#' @param apiKey You can find the API key in the settings after logging into Numetric
#' @param datasetId The dataset ID, in quotes. You can find the dataset ID by either (1) using the getDatasets function, or (2) navigating to the dataset, and then selecting the string after the last forward slash.
#' @export
deleteAllRows <- function(apiKey, datasetId){
  r <- DELETE(paste("https://api.numetric.com/v2/dataset/", datasetId, "/rows", sep = ""),
              add_headers("Authorization" = apiKey),
              verbose()
  )
}
