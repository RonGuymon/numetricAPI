#' @title Deletes a dataset (V2)
#' @description Deletes a dataset using the API.
#'
#' This function uses the V2 version of the API.
#' @param apiKey You can find the API key in the settings after logging into Numetric
#' @param datasetId The dataset ID, in quotes. You can find the dataset ID by either (1) using the getDatasets function, or (2) navigating to the dataset, and then selecting the string after the last forward slash.
#' @export
deleteDataset <- function(apiKey, datasetId){
  r <- DELETE(paste0("https://api.numetric.com/v2/dataset/", datasetId),
              add_headers("Authorization" = apiKey),
              verbose()
  )
}
