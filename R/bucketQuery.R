#' @title Bucket query a Numetric dataset (V2)
#' @description Returns a dataframe with a list of bucket variable values, and the number of observations for each bucket.
#'
#'  This is especially useful when you have a dataset larger than 10,000 rows because it allows you to create a loop through the data getting only the data for each bucket.
#'
#' This function uses the V2 version of the API.
#' @param apiKey The API key, in quotes. You can find the API key in the settings after logging into Numetric
#' @param datasetId The dataset ID, in quotes. The dataset ID can be found by using the getDatasets function, or by navigating to the dataset in Numetric, and selecting the string after the last forward slash.
#' @param bucketVar The name of the dataset column, in quotes, that will be used to bucket the data.
#' @return Returns a dataframe.
#' @export
bucketQuery <- function(apiKey, datasetId, bucketVar){

  r <- POST(paste("https://api.numetric.com/v2/dataset/",
                  datasetId,
                  "/agg",
                  sep = ""
  ),
  add_headers("Authorization" = apiKey,
              "Content-Type" = "application/json"),
  body = paste(
    '{"schema":[{
    "type": "bucket",
    "field": "',
    bucketVar,
    '","key": "',
    bucketVar,
    '", "__size": 10000}]}',
    sep = ""),
  verbose()
  )
  response <- content(r, as = "text") # Saves what was returned as raw text with all the encodings
  response <- fromJSON(response) # Converts what was returned to a dataframe
  return(response)
}
