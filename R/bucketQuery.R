#' @title Bucket query a Numetric dataset (V2)
#' @description Returns a dataframe with a list of bucket variable values, and the number of observations for each bucket.
#'
#'  This is especially useful when you have a dataset larger than 10,000 rows because it allows you to create a loop through the data getting only the data for each bucket.
#'
#' This function uses the V2 version of the API.
#' @param apiKey The API key, in quotes. You can find the API key in the settings after logging into Numetric
#' @param datasetId The dataset ID, in quotes. The dataset ID can be found by using the getDatasets function, or by navigating to the dataset in Numetric, and selecting the string after the last forward slash.
#' @param bucketVar The name of the dataset column, in quotes, that will be used to bucket the data.
#' @param filterType The type of filter to apply. Options are "term", "range", "custom", or "none". Default value is "none".
#' @param filterField The name of the column, in quotes, to use as a filter. This should not used when the filterType is "custom".
#' @param filterValue This is only used when applying a term filter. The value in quotes, is what will be included. (If the must argument is set to "false", then this will be an exclude filter.) This should not used when the filterType is "custom".
#' @param customFilterValue This is used in conjunction with a custom filterType. The format should be: {"filter": "term", "field": "fieldName", "value": "value"}
#' @param must Whether the term filter is an include or excludes filter. By default it's set to "true", which is an includes filter. If set to "false", then it will be an excludes term filter.
#' @param lowerBound This lower end of the range, lower boundary included, which is only specified when applying a range filter. The value, in quotes, should either be a date string formatted as "2017-06-02T00:00:00.000", or a number.
#' @param upperBound This upper end of the range, upper boundary included, which is only specified when applying a range filter. The value, in quotes, should either be a date string formatted as "2017-06-02T00:00:00.000", or a number.
#' @param childField The name of a column, in quotes, that you want to perform an operation on.
#' @param childOperation The name of an operation (avg, sum, cardinality), in quotes, to perform on a child.
#' @return Returns a dataframe.
#' @export
bucketQuery <- function(apiKey, datasetId, bucketVar, filterType = "none", filterField, filterValue, customFilterValue, must = "true", lowerBound, upperBound, childField, childOperation){

  # Filters
  if(filterType == "term"){
    filters <- paste0('"filters":
    [{"filter": "', filterType, '",
      "field": "',filterField,'",
      "value": "',filterValue,'",
      "must":', must,'}],')
  } else if(filterType == "range"){
    filters <- paste0('"filters":
    [{"filter": "', filterType, '",
                      "field": "',filterField,'",
                      "gte": "',lowerBound,'",
                      "lte": "', upperBound, '"}],')
  } else if(filterType == "custom"){
    filters <- paste0('"filters":[',customFilterValue,'],')
  }
  else if(filterType == "none"){
    filters <- ""
  }


  # Children
  if(missing(childField)){
    children <- ""
  } else{
    children <- paste0(',
                "children":[
                       {
                          "key": "',childField,'",
                          "field": "',childField,'",
                          "type": "value",
                          "valueType": "',childOperation,'"
                       }
                  ]
                       ')
  }

  r <- POST(paste("https://api.numetric.com/v2/dataset/",
                  datasetId,
                  "/agg",
                  sep = ""
  ),
  add_headers("Authorization" = apiKey,
              "Content-Type" = "application/json"),
  body = paste0('{',
    filters,
    '"schema":[{
    "type": "bucket",
    "field": "',bucketVar,'",
    "key": "',bucketVar,'",
    "__size": 10000',children,'}]}'),
  verbose()
  )
  response <- httr::content(r, as = "text") # Saves what was returned as raw text with all the encodings
  response <- fromJSON(response) # Converts what was returned to a dataframe
  return(response)
}
