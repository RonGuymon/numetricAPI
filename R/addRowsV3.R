#' @title Add rows to a dataset (V3)
#' @description Adds data to a new or existing dataset. If the primary key already exists in the indexed data, then the old data will be replaced with the new data.
#'
#' If the primary key doesn't already exist in the dataset, then the new data will be added on to the existing data.
#'
#' This function uses the V3 version of the API.
#' @param apiKey You can find the API key in the settings after logging into Numetric
#' @param datasetId The dataset ID, in quotes. It can be found by using the getDatasets function, or by navigating to the dataset in Numetric, and selecting the string after the last forward slash.
#' @param dataframeName The name of the dataframe, without quotes.
#' @param chunkSize Defaults to 1,500 rows.
#' @param printSample Defaults to False. If set to True, then it will return a sample of the data that is being indexed to verify that it's correctly being converted to JSON.
#' @param nullArrays Defaults to False. Set to true if your data has arrays within a cell, and if some of those arrays have NULL values.
#' @return Returns the status (e.g., 200, 401).
#' @export
addRowsV3 <- function(apiKey, datasetId, dataframeName, chunkSize = 1500, printSample = F, nullArrays = F){

  iterations <- ceiling(nrow(dataframeName)/chunkSize)
  nrows <- nrow(dataframeName)

  for(i in 1:iterations){
    startNum = ((i-1)*chunkSize)+1
    endNum = i*chunkSize
    if(endNum > nrows){
      endNum <- nrows
    }
    dataToIndex <- jsonlite::toJSON(dataframeName[startNum:endNum,], dataframe = "rows")
    indexReadyData <- paste('{"index": false, "rows":',
                            dataToIndex,
                            '}',
                            sep = '')
    if(nullArrays == T){
      indexReadyData <- gsub("\\{}",'[""]',indexReadyData)
    }
    r <- POST(paste("https://api.numetric.com/v3/table/", datasetId, "/rows", sep = ""),
              add_headers("Authorization" = apiKey,
                          "Content-Type" = "application/json"),
              body = indexReadyData
    )
    # Sample output
    if(printSample == T){
      cat(indexReadyData)
    }

    # Error message
    ifelse(content(r)$type != "SERVER_ERROR", "",
           print(paste(content(r)$message,
                       ". The error starts at 10 characters in from this: ",
                       substr(indexReadyData,
                              as.numeric(gsub(".*position ", "", content(r)$message)
                              )-10,
                              as.numeric(gsub(".*position ", "", content(r)$message)) + 10),
                       ". Between rows ",
                       startNum,
                       " and ",
                       endNum ,
                       sep = ""
           )
           )
    )
    # ifelse(content(r)$success == T, print("Looks good"), "")
    if(iterations == 1 | i == iterations){
      cat("100% processed", "\n")
    }  else{
      cat(round(i*chunkSize/nrows, 2)*100,"% processed, starting with row", i*chunkSize, "\n")
    }
  }
  # system("say Done adding rows Data indexing",wait=F)
  s <- GET(paste("https://api.numetric.com/v3/table/", datasetId, "/publish", sep = ""),
           add_headers("Authorization" = apiKey)
  )
  return(s$status)
}

