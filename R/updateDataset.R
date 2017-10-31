#' @title Updates the parameters of an existing dataset (V2)
#' @description Uupdate an existing dataset in a Numetric Org.
#'
#' This is useful when you want to add new columns to a dataset. You can also use it change the name or category of a dataset.
#'
#' This function uses the V2 version of the API.
#' @param apiKey You can find the API key in the settings after logging into Numetric
#' @param datasetId The dataset ID, in quotes, to be updated. It can be found by using the getDatasets function, or by navigating to the dataset in Numetric, and selecting the string after the last forward slash.
#' @param numetricName The name, in quotes, given to the dataset in the Numetric Warehouse.
#' @param dataframeName The name, without quotes, of the dataframe that will be used to create the dataset in Numetric.
#' @param category The category, in quotes, given to the dataset in the Numetric Warehouse.
#' @param id The name of the column, in quotes, that will be used as the primary key. Each row should have a unique id, otherwise, the last row indexed will be the one saved.
#' @param autocompletes The column name, or vector of column names, in quotes, that will autocomplete in the Numetric quick search bar.
#' @param geoshapes The column name, or vector of column names, in quotes, that will be stored as a geoShape (point on a map). This needs to be in the format, "lat,long".
#' @param geopoints The column name, or vector of column names, in quotes, that will be stored as a geoPoint (for heatmaps). This needs to be in the format, "lat,long".
#' @param boolean The column name, or vector of column names, in quotes, that will be stored as True/False. True = 1, False = 0.
#' @return Returns the status of the update.
#' @export
updateDataset <- function(apiKey, datasetId, numetricName, dataframeName, category = "New Data", autocompletes = "", geoshapes = "", geopoints = "", boolean = ""){
  if(autocompletes == ""){
    fieldAttributes <- data.frame(field = colnames(dataframeName), # Gets the column names of the dataset to index
                                  displayName = colnames(dataframeName), # Uses the column names of the dataset as the display names
                                  type = array(lapply(dataframeName, class)) %>% as.character() # Gets the class of each column in the dataframe
    )
  }else{
    fieldAttributes <- data.frame(field = colnames(dataframeName), # Gets the column names of the dataset to index
                                  displayName = colnames(dataframeName), # Uses the column names of the dataset as the display names
                                  autocomplete = rep("false", ncol(dataframeName)), # Does not make anything autocompletable
                                  type = array(lapply(dataframeName, class)) %>% as.character() # Gets the class of each column in the dataframe
    )
  }

  # Convert r class types to valid numetric class types
  fieldAttributes <- mutate(fieldAttributes, type = ifelse(type == "character", "string",
                                                           ifelse(type == "numeric", "double",
                                                                  ifelse(type == "integer", "integer",
                                                                         ifelse(type == "Date" | type == "POSIXct" | type == "POSIXlt", "datetime", "string")))))
  # Add in the fields that should be autocompleted and geocoded
  if(autocompletes == ""){
    fieldAttributes <- mutate(fieldAttributes,
                              type = ifelse(field %in% geoshapes, "geo_shape",
                                            ifelse(field %in% geopoints, "geo_point", type)),
                              type = ifelse(grepl("date", field, ignore.case = T), "datetime", type),
                              type = ifelse(field %in% boolean, "boolean", type)
    )
  }else{
    fieldAttributes <- mutate(fieldAttributes,
                              autocomplete = ifelse(field %in% autocompletes, "true", "false"),
                              type = ifelse(field %in% geoshapes, "geo_shape",
                                            ifelse(field %in% geopoints, "geo_point", type)),
                              type = ifelse(grepl("date", field, ignore.case = T), "datetime", type),
                              type = ifelse(field %in% boolean, "boolean", type)
    )
  }

  fieldAttributes <- jsonlite::toJSON(fieldAttributes, dataframe = "rows") # Convert to JSON format
  fieldAttributes <- gsub('\\"true\\"', 'true', fieldAttributes) # Remove quotes around true for autocomplete
  fieldAttributes <- gsub('\\"false\\"', 'false', fieldAttributes) # Remove quotes around false for autocomplete
  ## Merge the field attributes with the other information required----
  metadata <- paste('{"name": "', numetricName,
                    '","categories": ["', category,
                    '"],"description": "Stuff indexed through R",',
                    '"fields":',
                    fieldAttributes,
                    '}',
                    sep = "")
  r <- PATCH(paste('https://api.numetric.com/v2/dataset/', datasetId, sep = ""),
             add_headers("Authorization" = apiKey,
                         "Content-Type" = "application/json"),
             body = metadata,
             verbose()
  )
  response <- httr::content(r, as = "text") # Saves what was returned as raw text with all the encodings
}


