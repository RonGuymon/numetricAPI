#' @title Update a specific table (V3)
#' @description Updates a table using a dataframe.
#'
#' This function uses the V3 version of the API.
#' @param apiKey Required. You can find the API key in the settings after logging into Numetric
#' @param tableId Required. The id, in quotes, of a table in the Data Warehouse.
#' @param numetricName The name, in quotes, given to the dataset in the Numetric Warehouse.
#' @param description A description of the dataframe in quotes.
#' @param dataframeName The name, without quotes, of the dataframe that will be used to create the dataset in Numetric.
#' @param category The category, in quotes, given to the dataset in the Numetric Warehouse.
#' @param primaryKey The name of the column, in quotes, that will be used as the primary key. Each row should have a unique id, otherwise, the last row indexed will be the one saved.
#' @param geoshapes The column name, or vector of column names, in quotes, that will be stored as a geoShape (point on a map). This needs to be in the format, "lat,long".
#' @param geopoints The column name, or vector of column names, in quotes, that will be stored as a geoPoint (for heatmaps). This needs to be in the format, "lat,long".
#' @param boolean The column name, or vector of column names, in quotes, that will be stored as True/False. True = 1, False = 0.
#' @param transformations A dataframe of transformations. Columns are: actionType, displayName, index, name, sourceField, targetField, type, and values. Values is a list of lists. It's easiest to use the getTable function on a table that already has transofrmations, and then look at the transformations part.
#' @return Numetric Id
#' @export

updateTable <- function(apiKey, tableId, numetricName, dataframeName = "absent__default", category, primaryKey, geoshapes = "", geopoints = "", boolean = "", transformations){
  # Example: nApiCreateTableV3(apiKey = apiKey, numetricName = "Retail Sports",dataframeName = sport2Sample,category = "Retail Sports", primaryKey="primaryKey")
  # Be sure to have a field in the dataframe that is a unique value for each row.

  if(dataframeName == "absent__default"){
    fieldAttributesReady_body <- '"deleteMe"'
  }else {
    ## Create a dataframe with column attributes----
    # Create a dataframe with columns for each field: field, displayName, autocomplete, and type.
    fieldAttributes <- data.frame(field = colnames(dataframeName), # Gets the column names of the dataset to index
                                  displayName = colnames(dataframeName), # Uses the column names of the dataset as the display names
                                  # autocomplete = rep("false", ncol(dataframeName)), # Does not make anything autocompletable
                                  type = array(lapply(dataframeName, class)) %>% as.character() # Gets the class of each column in the dataframe
    )
    # Convert r class types to valid numetric class types
    fieldAttributes <- mutate(fieldAttributes, type = ifelse(type == "character", "string",
                                                             ifelse(type == "numeric", "double",
                                                                    ifelse(type == "integer", "integer",
                                                                           ifelse(type == "Date" | type == "POSIXct" | type == "POSIXlt", "datetime", "string")))))

    # Add in the fields that should be autocompleted and geocoded
    fieldAttributes <- mutate(fieldAttributes,
                              type = ifelse(field %in% geoshapes, "geo_shape",
                                            ifelse(field %in% geopoints, "geo_point", type)),
                              type = ifelse(grepl("date", field, ignore.case = T), "datetime", type),
                              type = ifelse(field %in% boolean, "boolean", type)
    )

    wholeString <- ""
    for (i in 1:nrow(fieldAttributes)){

      tempString <- paste0('"',fieldAttributes[i,1],'": {"displayName": "',fieldAttributes[i,2] ,'","type": "',fieldAttributes[i,3],'"},')

      wholeString <- paste0(wholeString,tempString)
      if(i == nrow(fieldAttributes)){
        wholeString <- gsub(",$","",wholeString)
      }

    }

    fieldAttributesReady_body <- paste0('"fields": {',wholeString,'}')

  }

  if(exists("numetricName")){
    numetricName_body <- paste0('"name": "', numetricName, '"')
  }else{
    numetricName_body <- '"deleteMe"'
  }

  if(exists("primaryKey")){
    primaryKey_body <- paste0('"primaryKey": ["', primaryKey, '"]')
  }else{
    primaryKey_body <- '"deleteMe"'
  }

  if(exists("category")){
    category_body <- paste0('"categories": ["', category, '"]')
  }else{
    category_body <- '"deleteMe"'
  }

  if(exists("description")){
    description_body <- paste0('"description": "', description, '"')
  }else{
    description_body <- '"deleteMe"'
  }

  if(exists("transformations")){
    transformations_body <- paste0('"transformations":', toJSON(transformations))
  }else{
    transformations_body <- '"deleteMe"'
  }

  ## Merge the field attributes with the other information required----
  metadata <- paste(numetricName_body,
                    primaryKey_body,
                    category_body,
                    description_body,
                    fieldAttributesReady_body,
                    transformations_body,
                    sep = ",") %>%
    gsub('"deleteMe",|"deleteMe"|,"deleteMe"', '', .) %>%
    paste0('{', ., '}')

  # Index the data----
  r <- PATCH(paste0("https://api.numetric.com/v3/table/", tableId),
            add_headers("Authorization" = apiKey,
                        "Content-Type" = "application/json"),
            body = metadata,
            verbose()
  )
  content(r)
}

# library(httr)
# library(dplyr)
# library(magrittr)
# library(jsonlite)
# apiKey <- "SMGfRBqmXanyL9mCigflRpJUONSbHvxFxWS1Y3Y1sms%3D"
# tableId <- "6f059e23-130b-4ec7-a186-cb4a7749c882" #Signbase2
# # tableId <- "c1753796-83fa-4a65-aaa9-fd71d56462a9" # Signbase
# category <- "Gnocchi"
#
# r <- GET(paste0("https://api.numetric.com/v3/table/", tableId),
#          add_headers("Authorization" = apiKey,
#                      "Content-Type" = "application/json"),
#          verbose()
# )
# response <- httr::content(r, as = "text") %>% fromJSON()
# response
# trannies <- response$transformations
# transformations <- trannies %>%
#   mutate(
#     sourceField = "L2DirOfTravel",
#     targetField = "L2DirOfTravel",
#     index = ifelse(index == 1, 3, 4)
#   ) %>%
#   bind_rows(trannies, .)




