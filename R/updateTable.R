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

updateTable <- function(apiKey, tableId, numetricName = "absent__default", description = "absent__default", dataframeName = "absent__default", category = "absent__default", primaryKey = "absent__default", geoshapes = "", geopoints = "", boolean = "", transformations = "absent__default"){
  # Example: nApiCreateTableV3(apiKey = apiKey, numetricName = "Retail Sports",dataframeName = sport2Sample,category = "Retail Sports", primaryKey="primaryKey")
  # Be sure to have a field in the dataframe that is a unique value for each row.

  if(numetricName == "absent__default" | dataframeName == "absent__default" | primaryKey == "absent__default"){
    r <- GET(paste0("https://api.numetric.com/v3/table/", tableId),
             add_headers("Authorization" = apiKey,
                         `Content-Type` = "application/json")
             # ,
             # verbose()
    )
    response <- httr::content(r, as = "text") %>% # Saves what was returned as raw text with all the encodings
      fromJSON()
  }

  if(class(dataframeName) == "character"){
    wholeString <- ""
    for(i in 1:length(response$fields)){
      a <- response$fields[i]
      a <- unlist(a)
      tempString <- paste0('"',a[1],'": {"displayName": "',a[1] ,'","type": "',a[3],'"},')

      wholeString <- paste0(wholeString,tempString)
      if(i == length(response$fields)){
        wholeString <- gsub(",$","",wholeString)
      }
    }


    fieldAttributesReady_body <- paste0('"fields": {',wholeString,'}')
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

  if(numetricName == "absent__default"){
    numetricName_body <- paste0('"name": "', response$name, '"')
  }else{
    numetricName_body <- paste0('"name": "', numetricName, '"')
  }

  if(primaryKey == "absent__default"){
    primaryKey_body <- paste0('"primaryKey": ["', response$primaryKey,'"]')
  }else{
    primaryKey_body <- paste0('"primaryKey": ["', primaryKey, '"]')
  }

  if(category == "absent__default"){
    category_body <- '"deleteMe"'
  }else{
    category_body <- paste0('"categories": ["', category, '"]')
  }

  if(description == "absent__default"){
    description_body <- '"deleteMe"'
  }else{
    description_body <- paste0('"description": "', description, '"')
  }

  if(class(transformations) == "character"){
    transformations_body <- '"deleteMe"'
  }else{
    transformations_body <- paste0('"transformations":', toJSON(transformations)) %>%
      gsub('"values":\\{\\},', '',.)
  }

  ## Merge the field attributes with the other information required----
  metadata <- paste(numetricName_body,
                    primaryKey_body,
                    category_body,
                    description_body,
                    transformations_body,
                    fieldAttributesReady_body,
                    sep = ",") %>%
    gsub('"deleteMe",|"deleteMe"|,"deleteMe"', '', .) %>%
    # gsub('"order":\\[\\d{1,}\\],', '', .) %>%
    paste0('{', ., '}')
# write(metadata, "metadata")
  # Index the data----
  r <- PATCH(paste0("https://api.numetric.com/v3/table/", tableId),
            add_headers("Authorization" = apiKey,
                        `Content-Type` = "application/json"),
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
 # tableId2 <- "6f059e23-130b-4ec7-a186-cb4a7749c882" #Signbase2
 # tableId <- "6f059e23-130b-4ec7-a186-cb4a7749c882" #Signbase2
 # # tableId1 <- "c1753796-83fa-4a65-aaa9-fd71d56462a9" # Signbase
 # tableId1 <- "e1518ea6-f121-4c27-bb5a-8437d181ef58" # Airspace
 # category <- "Gnocchi"
 #
 # r <- GET(paste0("https://api.numetric.com/v3/table/", tableId1),
 #          add_headers("Authorization" = apiKey,
 #                      "Content-Type" = "application/json"),
 #          verbose()
 # )
 # response <- httr::content(r, as = "text") %>% fromJSON()
 # response
 # trannies <- response$transformations %>% .[21,1:10]
 # transformations <- trannies %>%
 #   mutate(
 #     sourceField = "CityTownVillage",
 #     targetField = "CityTownVillage",
 #     index = 1
 #   )
 #
 # r <- GET(paste0("https://api.numetric.com/v3/table/", tableId,"/publish"),
 #          add_headers("Authorization" = apiKey),
 #          verbose()
 # )
 #
