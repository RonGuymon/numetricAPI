#' @title Create a new table (V3)
#' @description Create a new table in a Numetric Org.
#'
#' This function uses the V3 version of the API.
#' @param apiKey You can find the API key in the settings after logging into Numetric
#' @param numetricName The name, in quotes, given to the dataset in the Numetric Warehouse.
#' @param dataframeName The name, without quotes, of the dataframe that will be used to create the dataset in Numetric.
#' @param category The category, in quotes, given to the dataset in the Numetric Warehouse.
#' @param primaryKey A vector with the column name(s), in quotes, that will be used as the primary key. Each row should have a unique id, otherwise, the last row indexed will be the one saved.
#' @param geoshapes The column name, or vector of column names, in quotes, that will be stored as a geoShape (point on a map). This needs to be in the format, "lat,long".
#' @param geopoints The column name, or vector of column names, in quotes, that will be stored as a geoPoint (for heatmaps). This needs to be in the format, "lat,long".
#' @param boolean The column name, or vector of column names, in quotes, that will be stored as True/False. True = 1, False = 0.
#' @param everyone Defaults to "false". If set to "true", then it will allow everyone in the org to see the dataset.
#' @return Numetric Id
#' @export
createTable <- function(apiKey, numetricName, dataframeName, category = "New Data", primaryKey, geoshapes = "", geopoints = "", boolean = "", everyone = "false"){
  # Example: nApiCreateTableV3(apiKey = apiKey, numetricName = "Retail Sports",dataframeName = sport2Sample,category = "Retail Sports", primaryKey="primaryKey")
  # Be sure to have a field in the dataframe that is a unique value for each row.

  ## Create a dataframe with column attributes----
  # Create a dataframe with columns for each field: field, displayName, autocomplete, and type.
  fieldAttributes <- data.frame(field = colnames(dataframeName), # Gets the column names of the dataset to index
                                displayName = colnames(dataframeName), # Uses the column names of the dataset as the display names
                                type = array(lapply(dataframeName, class)) %>% as.character() # Gets the class of each column in the dataframe
  )
  # Convert r class types to valid numetric class types
  fieldAttributes <- mutate(fieldAttributes, type = ifelse(type == "character", "string",
                                                           ifelse(type == "numeric", "double",
                                                                  ifelse(type == "integer", "integer",
                                                                         ifelse(type == "Date" | type == "POSIXct" | type == "POSIXlt", "datetime", "string")))))

  # Add in the fields that should be geocoded and booleans
  fieldAttributes <- mutate(fieldAttributes,
                            type = ifelse(field %in% geoshapes, "geo_shape",
                                          ifelse(field %in% geopoints, "geo_point", type)),
                            type = ifelse(grepl("date", field, ignore.case = T), "datetime", type),
                            type = ifelse(field %in% boolean, "boolean", type)
  )
  wholeString <- ""
  for (i in 1:nrow(fieldAttributes)){

    tempString <- paste('"',fieldAttributes[i,1],'": {"displayName": "',fieldAttributes[i,2] ,'","type": "',fieldAttributes[i,3],'"},',  sep="")

    wholeString <- paste(wholeString,tempString,sep="")
    if(i == nrow(fieldAttributes)){
      wholeString <- gsub(",$","",wholeString)
    }

  }

  fieldAttributesReady <- paste0("{",wholeString,"}")

  ## Merge the field attributes with the other information required----
  metadata <- paste0('{"name": "', numetricName,
                    '","primaryKey": ["', primaryKey,'"],
                    "categories": ["', category,
                    '"],"description": "Stuff indexed through R","fields":',
                    fieldAttributesReady,
                    '}')

  r <- POST("https://api.numetric.com/v3/table",
            add_headers("Authorization" = apiKey,
                        "Content-Type" = "application/json"),
            body = metadata,
            verbose()
  )
  httr::content(r)
}

