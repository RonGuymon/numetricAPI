#' @title Get the details of a specific table (V3)
#' @description Returns a one row dataframe with meta data about the table of interest. This function uses the V3 version of the API.
#' @param apiKey You can find the API key in the settings after logging into Numetric
#' @param tableId The id of the table. You can find this by using the getTables function, or by navigating to the table, and then selecting the string after the last forward slash of the url.
#' @param fieldNames By default it returns the display name, order, and type of all the fields. This can make the table very wide. If set to false, this field information will not be returned.
#' @return A list of attributes: accessedAt, accessedBy, acl, categories, createdAt, description, ext, id, name, org, updatedAt, updatedBy, version, ownerInfo, updatedByInfo, fields, primaryKey, transformations, active, stats, status, sourceId, options, schedule
#' @export
getTableDetails <- function(apiKey, tableId, fieldNames = T, textOrJSON = "JSON"){
  r <- GET(paste0("https://api.numetric.com/v3/table/", tableId),
           add_headers("Authorization" = apiKey,
                       "Content-Type" = "application/json"),
           verbose()
  )
  if(textOrJSON == "JSON"){
    response <- httr::content(r, as = "text") %>% # Saves what was returned as raw text with all the encodings
      fromJSON() # Converts what was returned to a list or a dataframe
  }else {
    response <- httr::content(r, as = "text") # Saves what was returned as raw text with all the encodings
  }
  return(response)
  # r2 <- list.flatten(response) %>% as.data.frame()
  # colnames(r2) <- gsub("\\.", "_", colnames(r2))
  # if(fieldNames == F){
  #   r2 <- r2[,which(grepl("fields_", x = colnames(r2)) == F)]
  # }
  # return(r2)
}






