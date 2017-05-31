#' @title Updates an existing Numetric dashboard (V2)
#' @description Updates the name, category, and content of an existing Numetric dashboard.
#'
#' This function uses the V2 version of the API.
#' @param apiKey You can find the API key in the settings after logging into Numetric
#' @param name The name of the dashboard, in quotes, in Numetric. Defaults to "None".
#' @param category The category, in quotes, of the dashboard. Defaults to "None".
#' @param content JSON formatted string of text. See https://nrl.readme.io/docs for more information.
#' @export
updateDashboard <- function(apiKey, datasetId, name = "None", category = "None", content){
  r <- PATCH(paste0("https://api.numetric.com/v2/dashboard/", datasetId),
             add_headers("Authorization" = apiKey,
                         "Content-Type" = "application/json"
             ),
             body = paste0(
               '{"name": "', name,'","content":', content,',"categories": ["', category, '"],"description": "Using R and API","type": "nrl"}'
             ),
             verbose()
  )
}
