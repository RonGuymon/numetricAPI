#' @title Creates a new dashboard in Numetric (V2)
#' @description Creates a new dashboard in Numetric.
#'
#' This function uses the V2 version of the API.
#' @param apiKey You can find the API key in the settings after logging into Numetric
#' @param name The name of the dashboard, in quotes, in Numetric. Defaults to "None".
#' @param content JSON formatted string of text. See https://nrl.readme.io/docs for more information.
#' @export
createDashboard <- function(apiKey, name = "None", category = "None", content){
  r <- POST("https://api.numetric.com/v2/dashboard/",
            add_headers("Authorization" = apiKey,
                        "Content-Type" = "application/json"
            ),
            body = paste0(
              '{"name": "', name,'","content":', content,',"categories": ["', category, '"],"description": "Using R and API","type": "nrl"}'
            ),
            verbose()
  )
}
