#' @title Gets the JSON for a Numetric dashboard (V2)
#' @description Returns the JSON for a Numetric Dashboard
#'
#' This function uses the V2 version of the API.
#' @param apiKey You can find the API key in the settings after logging into Numetric
#' @param dashboardId The dashboard ID, in quotes. You can find the dashboard ID by either (1) using the getDashboards function, or (2) navigating to the dashboard, and then selecting the string after the last forward slash.
#' @export
getDashboard <- function(apiKey, dashboardId){
  r <- GET(paste0("https://api.numetric.com/v2/dashboard/", dashboardId),
           add_headers("Authorization" = apiKey,
                       "Content-Type" = "application/json"),
           verbose()
  )
}
