#' Retrieve longitude and latitude coordinates from ROR for institutions
#'
#' This function retrieves longitude and latitude coordinates.
#'
#' @param con connection to db
#'
#' @import DBI
#' @import dplyr
#' @import dbplyr
#' @import tidyr
#' @import httr
#' @import jsonlite
#' @return Tables "ror_coords" will be updated with n number of new rows containing metadata. If no data is retrieved for these n citations, "Unknown" will be returned in the relevant columns. 
#' @export
#' @examples
#' \dontrun{
#' # Example usage:
#' get_ror_coords(con)
#' }

get_ror_coords <- function(con){
  
  # if table doesn't exist, create it ----
  if (!dbExistsTable(con, "ror_coords")) {
    
    
    ror_coords <- data.frame(ror = as.character(),
                             longitude = as.numeric(),
                             latitude = as.numeric())
    
    dbWriteTable(con, "ror_coords", ror_coords)
    message("Created ror_coords table.")
    
  }
  
  # Gather the tables in their current state ----
  ror_coords_full <- tbl(con, "ror_coords") %>% 
    collect()
  
  # Get data to tag ----
  institutions <- dbReadTable(con, "institution_tag") %>%
    select(ror) %>%
    filter(ror != "Unknown") %>%
    distinct() %>%
    filter(!ror %in% ror_coords_full$ror)
  
  # Print number left to tag ----
  print(paste0(nrow(institutions), " records left to tag!"))
  
  if(nrow(institutions) < 1) {
    message("Done!")
    return()
  } else if(nrow(institutions) > 100) {
    message("Tagging the first 100 records...")
    institutions <- head(institutions, 100)
  } else {
    message("Tagging all remaining records...")
  }
  
  # Format ror for api call ----
  institutions$ror <- gsub("https://ror.org/", 
                           "https://api.ror.org/organizations/", 
                           institutions$ror)
  
  # Get coords in loop ----
  for (i in 1:length(institutions$ror)){
    
    # ror api call
    ror_response <- httr::GET(institutions$ror[i])
    ror_data <- httr::content(ror_response, "text")
    ror_json <- jsonlite::fromJSON(ror_data)
    
    # If data retrieved
    if (!is.null(ror_json$addresses)) {
      # Extract lng and lat
      institutions$latitude[i] <- ror_json$addresses$lat
      institutions$longitude[i] <- ror_json$addresses$lng
    }
    # If not, store NA
    else {
      institutions$latitude[i] <- NA
      institutions$longitude[i] <- NA
    }
    
  }
  
  # Format ror for database table ----
  institutions$ror <- gsub("https://api.ror.org/organizations/",
                           "https://ror.org/",
                           institutions$ror)
  
  # Append to database ----
  dbWriteTable(con, "ror_coords", institutions, append = TRUE)
  
  # Print result ----
  print(paste0(sum(complete.cases(institutions)), " out of ",nrow(institutions)," records tagged with coordinates."))
}