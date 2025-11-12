get_inat_obs_project_ben <- function (grpid, type = c("observations", "info"), raw = FALSE) 
{
  if (!curl::has_internet()) {
    message("No Internet connection.")
    return(invisible(NULL))
  }
  base_url <- "http://www.inaturalist.org/"
  if (httr::http_error(base_url)) {
    message("iNaturalist API is unavailable.")
    return(invisible(NULL))
  }
  argstring <- switch(match.arg(type), observations = "obs", 
                      info = "info")
  url <- paste0(base_url, "projects/", grpid, ".json")
  require(jsonlite)
  require(httr)
  require(plyr)
  xx <- fromJSON(content(GET(url), as = "text"))
  recs <- xx$observed_taxa_count
  dat <- NULL
  if (is.null(recs)) 
    (return(dat))
  message(paste(recs, "records\n"))
  if (argstring == "info") {
    output <- list()
    output[["title"]] <- xx$title
    output[["description"]] <- xx$description
    output[["slug"]] <- xx$slug
    output[["created_at"]] <- xx$created_at
    output[["id"]] <- xx$id
    output[["location"]] <- c(as.numeric(xx$lat), as.numeric(xx$long))
    output[["place_id"]] <- xx$place_id
    output[["taxa_number"]] <- xx$observed_taxa_count
    output[["taxa_count"]] <- xx$project_observations_count
    if (raw) {
      output[["raw"]] <- xx
    }
    return(output)
  }
  else if (argstring == "obs") {
    per_page <- 200
    
    obs_list <- list()
    i = 1
    repeat{
      
      url1 <- paste0(base_url, "observations/project/", 
                     grpid, ".json?page=", i, "&per_page=", per_page)
      if (i == 1) {
        message(paste0("Getting records 0-", per_page))
      }
      if (i > 1) {
        message(paste0("Getting records up to ", i * 
                         per_page))
      }
      res <- fromJSON(content(GET(url1), as = "text"), 
                      flatten = TRUE)
      if(length(res) == 0){
        break
      }
      obs_list[[i]] <- res
      i = i + 1
    }
    message("Done.\n")
    project_obs <- do.call("rbind.fill", obs_list)
    return(project_obs)
  }
}

inat_num_obs <- function (taxon_name = NULL, place_id = NULL, maxresults = 1, 
                          meta = TRUE) 
{
  require(jsonlite)
  require(httr)
  require(plyr)
  
  
  base_url <- "http://www.inaturalist.org/"
  ping_path <- "observations.json"
  search <- paste0("&taxon_name=", gsub(" ", "+", taxon_name))
  search <- paste0(search, "&place_id=", gsub(" ", "+", place_id))
  ping_query <- paste0(search, "&per_page=1&page=1")
  ping <- GET(base_url, path = ping_path, query = ping_query)
  total_res <- as.numeric(ping$headers$`x-total-entries`)
  
  return(total_res)
}

#function to get the iNat main species image

get_image_link <- function(x){
  base_url <- paste0("http://www.inaturalist.org/taxa/",x,".json")
  
  require(jsonlite)
  require(httr)
  require(plyr)
  xx <- fromJSON(content(GET(base_url), as = "text"))
  
  return(xx$default_photo$medium_url)
}

