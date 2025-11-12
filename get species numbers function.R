#function to return the iNaturalist ancestor taxon codes for a specific taxon code
taxon_ancestor_codes <- function(taxon_id){
  base_url <- paste0("https://api.inaturalist.org/v1/taxa/", taxon_id)
  xx <- fromJSON(content(GET(base_url), as = "text"))
  res = xx$results$ancestor_ids[[1]]
  return(res)
}


#function to get numbers of observation per species from iNat

inat_num_obs <- function (place_id = NULL, quality_grade = NULL) 
{
  require(jsonlite)
  require(httr)
  require(plyr)
  
  #get general info
  
  base_url <- paste0("https://api.inaturalist.org/v1/observations/species_counts?verifiable=true&place_id=", place_id)
  
  query <- list(
    quality_grade = quality_grade,
    place_id = place_id
  )
  
  # Remove NULL values to keep the URL clean
  query <- query[!sapply(query, is.null)]
  
  res <- GET(url = base_url, query = query) #get data from iNaturalist
  
  #check status
  if (status_code(res) != 200) {
    warning("API request failed on page ", page)
    break
  }
  
  xx <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
  
  N_sp <- xx$total_results #total number of species returned
  N_pages <- ceiling(N_sp/500) #number of json pages these results are spread across
  
  p1_res <- xx$results #page one results
  
  #tidy this p1 results output
  p1_res$taxon$place_observations_count <- p1_res$count
  
  res <- list() #output object
  
  this_res <- p1_res$taxon[,c("id", "rank", "iconic_taxon_id", "name", "is_active", "ancestry", "observations_count", "wikipedia_url", "iconic_taxon_name", "preferred_common_name", "place_observations_count")]
  
  this_res$default_photo.license_code <- p1_res$taxon$default_photo$license_code
  this_res$default_photo.attribution <- p1_res$taxon$default_photo$attribution
  this_res$default_photo.url <- p1_res$taxon$default_photo$url
  
  
  res[[1]] <- this_res
  
  #loop through remaining pages and download
  
  for (i in 2:N_pages) {
    
    print(i/N_pages)
    i_url <- paste0(base_url, "&page=",i)
    
    xx <- fromJSON(content(GET(i_url), as = "text"))
    
    i_res <- xx$results #page results
    
    #tidy this p1 results output
    i_res$taxon$place_observations_count <- i_res$count
    
    this_res <- i_res$taxon[,c("id", "rank", "iconic_taxon_id", "name", "is_active", "ancestry", "observations_count", "wikipedia_url", "iconic_taxon_name", "preferred_common_name", "place_observations_count")]
    
    this_res$default_photo.license_code <- i_res$taxon$default_photo$license_code
    this_res$default_photo.attribution <- i_res$taxon$default_photo$attribution
    this_res$default_photo.url <- i_res$taxon$default_photo$url
    
    res[[i]] <- this_res
  }
  
  #combine result into single table
  
  res <- do.call("rbind", res)
  
  return(res)
  
}

#function to return the score value for a particular species based on a particular scoring system

sp_score <- function(val, score_tab){
  
  subset(score_tab,  val <= Lower & val >= Upper)$Score
  
  
}

#function to get the number of observations for a higher taxon from and output from the inat_num_obs function
#NB this approach misses records that have been recorded to a higher taxonomic level

higher_tax_n_obs <- function(inat_num_obs_res){
  
  all_higher_taxa <- UK_N_obs$ancestry
  all_higher_taxa_list <- strsplit(all_higher_taxa, "/")
  all_higher_taxa <- unlist(all_higher_taxa_list)[!duplicated(unlist(all_higher_taxa_list))]
  
  res <- list()
  
  for (i in all_higher_taxa) {
  
  print(which(all_higher_taxa == i)/length(all_higher_taxa))  
  
  children_rows <-  inat_num_obs_res[unlist(lapply(all_higher_taxa_list, function(x){i %in% x})),]
  
  taxon_obs <- sum(children_rows$observations_count)
  taxon_obs_place <- sum(children_rows$observations_count)
  
  res[[which(all_higher_taxa == i)]] <- c(i, taxon_obs, taxon_obs_place)
  
  }
  
  res <- do.call("rbind", res)
  res <- as.data.frame(res)
  names(res) <- c("id", "observations_count", "place_observations_count")
  
  return(res)
}

#get number of observations for a specific taxon at a specific place


inat_num_obs_taxon <- function(taxon_id, place_id, project_id = NULL, quality_grade = NULL){
  
  require(httr)
  require(jsonlite)
  require(dplyr)
  
  
  base_url <- "https://api.inaturalist.org/v1/observations"
  
  query <- list(
    taxon_id = taxon_id,
    project_id = project_id,
    quality_grade = quality_grade,
    place_id = place_id
  )
  
  # Remove NULL values to keep the URL clean
  query <- query[!sapply(query, is.null)]
  
  res <- GET(url = base_url, query = query) #get data from iNaturalist
  
  #check status
  if (status_code(res) != 200) {
    warning("API request failed on page ", page)
    break
  }
  
  content_json <- fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE)
  
  return(content_json$total_results)
  
}


#function to get number of observations for a list of specific taxa at a specific place






