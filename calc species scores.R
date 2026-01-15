#code to create scoring system for a UK species list based on observation numbers

source("get species numbers function.R")

#full UK based

UK_N_obs <- inat_num_obs("6857") # iNat UK 'place' 

#create 9 scoring bins for all the species with more than one record
N_breaks <- quantile(UK_N_obs$place_observations_count[!UK_N_obs$place_observations_count == 1], probs = (1:10)/10)[-10]

#add the single record scoring bin
N_breaks <- c(1,round(N_breaks))

scoring_table <- data.frame("Score" = 10:1, "Lower" = N_breaks, "Upper" = c(N_breaks[-1] - 1, 1/0))

UK_N_obs$Score <- unlist(lapply(UK_N_obs$place_observations_count, FUN =sp_score, score_tab = scoring_table))

# results bar plot
species_score_bp(UK_N_obs, scoring_table)

#difference from 2024 version
load("2024_UK_Species_Scores.RData")

UK_based_scores2024 <- UK_N_obs2024$Score
names(UK_based_scores2024) <- as.character(UK_N_obs2024$id)

UK_N_obs$Score2024 <- UK_based_scores2024[as.character(UK_N_obs$id)]

UK_N_obs$Diff1 <- UK_N_obs$Score - UK_N_obs$Score2024

UK_N_obs_nona <- subset(UK_N_obs, !is.na(Diff1))


head(UK_N_obs_nona[order(UK_N_obs_nona$Diff1),c("name", "Score", "Score2024")])

subset(UK_N_obs_nona, name == "Carex oederi")
subset(UK_N_obs2024, name == "Carex oederi")


subset(UK_N_obs_nona, name == "Babakina anadoni")
subset(UK_N_obs2024, name == "Babakina anadoni")

tail(UK_N_obs_nona[order(UK_N_obs_nona$Diff1),c("name", "Score", "Score2024")])



subset(UK_N_obs_nona, name == "Diplocephalus cristatus")
subset(UK_N_obs2024, name == "Diplocephalus cristatus")


#get UK species coastal observation numbers

UK_inat_coastal_place_ids <- "214126,214128,214130,214369,214245,214248,214370,214412,214413,214247"

UK_coast_N_obs <- inat_num_obs(UK_inat_coastal_place_ids)

#create 9 scoring bins for all the species with more than one record
N_breaks <- quantile(UK_coast_N_obs$place_observations_count[!UK_coast_N_obs$place_observations_count == 1], probs = (1:10)/10)[-10]

#add the single record scoring bin
N_breaks <- c(1,round(N_breaks))

scoring_table <- data.frame("Score" = 10:1, "Lower" = N_breaks, "Upper" = c(N_breaks[-1] - 1, 1/0))

UK_coast_N_obs$Score <- unlist(lapply(UK_coast_N_obs$place_observations_count, FUN =sp_score, score_tab = scoring_table))

# results bar plot
species_score_bp(UK_coast_N_obs, scoring_table)

#what are the most different species between the two systems
UK_based_scores <- UK_N_obs$Score
names(UK_based_scores) <- as.character(UK_N_obs$id)

UK_coast_N_obs$ukbasedScore <- UK_based_scores[as.character(UK_coast_N_obs$id)]

UK_coast_N_obs$Diff1 <- UK_coast_N_obs$Score - UK_coast_N_obs$ukbasedScore

head(UK_coast_N_obs[order(UK_coast_N_obs$Diff1),c("name", "Score", "ukbasedScore")])

100 * (subset(UK_coast_N_obs, name == "Portumnus latipes")$place_observations_count/nrow(UK_coast_N_obs))
100 * (subset(UK_N_obs, name == "Portumnus latipes")$place_observations_count/nrow(UK_N_obs))

UK_coast_N_obs_nona <-  subset(UK_coast_N_obs, !is.na(Diff1))

tail(UK_coast_N_obs_nona[order(UK_coast_N_obs_nona$Diff1),c("name", "Score", "ukbasedScore", "Diff1")])

100 * (subset(UK_coast_N_obs, name == "Cnephaeus serotinus")$place_observations_count/nrow(UK_coast_N_obs))
100 * (subset(UK_N_obs, name == "Cnephaeus serotinus")$place_observations_count/nrow(UK_N_obs))

#common species in coastal areas with no inland records
common_coastal_species <- subset(UK_coast_N_obs, Score == 10)$name

common_coastal_species_not_inland <- common_coastal_species[!common_coastal_species %in% UK_N_obs$name]

#get obs numbers higher taxonomic groups

all_higher_taxa <- UK_coast_N_obs$ancestry
all_higher_taxa_list <- strsplit(all_higher_taxa, "/")
all_higher_taxa <- unlist(all_higher_taxa_list)[!duplicated(unlist(all_higher_taxa_list))]

reloadhighertaxadat <- FALSE

if (reloadhighertaxadat) {
  
chunks <- split(all_higher_taxa, ceiling(seq_along(all_higher_taxa) / 500))

if(file.exists("higher_taxa_counts")){
  load("higher_taxa_counts")
}else{
  higher_taxa_counts <- list()
}

chunks <- chunks[-(1:length(higher_taxa_counts))]

count = 1 + length(higher_taxa_counts)


for (chunk in chunks) {

output <- inat_num_obs_taxa(chunk, place_id = UK_inat_coastal_place_ids)

  higher_taxa_counts[[count]] <- output
  
  count = count + 1
  
  save(higher_taxa_counts, file = "higher_taxa_counts")
  Sys.sleep(60)
}

higher_taxa_counts_df <- do.call("rbind", higher_taxa_counts)

mean(higher_taxa_counts_df$taxon_id %in% all_higher_taxa)

rem_taxa <- all_higher_taxa[!all_higher_taxa %in% higher_taxa_counts_df$taxon_id]

higher_taxa_counts_df$taxon_id[!higher_taxa_counts_df$taxon_id %in% all_higher_taxa]


mis_output <- inat_num_obs_taxa(rem_taxa)

higher_taxa_counts_df <- rbind(higher_taxa_counts_df, mis_output)

save(higher_taxa_counts_df, file = "higher_taxa_counts_df.Rdata")
}else{
  load("higher_taxa_counts_df.Rdata")
}

#add species level data

sp_dat <- UK_N_obs[,c("id", "place_observations_count")]

names(sp_dat) <- names(higher_taxa_counts_df)

all_taxa_obs <- rbind(higher_taxa_counts_df, sp_dat)

#remove taxa with 0 records 
all_taxa_obs <- subset(all_taxa_obs, !observations_count == 0)

hist(all_taxa_obs$observations_count[all_taxa_obs$observations_count < 10])

#create 10 scoring bins for all the species with more than one record
N_breaks <- quantile(higher_taxa_counts_df$observations_count, probs = (1:10)/10)

#add the single record scoring bin
N_breaks <- round(N_breaks)

scoring_table <- data.frame("Score" = 10:1, "Lower" = c(1, N_breaks[-10] + 1), "Upper" = c(N_breaks[-10], 1/0))

all_taxa_obs$Score <- unlist(lapply(all_taxa_obs$observations_count, FUN =sp_score, score_tab = scoring_table))

#try a version ranging from 1 to 100
#create 10 scoring bins for all the species with more than one record
N_breaks <- quantile(higher_taxa_counts_df$observations_count, probs = (1:100)/100)

#add the single record scoring bin
N_breaks <- round(N_breaks)

scoring_table <- data.frame("Score" = 100:1, "Lower" = c(1, N_breaks[-100] + 1), "Upper" = c(N_breaks[-100], 1/0))

all_taxa_obs$Score100 <- unlist(lapply(all_taxa_obs$observations_count, FUN =sp_score, score_tab = scoring_table))


# results bar plot
species_score_bp(all_taxa_obs, scoring_table)

save(all_taxa_obs, file= "all_taxa_obs.RData")

subset(all_taxa_obs, taxon_id == 52523)

#try new system out
#get all inat data for a project - Castle Beach Nov 2025 - 258023
source("new get project obs function.R")

castlebeach_dat <- get_inat_obs_project_v2("258023")

observer_dat <- subset(castlebeach_dat, !duplicated(user.login))

all_observer_names <- observer_dat$user.name
all_observer_names[is.na(all_observer_names)] <- observer_dat$user.login[is.na(all_observer_names)]

names(all_observer_names) <- observer_dat$user.login

obs_taxa_list <- list()

for (observer in names(all_observer_names)) {
  observer_dat <- subset(castlebeach_dat, user.login == observer & !is.na(community_taxon_id))
  
  all_taxa_com_id <- observer_dat$community_taxon_id
  
  internal_nodes <- lapply(1:nrow(observer_dat), function(x){
    
    com_id_pos <- which(observer_dat$taxon.ancestor_ids[[x]] == all_taxa_com_id[x])
    
    nodes <- observer_dat$taxon.ancestor_ids[[x]][1:com_id_pos]
    
    internal_nodes <- nodes[-length(nodes)]
    
    internal_nodes
  })
  
  internal_nodes <- unique(unlist(internal_nodes))
  
  all_taxa_com_id <- unique(all_taxa_com_id)
  all_taxa_com_id <- all_taxa_com_id[!all_taxa_com_id %in% internal_nodes]
  
  obs_res <- subset(observer_dat, community_taxon_id %in% all_taxa_com_id)[,c("community_taxon_id", "taxon.rank")]
  
  obs_res <- subset(obs_res, !duplicated(community_taxon_id))
  
  obs_res$Score_2024 <- UK_N_obs2024$Score[match(obs_res$community_taxon_id, UK_N_obs2024$id)]
  obs_res$Score_sp_v1 <- UK_N_obs$Score[match(obs_res$community_taxon_id, UK_N_obs$id)]
  obs_res$Score_sp_v2 <- UK_coast_N_obs$Score[match(obs_res$community_taxon_id, UK_coast_N_obs$id)]
  obs_res$Score_taxa <- all_taxa_obs$Score[match(obs_res$community_taxon_id,all_taxa_obs$taxon_id)]
  
  obs_scores <- c(sum(obs_res$Score_2024, na.rm = T), sum(obs_res$Score_sp_v1, na.rm = T), sum(obs_res$Score_sp_v2, na.rm = T), sum(obs_res$Score_taxa, na.rm = T))
  names(obs_scores) <- c("Score_2024","Score_sp1", "Score_sp2", "Score_taxa")
  
  obs_allres <-list(obs_res, obs_scores, all_taxa_com_id)
  names(obs_allres) <- c("taxa_list", "Total_scores", "Taxa_ids")
  
  obs_taxa_list[[observer]] <- obs_allres
}

#league tables

names(obs_taxa_list) <- all_observer_names

all_res <- do.call("rbind",lapply(obs_taxa_list, function(x){x$Total_scores}))
all_res <- as.data.frame(all_res)

all_res[order(all_res$Score_2024, decreasing = T),]
all_res[order(all_res$Score_sp1, decreasing = T),]
all_res[order(all_res$Score_sp2, decreasing = T),]
head(all_res[order(all_res$Score_taxa, decreasing = T),])

#get taxa info

if(file.exists("all_taxa_info.Rdata")){
  load("all_taxa_info.Rdata")
}else{
  all_taxa_info <- inat_taxa_dets(all_taxa_obs$taxon_id)
  
  save(all_taxa_info, file = "all_taxa_info.Rdata")
}

castle_beach_verif_dat <- subset(castlebeach_dat, !is.na(community_taxon_id))


all_castle_beach_taxa <- castle_beach_verif_dat$community_taxon_id


internal_nodes <- lapply(1:nrow(castle_beach_verif_dat), function(x){
  
  com_id_pos <- which(castle_beach_verif_dat$taxon.ancestor_ids[[x]] == all_castle_beach_taxa[x])
  
  nodes <- castle_beach_verif_dat$taxon.ancestor_ids[[x]][1:com_id_pos]
  
  internal_nodes <- nodes[-length(nodes)]
  
  internal_nodes
})

castle_beach_leaftaxa <- unique(all_castle_beach_taxa[!all_castle_beach_taxa %in% unlist(internal_nodes)])

castlebeach_event_taxa_list <- subset(all_taxa_obs, taxon_id %in% castle_beach_leaftaxa)

castlebeach_event_taxa_list_info <- subset(all_taxa_info, id %in% castle_beach_leaftaxa)


castlebeach_event_taxa_list$name <- castlebeach_event_taxa_list_info$name[match(castlebeach_event_taxa_list_info$id, castlebeach_event_taxa_list$taxon_id)]
castlebeach_event_taxa_list$common <- castlebeach_event_taxa_list_info$preferred_common_name[match(castlebeach_event_taxa_list_info$id, castlebeach_event_taxa_list$taxon_id)]


table(castlebeach_event_taxa_list$Score)

apricots_taxa <- obs_taxa_list$apricotleaf$taxa_list
apricots_taxa$name <- all_taxa_info$name[match(apricots_taxa$community_taxon_id, all_taxa_info$id)]

subset(apricots_taxa, !taxon.rank == "species")
