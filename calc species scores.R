#code to create scoring system for a UK species list based on observation numbers

source("get species numbers function.R")

#get UK species observation numbers

UK_N_obs <- inat_num_obs("214126,214128,214130,214369,214245,214248,214370,214412,214413,214247")

plot(UK_N_obs$UK_observations_count)

hist(UK_N_obs$UK_observations_count, breaks = 100)

N_breaks <- quantile(UK_N_obs$UK_observations_count, probs = (1:11)/11)[-11]

#replace the first five breaks with 1:5 to give more variation in scores

N_breaks[1:6] <- 1:6

scoring_table <- data.frame("Score" = 10:1, "Upper" = N_breaks, "Lower" = c(N_breaks[-1] - 1, 1/0))

UK_N_obs$Score <- unlist(lapply(UK_N_obs$UK_observations_count, FUN =sp_score, score_tab = scoring_table))

hist(UK_N_obs$Score)

#get higher taxonomic groups

high_obs <- higher_tax_n_obs(UK_N_obs)


#find numbers of species records for higher taxa
sum(all_higher_taxa %in% UK_N_obs$id)

