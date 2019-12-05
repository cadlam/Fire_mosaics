# This loads all the data (some additional manipulation required for plants to select native species, spcific growth forms, or to change to p/a)

# Load packages
if(!require('pacman'))install.packages('pacman')
pacman::p_load(tidyverse, emmeans, ggplot2, cowplot, vegan, data.table, kableExtra, dplyr, plyr, nloptr, labdsv, betapart, lattice, rowr, plotly, spdep, bbmle, rsq, lmtest, broom, lmerTest, lme4, emmeans)

## Site
site_data2018 <- read.csv("/Users/christopheradlam/Desktop/Davis/R/GitHub Repos/Fire_mosaics/Data/site_data.csv") 
site_data2019 <- read.csv("/Users/christopheradlam/Desktop/Davis/R/GitHub Repos/Fire_mosaics/Data/site_data2019.csv") 
site_data2018$site_id <- as.factor(site_data2018$site_id)
site_data2018$tsf <- as.numeric(as.character(site_data2018$tsf))
site_data2018$fire_yr <- as.numeric(as.character(site_data2018$fire_yr))
#site_data2019$tsf <- as.numeric(as.character(site_data2019$tsf))
#site_data_read$fire_yr <- as.numeric(as.character(site_data2019$fire_yr))
site_data_read <- full_join(site_data2018, site_data2019)

# here I'm setting the TSF categories; change as necessary (tsf_cat = 3 age categories; tsf_2cat = 2 categories)
site_data <- site_data_read %>% 
  mutate(tsf_cat = ifelse(is.na(tsf), "4", "3")) %>% 
  mutate(tsf_2cat = ifelse(is.na(tsf), "3", "2"))

site_data$tsf_cat[site_data$tsf < 17] <-2
site_data$tsf_cat[site_data$tsf < 10] <-1

site_data$tsf_2cat[site_data$tsf < 15] <-1

site_data <- site_data_read %>% 
  mutate(tsf_cat = ifelse(is.na(tsf), "5", "4")) %>% 
  mutate(tsf_2cat = ifelse(is.na(tsf), "3", "2"))

site_data$tsf_cat[site_data$tsf < 17] <-3
site_data$tsf_cat[site_data$tsf < 13] <-2
site_data$tsf_cat[site_data$tsf < 6] <-1

site_data$tsf_2cat[site_data$tsf < 15] <-1

site_data <- site_data %>% 
  mutate(sev_tsf = paste(sev, tsf_cat, sep = "-"))  %>% 
  mutate(sev_tsf2 = paste(sev, tsf_2cat, sep = "-"))

site_data$site_id <- as.character(site_data$site_id) 

site_data <- site_data[-(111:114),] %>%# remove the LICH sites 
mutate(cov_cat = ifelse(tree_cov > 65, 3, ifelse(tree_cov > 30, 2, 1))) %>% # creating tree cover categories   
mutate(sev_cov = ifelse(sev == "u", "u", ifelse(sev == "h", "h", ifelse(sev =="Rx", paste("RX/mult", cov_cat, sep = "-"), ifelse(sev == "multiple", paste("RX/mult", cov_cat, sep = "-"),  ifelse(sev == "multiple/RX", paste("RX/mult", cov_cat, sep = "-"), paste(sev, cov_cat, sep = "-")))))))

#mutate(sev_cov = paste(sev, cov_cat, sep = "-")) %>% 


## Plants
plant_data <- read.csv("/Users/christopheradlam/Desktop/Davis/R/GitHub Repos/Fire_mosaics/Data/plant_data.csv")
plant_names <- read.csv("/Users/christopheradlam/Desktop/Davis/R/GitHub Repos/Fire_mosaics/data/plant_list.csv")
plant_data$species <- as.character(plant_data$species)
plant_data$site_id <- as.character(plant_data$site_id)
plant_names$species <- as.character(plant_names$species)
plant_names$full_name <- as.character(plant_names$full_name)
plant_names$native_status <- as.factor(plant_names$native_status)
plant_names$form <- as.factor(plant_names$form)

# CHECK FOR TYPOS - ALL GOOD
# missing <- anti_join(plant_data, plant_names, by = "species")

### Keep only native spp
## Also for some ecologically similar and taxonomically related plant species, use genus 
plant_dat <- left_join(plant_data, plant_names, by = "species") %>%
  filter((native_status == "native")) %>% # change if wanting to use subset of species, eg just woody spp, or grass, etc. eg. "& (form == "herb" | form == "grass")""
  mutate(species = ifelse(genus == "Lupinus", "Lupinus", species)) %>% 
  mutate(species = ifelse(genus == "Arctostaphylos", "Arctostaphylos", species)) %>% 
  mutate(species = ifelse(genus == "Penstemon", "Penstemon", species)) %>% 
  mutate(species = ifelse(genus == "Sidalcea", "Sidalcea", species)) %>% 
  mutate(species = ifelse(genus == "Iris", "Iris", species)) %>% 
  mutate(species = ifelse(genus == "Nemophila", "Nemophila", species)) %>% 
  mutate(species = ifelse(genus == "Agoseris", "Agoseris", species)) %>% 
  mutate(species = ifelse(genus == "Collinsia", "Collinsia", species)) %>% 
  mutate(species = ifelse(genus == "Clarkia", "Clarkia", species)) %>% 
  mutate(species = ifelse(genus == "Pyrola", "Pyrola", species)) %>% 
  mutate(species = ifelse(genus == "Ribes", "Ribes", species)) %>% 
  mutate(species = ifelse(genus == "Piperia", "Piperia", species)) %>% 
  mutate(species = ifelse(genus == "Platanthera", "Piperia", species)) %>% 
  mutate(species = ifelse(genus == "Madia", "Madia", species)) %>% 
  mutate(species = ifelse(genus == "Cirsium", "Cirsium", species))  %>% 
  dplyr::select(site_id, species, cover)

# "mat" is only species data; "dat" also includes site data if in wide format (w), or not if in long format (l)
#Presence/abs
plant_mat_pa_w1 <- splist2presabs(plant_dat, sites.col = 1, sp.col = 2)  # same as plant_matrix2
  
plant_mat_pa_w <- plant_mat_pa_w1[,-1] %>% 
  dplyr::select(which(colSums(.) > 0)) %>%  # choose threshold for number of detections
  cbind(plant_mat_pa_w1[,"site_id"], .)

names(plant_mat_pa_w)[1] <- "site_id" # having to rename first column because the code somehow changed it

plant_dat_pa_w <- plant_mat_pa_w %>% 
  left_join(., site_data, by ="site_id") %>% 
  mutate(cov_cat = ifelse(tree_cov > 65, 3, ifelse(tree_cov > 30, 2, 1)))  # creating tree cover categories

#plant_dat_pa_w %>% 
#  group_by(cov_cat) %>% 
#  tally()

plant_dat_pa_l <- plant_mat_pa_w %>% 
  pivot_longer(names_to = "species", values_to = "pa", cols = -site_id) 

# Cover
plant_dat_long <-ddply(plant_dat, .(site_id, species), summarize,
                       cover = sum(cover)) # needed to combine cover values where species were lumped into the same genus

plant_mat_cov_w <- spread(data = plant_dat_long, key = species, value = cover, fill = 0) # same as plant_matrix1

# The following is to remove species detected too few times
# convert to wide format for following analysis
#plant_matrix1 <- spread(data = plant_dat, key = species, value = cover, fill = 0)

# adding dummy species (eg. Webster 2010)
#plant_matrix <- plant_matrix %>% 
#  mutate("DUMB" = "0.5") 

## adding back in the site id and removing true/false row 
#(and making sure it's all read in as numeric)
#plant_mat_cov_w <- plant_matrix[-111, ]  #for only 2018 data, use change 111 to 49; this used to be plant_dat_cov, same as plant_mrpp_d;  %>% mutate_if(is.character, as.numeric)

plant_dat_cov_w <- left_join(plant_mat_cov_w, site_data, by = "site_id") #this used to be plant_dat_cov, same as plant_mrpp_d

plant_dat_cov_l <- plant_mat_cov_w %>% 
  pivot_longer(names_to = "species", values_to = "pa", cols = -site_id)




## Lichens
lichen_names <-read.csv("/Users/christopheradlam/Desktop/Davis/R/GitHub Repos/Fire_mosaics/Data/lichen_list.csv")

lichen_mat_species_w <- read.csv("data/lichen_data.csv", header = T) %>% 
  mutate(site_id = as.factor(site_id)) %>% 
  dplyr::select(site_id, species, abund) %>% 
  spread(key = species, value = abund, fill = 0) %>% 
  mutate("DUMB" = 1)  # adding dummy species (eg. Webster 2010) 

# for genus level analysis (presence/absence)
lichen_dat_genus_l <- lichen_mat_species_w %>% 
  gather(key=species, value=abund, AHPA:DUMB) %>% 
  left_join(., lichen_names, by = "species") %>% 
  dplyr::select(site_id,genus,abund) 

# remove duplicate rows (same species detected multiple times in a single plot)
lichen_dat_genus_pa1 <- lichen_dat_genus_l %>% 
  mutate(pa = ifelse(abund == 0, 0, 1)) %>% 
  dplyr::select(site_id, genus, pa)

#spread not working.
lichen_dat_genus_pa_l <- unique(lichen_dat_genus_pa1[ , c(1:3)]) %>%
  filter(pa==1) %>% 
  setnames(., old="genus", new="species")


lichen_mat_genus_pa_w <- lichen_dat_genus_pa_l %>% 
  spread(key = species, value = pa, fill = 0) %>% 
  dplyr::select(c(site_id:Xanthomendoza)) #%>% 
  #mutate("DUMB" = 1) # need dummy sp?

lichen_dat_genus_pa_w <- lichen_mat_genus_pa_w %>% 
  left_join(., site_data, by ="site_id")





## Insects
insect_dat1 <- read.csv("data/insect_data.csv", header = T) %>% 
  mutate(order = as.character(order)) %>% 
  mutate(suborder = as.character(suborder))

insect_dat2 <- insect_dat1 %>% 
  mutate(site_id = as.factor(site_id)) %>% 
  #  gsub(" ", "", order) %>% #get rid of spaces in some cells
  filter(family != "Curculionidae", family != "Formicidae", order != "Arachnida", order != "Apterygota", order != "Collembola", order!= "Myriapoda") %>% # here, can decide to look at only pan, or only cones
  mutate(trap_id = paste(site_id, trap)) %>% 
  mutate(taxon = ifelse(suborder %in% "", order, suborder)) # use suborder when available

insect_dat3 <- as.data.table(insect_dat2)[, sum(number), by = .(site_id, taxon)]# Adding rows when duplicate; could have done this using aggragate instead, but wasn't working

insect_mat <- insect_dat3 %>% 
  dplyr::select(site_id, taxon, V1) %>% 
  spread(key = taxon, value = V1, fill = 0) %>%
  mutate("DUMB" = 1)  # adding dummy species (eg. Webster 2010) 

insect_mat$site_id <- as.character(insect_mat$site_id) # wide format, dummy species, column 2 is garbage

insect_dat_l <- insect_mat %>% # Long format, with site data, dummmy species
  left_join(site_data, by = "site_id") %>% 
  gather(key = "taxon", value = "number", 3:19) %>% 
  dplyr::select(-2)

insect_dat_w <- spread(insect_dat_l, key = "taxon", value = "number")







### Birds
bird_dat_suppl <- read.csv("data/bird_data_suppl.csv")
bird_dat2019 <- read.csv("data/bird_data2019.csv") %>% 
  filter(DetectionLocationNm != "O") %>% # removing species outside (O) the stand
  dplyr::select(Point, Count, Spp, DistanceBin)

bird_dat_count <- read.csv("data/bird_data.csv", header = T) %>% 
  filter(DetectionLocationNm != "O") %>% # removing species outside (O) the stand
  filter(DistanceBin < 101)  %>%
  full_join(bird_dat2019) %>% 
  dplyr::select(Point, Spp) %>%  # keeping only relevant columns
  dplyr::rename(species = Spp) %>% 
  dplyr::rename(site_id = Point)
  
# Adding additional species detections (outside count)
bird_dat_long <- bind_rows(bird_dat_suppl, bird_dat_count) 

# remove duplicate rows (same species detected multiple times in a single plot)
bird_dat_long <- unique(bird_dat_count[ , c(1,2) ]) %>% 
  mutate(pa = 1) %>% 
#  dplyr::rename(site_id = Point) %>% 
#  dplyr::rename(species = Spp) %>% 
  mutate(species = recode(species, BTYW='BTYW/HEWA', RUHU='XXHU', HEWA='BTYW/HEWA')) %>%  #changing all RUHU and ANHU to XXHU; BTYW and HEWA to BTYW/HEWA
  filter(!is.na(site_id))

# executing function and going from wide to long:
bird_dat_pa <- splist2presabs(bird_dat_long, sites.col = 1, sp.col = 2) %>% 
  pivot_longer(names_to = "species", values_to = "pa", cols = -site_id)

#Add in site data
site_data$site_id <- as.factor(site_data$site_id)
bird_dat <- left_join(bird_dat_pa, site_data, by = "site_id") 

# convert to wide format for following analysis
bird_dat_w <- spread(data = bird_dat, key = species, value = pa, fill = 0) #previously bird_matrix1

bird_mat_w <- bird_dat_w %>% 
  dplyr::select(-c(fire:sev_cov))

# optional:remove birds with few sightings; not sure this makes much difference
## add columns true/false depending on obs count reaching minimum value
#bird_matrix <- rbind(bird_matrix1[,-c(1:20)], c("colsum", colSums(bird_matrix1[,-c(1:20)]) == 1))

#bird_matrix <- bird_matrix1[,-c(1:27)] %>%
#  bind_rows(summarise_all(., funs(sum(.)))) # note: if wanting to make a row named total, could have used: funs(if(is.numeric(.)) sum(.) else "Total"

## keep only columns where minimum is reached (if subsetting some sites, change row number, here 49)

#bird_matrix <- bird_matrix[, (bird_matrix[49, ]) > 1]

## adding back in the site id and removing true/false row (and making sure it's all read in as numeric)

#bird_matrix <- cbind(site_id = bird_matrix1[, 1], bird_matrix[-49, ]) %>%
#  mutate_if(is.character, as.numeric)





# All spp
all_spp <- rbind(plant_dat_pa_l, bird_dat_pa) %>% 
  rbind(., lichen_dat_genus_pa_l) %>% 
#  rbind(., insect_dat_long_filt)
  filter(species != "Dummy") %>% 
  filter(species != "DUMB") 


# convert to wide matrix
all_spp_w_pa <- unique(all_spp[ , c(1:3)]) %>%
  filter(pa==1) %>% 
  spread(key = species, value = pa, fill = 0)

all_spp_dat_w <- merge(all_spp_w_pa, site_data, by = "site_id")

all_spp_dat_l <- gather(all_spp_dat_w, key = species, value = pa, ACMA:YRWA)
