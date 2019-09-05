# This loads all the data (some additional manipulation required for plants to select native species, spcific growth forms, or to change to p/a)
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

## Plants
plant_data <- read.csv("/Users/christopheradlam/Desktop/Davis/R/GitHub Repos/Fire_mosaics/Data/plant_data.csv")
plant_names <- read.csv("/Users/christopheradlam/Desktop/Davis/R/GitHub Repos/Fire_mosaics/data/plant_list.csv")
plant_data$species <- as.character(plant_data$species)
plant_data$site_id <- as.character(plant_data$site_id)
plant_names$species <- as.character(plant_names$species)
plant_names$full_name <- as.character(plant_names$full_name)
plant_names$native_status <- as.factor(plant_names$native_status)
plant_names$form <- as.factor(plant_names$form)

### Keep only native spp
## Also for some ecologically similar and taxonomically related plant species, use genus 
plant_dat <- left_join(plant_data, plant_names, by = "species") %>%
  filter(native_status == "native") %>%
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
  mutate(species = ifelse(genus == "Cirsium", "Cirsium", species)) %>% 
  dplyr::select(site_id, species, cover)

plant_mat_pa_w <- splist2presabs(plant_dat, sites.col = 1, sp.col = 2) 

plant_dat_pa_w <- plant_mat_pa_w %>% 
  left_join(., site_data, by ="site_id") %>% 
  mutate(cov_cat = ifelse(tree_cov > 65, 3, ifelse(tree_cov > 30, 2, 1)))

plant_mat_pa_l <- plant_mat_pa_w %>% 
  gather(key = species, value = pa, ABCO:WOFI)

## Lichens
lichen_names <-read.csv("/Users/christopheradlam/Desktop/Davis/R/GitHub Repos/Fire_mosaics/Data/lichen_list.csv")

lichen_mat_species_w <- read.csv("data/lichen_data.csv", header = T) %>% 
  mutate(site_id = as.factor(site_id)) %>% 
  dplyr::select(site_id, species, abund) %>% 
  spread(key = species, value = abund, fill = 0) %>% 
  mutate("DUMB" = 1)  # adding dummy species (eg. Webster 2010) 

# for genus level analysis (presence/absence)
lichen_mat_genus_l <- lichen_mat_species_w %>% 
  gather(key=species, value=abund, AHPA:DUMB) %>% 
  left_join(., lichen_names, by = "species") %>% 
  dplyr::select(site_id,genus,abund) 

# remove duplicate rows (same species detected multiple times in a single plot)
lichen_mat_genus_pa1 <- lichen_mat_genus_l %>% 
  mutate(pa = ifelse(abund == 0, 0, 1)) %>% 
  dplyr::select(site_id, genus, pa)

#spread not working.
lichen_mat_genus_pa_l <- unique(lichen_mat_genus_pa1[ , c(1:3)]) %>%
  filter(pa==1)

lichen_mat_genus_pa_w <- lichen_mat_genus_pa_l %>% 
  spread(key = genus, value = pa, fill = 0) %>% 
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


