
# This loads all the data
## Site
site_data_read <- read.csv("/Users/christopheradlam/Desktop/Davis/R/GitHub Repos/Fire_mosaics/Data/site_data.csv") 
site_data_read$tsf <- as.numeric(as.character(site_data_read$tsf))
site_data_read$fire_yr <- as.numeric(as.character(site_data_read$fire_yr))

# here I'm setting the TSF categories; change as necessary (tsf_cat = 3 age categories; tsf_2cat = 2 categories)
site_data <- site_data_read %>% 
  mutate(tsf_cat = ifelse(is.na(tsf), "4", "3")) %>% 
  mutate(tsf_2cat = ifelse(is.na(tsf), "3", "2"))

site_data$tsf_cat[site_data$tsf < 17] <-2
site_data$tsf_cat[site_data$tsf < 10] <-1

site_data$tsf_2cat[site_data$tsf < 15] <-1

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

## Lichens
lichen_names <-read.csv("/Users/christopheradlam/Desktop/Davis/R/GitHub Repos/Fire_mosaics/Data/lichen_list.csv")

lichen_dat_wide <- read.csv("data/lichen_data.csv", header = T) %>% 
  mutate(site_id = as.factor(site_id)) %>% 
  dplyr::select(site_id, species, abund) %>% 
  spread(key = species, value = abund, fill = 0) %>% 
  mutate("DUMB" = 1)  # adding dummy species (eg. Webster 2010) 

#f or genus level analysis (presence/absence)
lichen_dat_genus <- lichen_dat_wide %>% 
  gather(key=species, value=abund, AHPA:DUMB) %>% 
  left_join(., lichen_names, by = "species") %>% 
  dplyr::select(site_id,genus,abund) 

# remove duplicate rows (same species detected multiple times in a single plot)
lichen_dat_genus_pa1 <- lichen_dat_genus %>% 
  mutate(pa = ifelse(abund == 0, 0, 1)) %>% 
  dplyr::select(site_id, genus, pa)

#spread not working.
lichen_dat_genus_pa_l <- unique(lichen_dat_genus_pa1[ , c(1:3)]) %>%
  filter(pa==1)

lichen_dat_genus_pa_w <- lichen_dat_genus_pa_l %>% 
  spread(key = genus, value = pa, fill = 0) # Note column 30 is bogus; need dummy sp?

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

insect_dat <- insect_dat3 %>% 
  dplyr::select(site_id, taxon, V1) %>% 
  spread(key = taxon, value = V1, fill = 0) %>%
  mutate("DUMB" = 1)  # adding dummy species (eg. Webster 2010) 

insect_dat$site_id <- as.character(insect_dat$site_id) # wide format, dummy species, column 2 is garbage

insect_dat_long <- insect_dat %>% # Long format, with site data, dummmy species, row 2 is garbage
  left_join(site_data, by = "site_id") %>% 
  gather(key = "taxon", value = "number", 3:19)

insect_dat_wide <- spread(insect_dat_long, key = "taxon", value = "number") %>% 
  mutate(sev_tsf = paste(sev, tsf_cat, sep = "-")) # wide format, with site data, dummy species, column 2 is garbage
```


```{r echo = F}
# Select data (pick one)
# CHECK FOR TYPOS - ALL GOOD
# missing <- anti_join(plant_data, plant_names, by = "species")
# Join file with data and file with plant info, then dplyr::select required information; dplyr::select and run the right one:
plant_dat <- left_join(plant_data, plant_names, by = "species") %>%
  filter(native_status == "native") %>%
  dplyr::select(site_id, species, cover)

#plant_dat <- left_join(plant_dat, site_data, by = "site_id") %>% 
#  filter(sev=="l")

```

```{r eval = F, include = F}
plant_dat <- left_join(plant_data, plant_names, by = "species") %>%
  filter(native_status == "native" & form == "tree") %>%
  dplyr::select(site_id, species, cover)

plant_dat <- left_join(plant_data, plant_names, by = "species") %>%
  filter(native_status == "native" & form == "shrub") %>%
  dplyr::select(site_id, species, cover)

plant_dat <- left_join(plant_data, plant_names, by = "species") %>%
  filter(native_status == "native" & (form == "shrub" | form == "tree")) %>%
  dplyr::select(site_id, species, cover)

plant_dat <- left_join(plant_data, plant_names, by = "species") %>%
  filter(native_status == "native" & (form == "herb" | form == "grass")) %>%
  dplyr::select(site_id, species, cover)

plant_dat <- left_join(plant_data, plant_names, by = "species") %>%
  filter(native_status == "native" & (form == "herb")) %>%
  dplyr::select(site_id, species, cover)
```

```{r eval = F, include = F}
#Only run this chunk if converting to presence-absence
plant_dat$cover[plant_dat$cover > 0] <- 1 
```


```{r include=F}
# running this more than once without rerunning code above will mess it up
# Data prep
# convert to wide format for following analysis
plant_matrix1 <- spread(data = plant_dat, key = species, value = cover, fill = 0)

# optional:remove plants with few sightings; not sure this makes much difference
## Here I'm making a row for the frequency of detection
plant_dat$cover[plant_dat$cover > 0] <- 1 
plant_matrix2 <- spread(data = plant_dat, key = species, value = cover, fill = 0) %>% 
  bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))

plant_matrix <- plant_matrix1 %>%
  bind_rows(., plant_matrix2[49,]) %>%
  mutate("DUMB" = "0.5") # adding dummy species (eg. Webster 2010)  

## keep only columns where minimum is reached (if subsetting some sites, change row number, here 49) (true)
plant_matrix <- plant_matrix[, (plant_matrix[49, ]) > 10]
## adding back in the site id and removing true/false row (and making sure it's all read in as numeric)
plant_matrix <- plant_matrix[-49, ] %>%
  mutate_if(is.character, as.numeric)

# Make Bray-Curtis (?) dissimilarity matrix
plants_matrix <- as.matrix(plant_matrix1[, -1])
```
