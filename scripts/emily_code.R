# I have 3 habitat types: high severity (HS), low severity (LS), and unburnt (UN)
# Adjust accordingly for your data.

# First, make a list of spp found in HS
hs <- plant_dat_pa %>% 
  filter(sev =="h") %>% dplyr::select(c(ABCO:WOFI)) # Make a presence-absence matrix for species in HS; let me know if you need a function to convert abuncance to p/a
hs1 <- rbind(hs, colSums(hs)) # creating a row that sums all the presences and absences
hs1[hs1 > 0] <-1 # setting all the values in that row to 1 (is the species found in that habitat category or not)

hs2 <- hs1[,(hs1[24,]) == 1] # Keep only species for which the last row is 1 (species that are found in that habitat); you'll need to change 24 to whatever number is the last in your matrix

hs_spp <- data.frame(names = colnames(hs2)) # Making a list of species found in this habitat

# Next, make a list of spp found in LS
ls <- plant_dat_pa %>% filter(sev =="l") %>% dplyr::select(c(ABCO:WOFI))
ls1 <- rbind(ls, colSums(ls))
ls1[ls1 > 0] <-1
ls2 <- ls1[,(ls1[20,]) == 1] # adjust number for last row
ls_spp <- data.frame(names = colnames(ls2))

# Next a list of spp found in UN
un <- plant_dat_pa %>% filter(sev =="u") %>% dplyr::select(c(ABCO:WOFI))
un1 <- rbind(un, colSums(un))
un1[un1 > 0] <-1
un2 <- un1[,(un1[7,]) == 1] # adjust number for last row
un_spp <- data.frame(names = colnames(un2))

# data frame with species from each of my 3 habitats
x2 <- cbind.fill(ls_spp, hs_spp, un_spp, fill = NA)

# My goal is to create 2 columns: species unique to HS, and species unique to LS/ UN together. So adjust the next part as needed.

# spp in HS not in LS
x1 <- cbind.fill(hs_spp, ls_spp, fill = NA)
names(x1)[1]<-"hs_spp1"
names(x1)[2]<-"ls_spp1"
hs_spp1 <- x1[,1]
ls_spp1 <- x1[,2]
#spp_diff <- setdiff(hs_spp1, ls_spp1)

#spp in HS not in LS or UN
# This is messy but I'm merging the LS and UN categories
hs_spp1 <- rep(NA, 121) #change this number to the total number of species HS
ls_spp1 <- x2[,3]
un_df <- data.frame(hs_spp1, ls_spp1)
x3 <- rbind(x1, un_df)

hs_spp1 <- x3[,1]
ls_spp1 <- x3[,2] # henceforth ls_spp include unburnt
spp_diff1 <- setdiff(hs_spp1, ls_spp1)
spp_diff1 <- data.frame(spp_diff1)
names(spp_diff1)[1]<-"species"
spp_diff1 <- left_join(spp_diff1, plant_names, by = "species") %>% 
  dplyr::select(full_name) # this is because I want the species full names rather than the codes, and these are in a separate dataframe; you can take this out if you want just the codes

#spp in LS or UN not in HS
spp_diff2 <- setdiff(ls_spp1, hs_spp1)
spp_diff2 <- data.frame(spp_diff2)
names(spp_diff2)[1] <- "species"
spp_diff2 <- left_join(spp_diff2, plant_names, by = "species") %>% 
  dplyr::select(full_name) # this is because I want the species full names rather than the codes, and these are in a separate dataframe; you can take this out if you want just the codes

names(spp_diff1)[1]<-"Species found only in HS"
names(spp_diff2)[1]<-"Species found only in LS/UN"
spp_diff_table <- list(spp_diff1, spp_diff2) 

kable(spp_diff_table, caption = "Table 1: species restricted to only HS or LS/UN") %>%
  kable_styling()