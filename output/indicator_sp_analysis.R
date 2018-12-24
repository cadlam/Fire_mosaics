# load packages (I use these two lines of code because they install the package if it isn't already installed, otherwise they just load it)
if(!require('pacman'))install.packages('pacman')
pacman::p_load(tidyverse, knitr, kableExtra, labdsv)

# Start with your matrix; species as columns, plots as rows
# In my case, the matrix also has the plot data such as severity and TSF

data <- plant_matrix[,-c(1:2)] # removing the site data to leave only the species columns
groups1 <- as.vector(plant_matrix$sev) # select the column that provides the categories you want

Indval_out1 <- indval(data, groups1, numitr=10000)
gr <- Indval_out1$maxcls[Indval_out1$pval<=0.05]
iv <- Indval_out1$indcls[Indval_out1$pval<=0.05]
pv <- Indval_out1$pval[Indval_out1$pval<=0.05]
fr <- apply(data>0, 2, sum)[Indval_out1$pval<=0.05]
indvalsummary <- data.frame(group=gr, indval=iv, pvalue=pv, freq=fr)
indvalsummary1 <- indvalsummary[order(indvalsummary$group, -indvalsummary$indval),]
prob.corrected1 = p.adjust(Indval_out1$pval, "BH") # correct p.value for multiple testing

indvalsummary1 <- cbind(rownames(indvalsummary1), data.frame(indvalsummary1, row.names=NULL))

names(indvalsummary1)[1]<-"species"

# You could stop here. The following code it to make a prettier output, modify at will.
## Make indicator value out of 100 and remove species with indicator value less than 25 or with non-significant p-value
indval <- indvalsummary1 %>% 
  dplyr::select(species, group, indval, pvalue) %>% 
  mutate("Indicator value" = indval * 100) %>% 
  filter(indval >= 0.25) %>% 
  filter(pvalue < 0.05)

## I have 2 groups HS and LS/UN. The function just calls them 1 and 2, so I have to change that.
indval_hs <- indval %>% 
  filter(group =="1") %>% 
  dplyr::select(species, "Indicator value")

indval_ls <- indval %>% 
  filter(group =="2")%>% 
  dplyr::select(species, "Indicator value")

names(indval_hs)[1]<-"Indicators of HS"
names(indval_ls)[1]<-"Indicators of LS/UN"

## Now I make the table
indval_table <- list(indval_hs, indval_ls)

kable(indval_table, caption = "Table 1: Indicator species for different habitats", digits = 0) %>%
  kable_styling()
