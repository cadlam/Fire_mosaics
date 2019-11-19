# Moran's I

dists <- as.matrix(dist(cbind(plant_dat_pa_w$lon, plant_dat_pa_w$lat))) # used to use plant_glm_cov
#dists.inv <- 1/dists
#diag(dists.inv) <- 0
#Moran.I(plant_mrpp_d$PSME, dists.inv, na.rm = T)

# convert w to a row standardised general weights object
lw <- mat2listw(dists)
lwW <- nb2listw(lw$neighbours, glist=lw$weights, style="W")






# lmt function (p/a)
#Converting logit scale to natural scale. The link function for binomial is logit, so you need to backtransform to get from that scale to a probability of occurence (0-1). Use the following function. Be sure to add together any coefficients and intercepts first - all arithmetic should be done before backtransformation:

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#If you do anything that uses Poisson, that's on the log scale so you can just use exp() to convert to the natural scale.


# before the function can add a line to the table, need to create the table!
tbl_headr <- c("sp", "Model", "dAICc", "w", "X2", "df", "p", "R2", 'Moran I', 'Moran I (p)')

#function:
lmt <- function(sp_dat) {
  tsf <- glm(sp_dat[,2] ~ tsf_cat2, sp_dat, family = binomial) #sp cover /100 because it has to be between 0 and 1
  sev <- glm(sp_dat[,2] ~ sev, sp_dat, family = binomial)
  sp_null  <-glm(sp_dat[,2] ~ 1, sp_dat, family = binomial)
  add <- glm(sp_dat[,2] ~ sev + tsf_cat2, sp_dat, family = binomial)
  mult <- glm(sp_dat[,2] ~ sev * tsf_cat2, sp_dat, family = binomial)
  null <- glm(sp_dat[,2] ~ 1, sp_dat, family = binomial)
  
  # AIC and likelihood ratio test
  sp_aic <- AICctab(null, add, mult, sev, tsf, base=T, delta=T, weights=T)
  
  # R2 values 
  
  # row 1 R2
  if(attr(sp_aic, "row.names")[1] == "sev") {
    sp_R2_1 <- rsq(sev, adj = T)
  } else if(attr(sp_aic, "row.names")[1] == "tsf") {
    sp_R2_1 <- rsq(tsf, adj = T)
  } else if(attr(sp_aic, "row.names")[1] == "add") {
    sp_R2_1 <- rsq(add, adj = T)
  } else if(attr(sp_aic, "row.names")[1] == "null") {
    sp_R2_1 <- rsq(null, adj = T)
  } else {
    sp_R2_1 <- rsq(mult, adj = T)
  }
  
  # row 2 R2
  if(attr(sp_aic, "row.names")[2] == "sev") {
    sp_R2_2 <- rsq(sev, adj = T)
  } else if(attr(sp_aic, "row.names")[2] == "tsf") {
    sp_R2_2 <- rsq(tsf, adj = T)
  } else if(attr(sp_aic, "row.names")[2] == "add") {
    sp_R2_2 <- rsq(add, adj = T)
  } else if(attr(sp_aic, "row.names")[2] == "null") {
    sp_R2_2 <- rsq(null, adj = T)
  } else {
    sp_R2_2 <- rsq(mult, adj = T)
  }
  
  # row 3 R2
  if(attr(sp_aic, "row.names")[3] == "sev") {
    sp_R2_3 <- rsq(sev, adj = T)
  } else if(attr(sp_aic, "row.names")[3] == "tsf") {
    sp_R2_3 <- rsq(tsf, adj = T)
  } else if(attr(sp_aic, "row.names")[3] == "add") { 
    sp_R2_3 <- rsq(add, adj = T)
  } else if(attr(sp_aic, "row.names")[3] == "null") {
    sp_R2_3 <- rsq(null, adj = T)
  } else {
    sp_R2_3 <- rsq(mult, adj = T)
  }
  
  # row 4 R2
  if(attr(sp_aic, "row.names")[4] == "sev") {
    sp_R2_4 <- rsq(sev, adj = T)
  } else if(attr(sp_aic, "row.names")[4] == "tsf") {
    sp_R2_4 <- rsq(tsf, adj = T)
  } else if(attr(sp_aic, "row.names")[4] == "add") {
    sp_R2_4 <- rsq(add, adj = T)
  } else if(attr(sp_aic, "row.names")[4] == "null") {
    sp_R2_4 <- rsq(null, adj = T)
  } else {
    sp_R2_4 <- rsq(mult, adj = T)
  }
  
  # row 5 R2
  if(attr(sp_aic, "row.names")[5] == "sev") {
    sp_R2_5 <- rsq(sev, adj = T)
  } else if(attr(sp_aic, "row.names")[5] == "tsf") {
    sp_R2_5 <- rsq(tsf, adj = T)
  } else if(attr(sp_aic, "row.names")[5] == "add") {
    sp_R2_5 <- rsq(add, adj = T)
  } else if(attr(sp_aic, "row.names")[5] == "null") {
    sp_R2_5 <- rsq(null, adj = T)
  } else {
    sp_R2_5 <- rsq(mult, adj = T)
  }
  
  # Likelihood Ratio Test
  # row 1 R2
  if(attr(sp_aic, "row.names")[1] == "sev") {
    sp_lrt_1 <- lrtest(sev, sp_null)
  } else if(attr(sp_aic, "row.names")[1] == "tsf") {
    sp_lrt_1 <- lrtest(tsf, sp_null)
  } else if(attr(sp_aic, "row.names")[1] == "add") {
    sp_lrt_1 <- lrtest(add, sp_null)
  } else if(attr(sp_aic, "row.names")[1] == "null") {
    sp_lrt_1 <- lrtest(null, sp_null)
  } else {
    sp_lrt_1 <- lrtest(mult, sp_null)
  }
  
  # row 2 R2
  if(attr(sp_aic, "row.names")[2] == "sev") {
    sp_lrt_2 <- lrtest(sev, sp_null)
  } else if(attr(sp_aic, "row.names")[2] == "tsf") {
    sp_lrt_2 <- lrtest(tsf, sp_null)
  } else if(attr(sp_aic, "row.names")[2] == "add") {
    sp_lrt_2 <- lrtest(add, sp_null)
  } else if(attr(sp_aic, "row.names")[2] == "null") {
    sp_lrt_2 <- lrtest(null, sp_null)
  } else {
    sp_lrt_2 <- lrtest(mult, sp_null)
  }
  
  # row 3 R2
  if(attr(sp_aic, "row.names")[3] == "sev") {
    sp_lrt_3 <- lrtest(sev, sp_null)
  } else if(attr(sp_aic, "row.names")[3] == "tsf") {
    sp_lrt_3 <- lrtest(tsf, sp_null)
  } else if(attr(sp_aic, "row.names")[3] == "add") {
    sp_lrt_3 <- lrtest(add, sp_null)
  } else if(attr(sp_aic, "row.names")[3] == "null") {
    sp_lrt_3 <- lrtest(null, sp_null)
  } else {
    sp_lrt_3 <- lrtest(mult, sp_null)
  }
  
  # row 4 R2
  if(attr(sp_aic, "row.names")[4] == "sev") {
    sp_lrt_4 <- lrtest(sev, sp_null)
  } else if(attr(sp_aic, "row.names")[4] == "tsf") {
    sp_lrt_4 <- lrtest(tsf, sp_null)
  } else if(attr(sp_aic, "row.names")[4] == "add") {
    sp_lrt_4 <- lrtest(add, sp_null)
  } else if(attr(sp_aic, "row.names")[4] == "null") {
    sp_lrt_4 <- lrtest(null, sp_null)
  } else {
    sp_lrt_4 <- lrtest(mult, sp_null)
  }
  
  # row 5 R2
  if(attr(sp_aic, "row.names")[5] == "sev") {
    sp_lrt_5 <- lrtest(sev, sp_null)
  } else if(attr(sp_aic, "row.names")[5] == "tsf") {
    sp_lrt_5 <- lrtest(tsf, sp_null)
  } else if(attr(sp_aic, "row.names")[5] == "add") {
    sp_lrt_5 <- lrtest(add, sp_null)
  } else if(attr(sp_aic, "row.names")[5] == "null") {
    sp_lrt_5 <- lrtest(null, sp_null)
  } else {
    sp_lrt_5 <- lrtest(mult, sp_null)
  }
  
  
  #sp_lrt_full <- lrtest(full, sp_null)
  #sp_lrt_sev <- lrtest(sev, sp_null)
  #sp_lrt_tsfcat <- lrtest(tsf, sp_null)
  
  #moran's I (first load function)
  sp_mi <- (moran.test(sp_dat[,2], lwW, alternative="two.sided"))$statistic
  sp_mip <- (moran.test(sp_dat[,2], lwW, alternative="two.sided"))$'p.value'
  
  # construct table
  sp_tbl1 <- c(colnames(sp_dat)[2], attr(sp_aic, "row.names")[1], sp_aic$dAICc[1], sp_aic$weight[1], sp_lrt_1$Chisq[2], sp_lrt_1$'#Df'[1], sp_lrt_1$'Pr(>Chisq)'[2], sp_R2_1, sp_mi, sp_mip)
  
  sp_tbl2 <- c(colnames(sp_dat)[2], attr(sp_aic, "row.names")[2], sp_aic$dAICc[2], sp_aic$weight[2], sp_lrt_2$Chisq[2], sp_lrt_2$'#Df'[1], sp_lrt_2$'Pr(>Chisq)'[2], sp_R2_2, sp_mi, sp_mip)
  
  sp_tbl3 <- c(colnames(sp_dat)[2], attr(sp_aic, "row.names")[3], sp_aic$dAICc[3], sp_aic$weight[3], sp_lrt_3$Chisq[2], sp_lrt_3$'#Df'[1], sp_lrt_3$'Pr(>Chisq)'[2], sp_R2_3, sp_mi, sp_mip)
  
  sp_tbl4 <- c(colnames(sp_dat)[2], attr(sp_aic, "row.names")[4], sp_aic$dAICc[4], sp_aic$weight[4], sp_lrt_4$Chisq[2], sp_lrt_4$'#Df'[1], sp_lrt_4$'Pr(>Chisq)'[2], sp_R2_4, sp_mi, sp_mip)
  
  sp_tbl5 <- c(colnames(sp_dat)[2], attr(sp_aic, "row.names")[5], sp_aic$dAICc[5], sp_aic$weight[5], sp_lrt_5$Chisq[2], sp_lrt_5$'#Df'[1], sp_lrt_5$'Pr(>Chisq)'[2], sp_R2_5, sp_mi, sp_mip)
  
  tbl <- data.frame(rbind(tbl_headr, sp_tbl1, sp_tbl2, sp_tbl3, sp_tbl4, sp_tbl5)) 
  
  # Turn first row into column names
  names(tbl) <- lapply(tbl[1, ], as.character)
  tbl <- tbl[-1,]
  
  # convert to numeric
  tbl[,c(3:10)] <- as.numeric(as.character(unlist(tbl[,c(3:10)]))) 
  
  #remove row names
  rownames(tbl) <- c()
  
  tbl
}









# lmt function (cov)
#need to fix if else like above
#function:
lmt <- function(sp_dat) {
  tsf <- glm(sp_dat[,2]/100 ~ tsf_cat, sp_dat, family = gaussian) #sp cover /100 because it has to be between 0 and 1
  sev <- glm(sp_dat[,2]/100 ~ sev, sp_dat, family = gaussian)
  sp_null  <-glm(sp_dat[,2]/100 ~ 1, sp_dat, family = gaussian)
  add <- glm(sp_dat[,2]/100 ~ sev + tsf_cat, sp_dat, family = gaussian)
  mult <- glm(sp_dat[,2]/100 ~ sev * tsf_cat, sp_dat, family = gaussian)
  null <- glm(sp_dat[,2]/100 ~ 1, sp_dat, family = gaussian)
  
  # AIC and likelihood ratio test
  sp_aic <- AICctab(null, add, mult, sev, tsf, base=T, delta=T, weights=T)
  
  # R2 values 
  
  # row 1 R2
  if(attr(sp_aic, "row.names")[1] == "sev")
  {sp_R2_1 <- rsq(sev, adj = T)}
  if(attr(sp_aic, "row.names")[1] == "tsf")
  {sp_R2_1 <- rsq(tsf, adj = T)}
  if(attr(sp_aic, "row.names")[1] == "add")
  {sp_R2_1 <- rsq(add, adj = T)}
  if(attr(sp_aic, "row.names")[1] == "null")
  {sp_R2_1 <- rsq(null, adj = T)}
  else 
  {sp_R2_1 <- rsq(mult, adj = T)}
  
  # row 2 R2
  if(attr(sp_aic, "row.names")[2] == "sev")
  {sp_R2_2 <- rsq(sev, adj = T)}
  if(attr(sp_aic, "row.names")[2] == "tsf")
  {sp_R2_2 <- rsq(tsf, adj = T)}
  if(attr(sp_aic, "row.names")[2] == "add")
  {sp_R2_2 <- rsq(add, adj = T)}
  if(attr(sp_aic, "row.names")[2] == "null")
  {sp_R2_5 <- rsq(null, adj = T)}
  else 
  {sp_R2_2 <- rsq(mult, adj = T)}
  
  # row 3 R2
  if(attr(sp_aic, "row.names")[3] == "sev")
  {sp_R2_3 <- rsq(sev, adj = T)}
  if(attr(sp_aic, "row.names")[3] == "tsf")
  {sp_R2_3 <- rsq(tsf, adj = T)}
  if(attr(sp_aic, "row.names")[3] == "add")
  {sp_R2_3 <- rsq(add, adj = T)}
  if(attr(sp_aic, "row.names")[3] == "null")
  {sp_R2_5 <- rsq(null, adj = T)}
  else 
  {sp_R2_3 <- rsq(mult, adj = T)}
  
  # row 4 R2
  if(attr(sp_aic, "row.names")[4] == "sev")
  {sp_R2_4 <- rsq(sev, adj = T)}
  if(attr(sp_aic, "row.names")[4] == "tsf")
  {sp_R2_4 <- rsq(tsf, adj = T)}
  if(attr(sp_aic, "row.names")[4] == "add")
  {sp_R2_4 <- rsq(add, adj = T)}
  if(attr(sp_aic, "row.names")[4] == "null")
  {sp_R2_5 <- rsq(null, adj = T)}
  else 
  {sp_R2_4 <- rsq(mult, adj = T)}
  
  # row 5 R2
  if(attr(sp_aic, "row.names")[5] == "sev")
  {sp_R2_5 <- rsq(sev, adj = T)}
  if(attr(sp_aic, "row.names")[5] == "tsf")
  {sp_R2_5 <- rsq(tsf, adj = T)}
  if(attr(sp_aic, "row.names")[5] == "add")
  {sp_R2_5 <- rsq(add, adj = T)}
  if(attr(sp_aic, "row.names")[5] == "null")
  {sp_R2_5 <- rsq(null, adj = T)}
  else 
  {sp_R2_5 <- rsq(mult, adj = T)}
  
  # Likelihood Ratio Test
  # row 1 R2
  if(attr(sp_aic, "row.names")[1] == "sev")
  {sp_lrt_1 <- lrtest(sev, sp_null)}
  if(attr(sp_aic, "row.names")[1] == "tsf")
  {sp_lrt_1 <- lrtest(tsf, sp_null)}
  if(attr(sp_aic, "row.names")[1] == "add")
  {sp_lrt_1 <- lrtest(add, sp_null)}
  if(attr(sp_aic, "row.names")[1] == "null")
  {sp_lrt_1 <- lrtest(null, sp_null)}
  else 
  {sp_lrt_1 <- lrtest(mult, sp_null)}
  
  # row 2 R2
  if(attr(sp_aic, "row.names")[2] == "sev")
  {sp_lrt_2 <- lrtest(sev, sp_null)}
  if(attr(sp_aic, "row.names")[2] == "tsf")
  {sp_lrt_2 <- lrtest(tsf, sp_null)}
  if(attr(sp_aic, "row.names")[2] == "add")
  {sp_lrt_2 <- lrtest(add, sp_null)}
  if(attr(sp_aic, "row.names")[2] == "null")
  {sp_lrt_2 <- lrtest(null, sp_null)}
  else 
  {sp_lrt_2 <- lrtest(mult, sp_null)}
  
  # row 3 R2
  if(attr(sp_aic, "row.names")[3] == "sev")
  {sp_lrt_3 <- lrtest(sev, sp_null)}
  if(attr(sp_aic, "row.names")[3] == "tsf")
  {sp_lrt_3 <- lrtest(tsf, sp_null)}
  if(attr(sp_aic, "row.names")[3] == "add")
  {sp_lrt_3 <- lrtest(add, sp_null)}
  if(attr(sp_aic, "row.names")[3] == "null")
  {sp_lrt_3 <- lrtest(null, sp_null)}
  else 
  {sp_lrt_3 <- lrtest(mult, sp_null)}
  
  # row 4 R2
  if(attr(sp_aic, "row.names")[4] == "sev")
  {sp_lrt_4 <- lrtest(sev, sp_null)}
  if(attr(sp_aic, "row.names")[4] == "tsf")
  {sp_lrt_4 <- lrtest(tsf, sp_null)}
  if(attr(sp_aic, "row.names")[4] == "add")
  {sp_lrt_4 <- lrtest(add, sp_null)}
  if(attr(sp_aic, "row.names")[4] == "null")
  {sp_lrt_4 <- lrtest(null, sp_null)}
  else 
  {sp_lrt_4 <- lrtest(mult, sp_null)}
  
  # row 5 R2
  if(attr(sp_aic, "row.names")[5] == "sev")
  {sp_lrt_5 <- lrtest(sev, sp_null)}
  if(attr(sp_aic, "row.names")[5] == "tsf")
  {sp_lrt_ <- lrtest(tsf, sp_null)}
  if(attr(sp_aic, "row.names")[5] == "add")
  {sp_lrt_5 <- lrtest(add, sp_null)}
  if(attr(sp_aic, "row.names")[5] == "null")
  {sp_lrt_5 <- lrtest(null, sp_null)}
  else 
  {sp_lrt_5 <- lrtest(mult, sp_null)}
  
  
  #sp_lrt_full <- lrtest(full, sp_null)
  #sp_lrt_sev <- lrtest(sev, sp_null)
  #sp_lrt_tsfcat <- lrtest(tsf, sp_null)
  
  #moran's I (first load function)
  sp_mi <- (moran.test(sp_dat[,2], lwW, alternative="two.sided"))$statistic
  sp_mip <- (moran.test(sp_dat[,2], lwW, alternative="two.sided"))$'p.value'
  
  # construct table
  sp_tbl1 <- c(colnames(sp_dat)[2], attr(sp_aic, "row.names")[1], sp_aic$dAICc[1], sp_aic$weight[1], sp_lrt_1$Chisq[2], sp_lrt_1$'#Df'[1], sp_lrt_1$'Pr(>Chisq)'[2], sp_R2_1, sp_mi, sp_mip)
  
  sp_tbl2 <- c(colnames(sp_dat)[2], attr(sp_aic, "row.names")[2], sp_aic$dAICc[2], sp_aic$weight[2], sp_lrt_2$Chisq[2], sp_lrt_2$'#Df'[1], sp_lrt_2$'Pr(>Chisq)'[2], sp_R2_2, sp_mi, sp_mip)
  
  sp_tbl3 <- c(colnames(sp_dat)[2], attr(sp_aic, "row.names")[3], sp_aic$dAICc[3], sp_aic$weight[3], sp_lrt_3$Chisq[2], sp_lrt_3$'#Df'[1], sp_lrt_3$'Pr(>Chisq)'[2], sp_R2_3, sp_mi, sp_mip)
  
  sp_tbl4 <- c(colnames(sp_dat)[2], attr(sp_aic, "row.names")[4], sp_aic$dAICc[4], sp_aic$weight[4], sp_lrt_4$Chisq[2], sp_lrt_4$'#Df'[1], sp_lrt_4$'Pr(>Chisq)'[2], sp_R2_4, sp_mi, sp_mip)
  
  sp_tbl5 <- c(colnames(sp_dat)[2], attr(sp_aic, "row.names")[5], sp_aic$dAICc[5], sp_aic$weight[5], sp_lrt_5$Chisq[2], sp_lrt_5$'#Df'[1], sp_lrt_5$'Pr(>Chisq)'[2], sp_R2_5, sp_mi, sp_mip)
  
  tbl <- data.frame(rbind(tbl_headr, sp_tbl1, sp_tbl2, sp_tbl3, sp_tbl4, sp_tbl5)) 
  
  # Turn first row into column names
  names(tbl) <- lapply(tbl[1, ], as.character)
  tbl <- tbl[-1,]
  
  # convert to numeric
  tbl[,c(3:10)] <- as.numeric(as.character(unlist(tbl[,c(3:10)]))) 
  
  #remove row names
  rownames(tbl) <- c()
  
  tbl
}
