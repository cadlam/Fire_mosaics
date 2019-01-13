
# Occ_plot function (cov)
# how to transform skewed data?
# check if there is a nice bell curve or if it's truncated (gaussian not so good if skewed)
# transform SE to standard dev

# to extract the coefficients:
#coef(summary(CEIN_mod))["(Intercept)","Estimate"]

# equivalently using package broom:
## construct function; this works with sev and tsf_cat (3 categories)
occ_plot_full <- function(mod) {
  CEIN_tidy <- tidy(mod)
  # I think there should only be 2 age categories for ls and hs rather than 3
  hs1_mean <- CEIN_tidy[1,2]
  hs2_mean <- CEIN_tidy[1,2] + CEIN_tidy[4,2]
  hs3_mean <- CEIN_tidy[1,2] + CEIN_tidy[5,2]
  ls1_mean <- CEIN_tidy[1,2] + CEIN_tidy[2,2]
  ls2_mean <- CEIN_tidy[1,2] + CEIN_tidy[2,2] + CEIN_tidy[4,2]
  ls3_mean <- CEIN_tidy[1,2] + CEIN_tidy[2,2] + CEIN_tidy[5,2]
  un_mean <- CEIN_tidy[1,2] + CEIN_tidy[3,2]
  
  mean <- unlist(c(hs1_mean, hs2_mean, hs3_mean, ls1_mean, ls2_mean, ls3_mean, un_mean))
  
  hs1_SE <- CEIN_tidy[1,3]
  hs2_SE <- CEIN_tidy[1,3] + CEIN_tidy[4,3]
  hs3_SE <- CEIN_tidy[1,3] + CEIN_tidy[5,3]
  ls1_SE <- CEIN_tidy[1,3] + CEIN_tidy[2,3]
  ls2_SE <- CEIN_tidy[1,3] + CEIN_tidy[2,3] + CEIN_tidy[4,3]
  ls3_SE <- CEIN_tidy[1,3] + CEIN_tidy[2,3] + CEIN_tidy[5,3]
  un_SE <- CEIN_tidy[1,3] + CEIN_tidy[3,3]
  
  SE <- unlist(c(hs1_SE, hs2_SE, hs3_SE, ls1_SE, ls2_SE, ls3_SE, un_SE))
  
  hab <- c("hs1", "hs2", "hs3", "ls1", "ls2", "ls3", 'un')
  
  
  plot_data <- data.frame(hab, mean, SE)
  
  p <- ggplot(plot_data, aes(x = hab, y = mean)) +
    geom_col() +
    geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,
                  position=position_dodge(.9))
  
  return(p)
}


##THis works with sev_tsf
occ_plot_sevtsf <- function(mod) {
  CEIN_tidy <- tidy(mod)
  # I think there should only be 2 age categories for ls and hs rather than 3
  hs1_mean <- CEIN_tidy[1,2]
  hs2_mean <- CEIN_tidy[1,2] + CEIN_tidy[2,2]
  hs3_mean <- CEIN_tidy[1,2] + CEIN_tidy[3,2]
  ls1_mean <- CEIN_tidy[1,2] + CEIN_tidy[4,2]
  ls2_mean <- CEIN_tidy[1,2] + CEIN_tidy[5,2]
  ls3_mean <- CEIN_tidy[1,2] + CEIN_tidy[6,2]
  un_mean <- CEIN_tidy[1,2] + CEIN_tidy[7,2]
  
  mean <- unlist(c(hs1_mean, hs2_mean, hs3_mean, ls1_mean, ls2_mean, ls3_mean, un_mean))
  
  hs1_SE <- CEIN_tidy[1,3]
  hs2_SE <- CEIN_tidy[1,3] + CEIN_tidy[2,3]
  hs3_SE <- CEIN_tidy[1,3] + CEIN_tidy[3,3]
  ls1_SE <- CEIN_tidy[1,3] + CEIN_tidy[4,3]
  ls2_SE <- CEIN_tidy[1,3] + CEIN_tidy[5,3]
  ls3_SE <- CEIN_tidy[1,3] + CEIN_tidy[6,3]
  un_SE <- CEIN_tidy[1,3] + CEIN_tidy[7,3]
  
  SE <- unlist(c(hs1_SE, hs2_SE, hs3_SE, ls1_SE, ls2_SE, ls3_SE, un_SE))
  
  hab <- c("hs1", "hs2", "hs3", "ls1", "ls2", "ls3", 'un')
  
  
  plot_data <- data.frame(hab, mean, SE)
  
  p <- ggplot(plot_data, aes(x = hab, y = mean)) +
    geom_col() +
    geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,
                  position=position_dodge(.9))
  
  return(p)
}

#sev only
occ_plot_sev <- function(mod) {
  CEIN_tidy <- tidy(mod)
  # I think there should only be 2 age categories for ls and hs rather than 3
  hs_mean <- CEIN_tidy[1,2]
  ls_mean <- CEIN_tidy[1,2] + CEIN_tidy[2,2]
  un_mean <- CEIN_tidy[1,2] + CEIN_tidy[3,2]
  
  mean <- unlist(c(hs_mean, ls_mean, un_mean))
  
  hs_SE <- CEIN_tidy[1,3]
  ls_SE <- CEIN_tidy[1,3] + CEIN_tidy[2,3]
  un_SE <- CEIN_tidy[1,3] + CEIN_tidy[3,3]
  
  SE <- unlist(c(hs_SE, ls_SE, un_SE))
  
  hab <- c("HS", "LS", "UN")
  
  
  plot_data <- data.frame(hab, mean, SE)
  
  p <- ggplot(plot_data, aes(x = hab, y = mean)) +
    geom_col() +
    geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,
                  position=position_dodge(.9))
  
  return(p)
}

#tsf_cat only (3 categories plus UN)
occ_plot_tsf <- function(mod) {
  CEIN_tidy <- tidy(mod)
  # I think there should only be 2 age categories for ls and hs rather than 3
  mean1 <- CEIN_tidy[1,2]
  mean2 <- CEIN_tidy[1,2] + CEIN_tidy[2,2]
  mean3 <- CEIN_tidy[1,2] + CEIN_tidy[3,2]
  un_mean <- CEIN_tidy[1,2] + CEIN_tidy[4,2]
  
  mean <- unlist(c(mean1, mean2, mean3, un_mean))
  
  SE1 <- CEIN_tidy[1,3]
  SE2 <- CEIN_tidy[1,3] + CEIN_tidy[2,3]
  SE3 <- CEIN_tidy[1,3] + CEIN_tidy[3,3]
  un_SE <- CEIN_tidy[1,3] + CEIN_tidy[4,3]
  
  SE <- unlist(c(SE1, SE2, SE3, un_SE))
  
  hab <- c("1", "2", "3", 'un')
  
  
  plot_data <- data.frame(hab, mean, SE)
  
  p <- ggplot(plot_data, aes(x = hab, y = mean)) +
    geom_col() +
    geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,
                  position=position_dodge(.9))
  
  return(p)
}

#full with 2 tsf categories
occ_plot_full2cat <- function(mod) {
  CEIN_tidy <- tidy(mod)
  # I think there should only be 2 age categories for ls and hs rather than 3
  hs1_mean <- CEIN_tidy[1,2]
  hs2_mean <- CEIN_tidy[1,2] + CEIN_tidy[4,2]
  ls1_mean <- CEIN_tidy[1,2] + CEIN_tidy[2,2]
  ls2_mean <- CEIN_tidy[1,2] + CEIN_tidy[2,2] + CEIN_tidy[4,2]
  un_mean <- CEIN_tidy[1,2] + CEIN_tidy[3,2]
  
  mean <- unlist(c(hs1_mean, hs2_mean, ls1_mean, ls2_mean, un_mean))
  
  hs1_SE <- CEIN_tidy[1,3]
  hs2_SE <- CEIN_tidy[1,3] + CEIN_tidy[4,3]
  ls1_SE <- CEIN_tidy[1,3] + CEIN_tidy[2,3]
  ls2_SE <- CEIN_tidy[1,3] + CEIN_tidy[2,3] + CEIN_tidy[4,3]
  un_SE <- CEIN_tidy[1,3] + CEIN_tidy[3,3]
  
  SE <- unlist(c(hs1_SE, hs2_SE, ls1_SE, ls2_SE, un_SE))
  
  hab <- c("hs1", "hs2", "ls1", "ls2", 'un')
  
  
  plot_data <- data.frame(hab, mean, SE)
  
  p <- ggplot(plot_data, aes(x = hab, y = mean)) +
    geom_col() +
    geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,
                  position=position_dodge(.9))
  
  return(p)
}












# Occ_plot function for p/a
## construct function; this works with sev and tsf_cat (3 categories)
#mod <- sp_mod1

#turns out the full, multiplicative model is never the best!
occ_plot_full <- function(mod) {
  CEIN_tidy <- tidy(mod)
  # I think there should only be 2 age categories for ls and hs rather than 3
  hs1_mean <- logit2prob(CEIN_tidy[1,2])
  hs2_mean <- logit2prob(CEIN_tidy[1,2]) + logit2prob(CEIN_tidy[4,2])
  hs3_mean <- logit2prob(CEIN_tidy[1,2]) + logit2prob(CEIN_tidy[5,2])
  ls1_mean <- logit2prob(CEIN_tidy[1,2]) + logit2prob(CEIN_tidy[2,2])
  ls2_mean <- logit2prob(CEIN_tidy[1,2]) + logit2prob(CEIN_tidy[2,2]) + logit2prob(CEIN_tidy[4,2])
  ls3_mean <- logit2prob(CEIN_tidy[1,2]) + logit2prob(CEIN_tidy[2,2]) + logit2prob(CEIN_tidy[5,2])
  un_mean <- logit2prob(CEIN_tidy[1,2]) + logit2prob(CEIN_tidy[3,2])
  
  mean <- unlist(c(hs1_mean, hs2_mean, hs3_mean, ls1_mean, ls2_mean, ls3_mean, un_mean))
  
  hs1_SD <- sqrt(logit2prob(CEIN_tidy[1,3]))
  hs2_SD <- sqrt(logit2prob(CEIN_tidy[1,3])) + sqrt(logit2prob(CEIN_tidy[4,3]))
  hs3_SD <- sqrt(logit2prob(CEIN_tidy[1,3])) + sqrt(logit2prob(CEIN_tidy[5,3]))
  ls1_SD <- sqrt(logit2prob(CEIN_tidy[1,3])) + sqrt(logit2prob(CEIN_tidy[2,3]))
  ls2_SD <- sqrt(logit2prob(CEIN_tidy[1,3])) + sqrt(logit2prob(CEIN_tidy[2,3])) + sqrt(logit2prob(CEIN_tidy[4,3]))
  ls3_SD <- sqrt(logit2prob(CEIN_tidy[1,3])) + sqrt(logit2prob(CEIN_tidy[2,3])) + sqrt(logit2prob(CEIN_tidy[5,3]))
  un_SD <- sqrt(logit2prob(CEIN_tidy[1,3])) + sqrt(logit2prob(CEIN_tidy[3,3]))
  
  SD <- unlist(c(hs1_SD, hs2_SD, hs3_SD, ls1_SD, ls2_SD, ls3_SD, un_SD))
  
  hab <- c("hs1", "hs2", "hs3", "ls1", "ls2", "ls3", 'un')
  
  
  plot_data <- data.frame(hab, mean, SD)
  
  p <- ggplot(plot_data, aes(x = hab, y = mean)) +
    geom_col() +
    geom_errorbar(aes(ymin=mean-SD, ymax=mean+SD), width=.2,
                  position=position_dodge(.9))
  
  return(p)
}

# I think there should only be 2 age categories for ls and hs rather than 3
## This works with sev_tsf
occ_plot_sevtsf <- function(mod) {
  CEIN_tidy <- tidy(mod)
  # I think there should only be 2 age categories for ls and hs rather than 3
  hs1_mean <- logit2prob(CEIN_tidy[1,2])
  hs2_mean <- logit2prob(CEIN_tidy[1,2]) + logit2prob(CEIN_tidy[2,2])
  hs3_mean <- logit2prob(CEIN_tidy[1,2]) + logit2prob(CEIN_tidy[3,2])
  ls1_mean <- logit2prob(CEIN_tidy[1,2]) + logit2prob(CEIN_tidy[4,2])
  ls2_mean <- logit2prob(CEIN_tidy[1,2]) + logit2prob(CEIN_tidy[5,2])
  ls3_mean <- logit2prob(CEIN_tidy[1,2]) + logit2prob(CEIN_tidy[6,2])
  un_mean <- logit2prob(CEIN_tidy[1,2]) + logit2prob(CEIN_tidy[7,2])
  
  mean <- unlist(c(hs1_mean, hs2_mean, hs3_mean, ls1_mean, ls2_mean, ls3_mean, un_mean))
  
  hs1_SD <- sqrt(logit2prob(CEIN_tidy[1,3]))
  hs2_SD <- sqrt(logit2prob(CEIN_tidy[1,3])) + sqrt(logit2prob(CEIN_tidy[2,3]))
  hs3_SD <- sqrt(logit2prob(CEIN_tidy[1,3])) + sqrt(logit2prob(CEIN_tidy[3,3]))
  ls1_SD <- sqrt(logit2prob(CEIN_tidy[1,3])) + sqrt(logit2prob(CEIN_tidy[4,3]))
  ls2_SD <- sqrt(logit2prob(CEIN_tidy[1,3])) + sqrt(logit2prob(CEIN_tidy[5,3]))
  ls3_SD <- sqrt(logit2prob(CEIN_tidy[1,3])) + sqrt(logit2prob(CEIN_tidy[6,3]))
  un_SD <- sqrt(logit2prob(CEIN_tidy[1,3])) + sqrt(logit2prob(CEIN_tidy[7,3]))
  
  SD <- unlist(c(hs1_SD, hs2_SD, hs3_SD, ls1_SD, ls2_SD, ls3_SD, un_SD))
  
  hab <- c("hs1", "hs2", "hs3", "ls1", "ls2", "ls3", 'un')
  
  
  plot_data <- data.frame(hab, mean, SD)
  
  p <- ggplot(plot_data, aes(x = hab, y = mean)) +
    geom_col() +
    geom_errorbar(aes(ymin=mean-SD, ymax=mean+SD), width=.2,
                  position=position_dodge(.9))
  
  return(p)
}

# sev only/ NOT WORKING; maybe something with the link function, or the logit2prob function



occ_plot_sev <- function(mod) {
  # This works nicely but couldn't extract the values to make a graph
  #A <- allEffects(sp_mod1)
  #summary(A)
  # OR 
  #X <- predictorEffects((sp_mod1))
  #see also ?confint
  
  #hs
  predicted = predict(mod, data.frame(sev='h'), type='link', se.fit=TRUE)
  
  hs_mean = logit2prob(predicted$fit)
  hs_CI_high = logit2prob(predicted$fit + (predicted$se.fit*1.96)) - hs_mean 
  hs_CI_low = hs_mean - logit2prob(predicted$fit - (predicted$se.fit*1.96))
  
  #ls
  predicted = predict(mod, data.frame(sev='l'), type='link', se.fit=TRUE)
  ls_mean = logit2prob(predicted$fit)
  ls_CI_high = logit2prob(predicted$fit + (predicted$se.fit*1.96)) - ls_mean 
  ls_CI_low = ls_mean - logit2prob(predicted$fit - (predicted$se.fit*1.96))
  
  #ls
  predicted = predict(mod, data.frame(sev='u'), type='link', se.fit=TRUE)
  un_mean = logit2prob(predicted$fit)
  un_CI_high = logit2prob(predicted$fit + (predicted$se.fit*1.96)) - un_mean 
  un_CI_low = un_mean -logit2prob(predicted$fit - (predicted$se.fit*1.96))
  
  mean <- unlist(c(hs_mean, ls_mean, un_mean))
  CI_high <- unlist(c(hs_CI_high, ls_CI_high, un_CI_high))
  CI_low <- unlist(c(hs_CI_low, ls_CI_low, un_CI_low))
  
  hab <- c("HS", "LS", "UN")
  
  plot_data <- data.frame(hab, mean, CI_high, CI_low)
  
  p <- ggplot(plot_data, aes(x = hab, y = mean)) +
    geom_col() +
    geom_errorbar(aes(ymin=mean-CI_low, ymax=mean+CI_high), width=.2,
                  position=position_dodge(.9))
  
  return(p)
}

#tsf_cat only (3 categories plus UN)
occ_plot_tsf <- function(mod) {
  CEIN_tidy <- tidy(mod)
  # I think there should only be 2 age categories for ls and hs rather than 3
  mean1 <- logit2prob(CEIN_tidy[1,2])
  mean2 <- logit2prob(CEIN_tidy[1,2]) + logit2prob(CEIN_tidy[2,2])
  mean3 <- logit2prob(CEIN_tidy[1,2]) + logit2prob(CEIN_tidy[3,2])
  un_mean <- logit2prob(CEIN_tidy[1,2]) + logit2prob(CEIN_tidy[4,2])
  
  mean <- unlist(c(mean1, mean2, mean3, un_mean))
  
  SD1 <- sqrt(logit2prob(CEIN_tidy[1,3]))
  SD2 <- sqrt(logit2prob(CEIN_tidy[1,3])) + sqrt(logit2prob(CEIN_tidy[2,3]))
  SD3 <- sqrt(logit2prob(CEIN_tidy[1,3])) + sqrt(logit2prob(CEIN_tidy[3,3]))
  un_SD <- sqrt(logit2prob(CEIN_tidy[1,3])) + sqrt(logit2prob(CEIN_tidy[4,3]))
  
  SD <- unlist(c(SD1, SD2, SD3, un_SD))
  
  hab <- c("1", "2", "3", 'un')
  
  
  plot_data <- data.frame(hab, mean, SD)
  
  p <- ggplot(plot_data, aes(x = hab, y = mean)) +
    geom_col() +
    geom_errorbar(aes(ymin=mean-SD, ymax=mean+SD), width=.2,
                  position=position_dodge(.9))
  
  return(p)
}

#full with 2 tsf categories
occ_plot_full2cat <- function(mod) {
  CEIN_tidy <- tidy(mod)
  # I think there should only be 2 age categories for ls and hs rather than 3
  hs1_mean <- logit2prob(CEIN_tidy[1,2])
  hs2_mean <- logit2prob(CEIN_tidy[1,2]) + logit2prob(CEIN_tidy[4,2])
  ls1_mean <- logit2prob(CEIN_tidy[1,2]) + logit2prob(CEIN_tidy[2,2])
  ls2_mean <- logit2prob(CEIN_tidy[1,2]) + logit2prob(CEIN_tidy[2,2]) + logit2prob(CEIN_tidy[4,2])
  un_mean <- logit2prob(CEIN_tidy[1,2]) + logit2prob(CEIN_tidy[3,2])
  
  mean <- unlist(c(hs1_mean, hs2_mean, ls1_mean, ls2_mean, un_mean))
  
  hs1_SD <- sqrt(logit2prob(CEIN_tidy[1,3]))
  hs2_SD <- sqrt(logit2prob(CEIN_tidy[1,3])) + sqrt(logit2prob(CEIN_tidy[4,3]))
  ls1_SD <- sqrt(logit2prob(CEIN_tidy[1,3])) + sqrt(logit2prob(CEIN_tidy[2,3]))
  ls2_SD <- sqrt(logit2prob(CEIN_tidy[1,3])) + sqrt(logit2prob(CEIN_tidy[2,3])) + sqrt(logit2prob(CEIN_tidy[4,3]))
  un_SD <- sqrt(logit2prob(CEIN_tidy[1,3])) + sqrt(logit2prob(CEIN_tidy[3,3]))
  
  SD <- unlist(c(hs1_SD, hs2_SD, ls1_SD, ls2_SD, un_SD))
  
  hab <- c("hs1", "hs2", "ls1", "ls2", 'un')
  
  
  plot_data <- data.frame(hab, mean, SD)
  
  p <- ggplot(plot_data, aes(x = hab, y = mean)) +
    geom_col() +
    geom_errorbar(aes(ymin=mean-SD, ymax=mean+SD), width=.2,
                  position=position_dodge(.9))
  
  return(p)
}


#tsf_cat only (2 categories plus UN)
occ_plot_tsf_cat2 <- function(mod) {
  
  # 1
  predicted = predict(mod, data.frame(tsf_cat2='1'), type='link', se.fit=TRUE)
  
  mean1 = logit2prob(predicted$fit)
  CI_high1 = logit2prob(predicted$fit + (predicted$se.fit*1.96)) - mean1
  CI_low1 = mean1 - logit2prob(predicted$fit - (predicted$se.fit*1.96))
  
  # 2
  predicted = predict(mod, data.frame(tsf_cat2='2'), type='link', se.fit=TRUE)
  mean2 = logit2prob(predicted$fit)
  CI_high2 = logit2prob(predicted$fit + (predicted$se.fit*1.96)) - mean2 
  CI_low2 = mean2 - logit2prob(predicted$fit - (predicted$se.fit*1.96))
  
  # 3
  predicted = predict(mod, data.frame(tsf_cat2='3'), type='link', se.fit=TRUE)
  mean3 = logit2prob(predicted$fit)
  CI_high3 = logit2prob(predicted$fit + (predicted$se.fit*1.96)) - mean3 
  CI_low3 = mean3 -logit2prob(predicted$fit - (predicted$se.fit*1.96))
  
  mean <- unlist(c(mean1, mean2, mean3))
  CI_high <- unlist(c(CI_high1, CI_high2, CI_high3))
  CI_low <- unlist(c(CI_low1, CI_low2, CI_low3))
  
  hab <- c("0-15", "15-31", "UN")
  
  plot_data <- data.frame(hab, mean, CI_high, CI_low)
  
  p <- ggplot(plot_data, aes(x = hab, y = mean)) +
    geom_col() +
    geom_errorbar(aes(ymin=mean-CI_low, ymax=mean+CI_high), width=.2,
                  position=position_dodge(.9))
  
  return(p)
}

