 
setwd("~/Dropbox/results11Mar/")
data_files <- list.files(pattern = "\\.csv$")

repro_filenames <- data_files[((length(data_files)/3)+1):(2*(length(data_files)/3))]   
state_filenames <- data_files[(2*(length(data_files)/3)+1):(3*(length(data_files)/3))]
length_filenames <- data_files[1:(length(data_files)/3)]

length_filenames <- unique(length_filenames)
repro_filenames <- unique(repro_filenames)
state_filenames <- unique(state_filenames)

length_data <- lapply(length_filenames, read.csv)
repro_data <- lapply(repro_filenames, read.csv)
state_data <- lapply(state_filenames, read.csv)


test_length <- read.csv("~/Dropbox/results2Apr/01LengthTemp293c10Kappa1.7.csv")
test_repro <- read.csv("~/Dropbox/results2Apr/02ReproTemp293c10Kappa1.7.csv")
test_state <- read.csv("~/Dropbox/results2Apr/03StateTemp293c10Kappa1.7.csv")

 
     	 
 mort.est <- function(idata, filename) {
     alive_at_age<- 10000.001 - colSums(apply(idata[,-1],2, is.na ))
      age=1:64
      
      mu_a <-  log(alive_at_age[age[-64]+1]/  alive_at_age[age[-64]])
      asp <- which(cumprod(mu_a)<=0.01) 
     #mu_a[asp:length(mu_a)]=-5  
      
     m1<- lm(log(alive_at_age) ~ age) 	
 	 mu <- as.numeric(coef(m1)[2])
 	 
 	 #c(filename, exp(mu), exp(mu_a))
 	 mu
 	 
 	 }
 	 
 
mu_K <- mapply(mort.est, state_data, state_filenames)
 
 
 #find mean of all length datasets at age 3
 mean_L <- function(data) {
 	
 	mean(data[, 13], na.rm=TRUE) #take mean at age 3
 }	 
 
mean_kappa <- mapply(mean_L, length_data)
 
 
 #find sd of all length datasets at age 3
 sd_L <- function(data) {
 	
 	sd(data[, 13], na.rm=TRUE) #take mean at age 3
 }	 
 
 sd_kappa <- mapply(sd_L, length_data)
 
 
#sample lengths and statistics
obs_length <-  test_length[sample(1:nrow(test_length), 1000), 12]
J <- sum(obs_length>0, na.rm=TRUE)
mean_obs <- mean(obs_length, na.rm=TRUE)
sd_obs <- sd(obs_length, na.rm=TRUE)

kappa <- seq(0.5, 4, by=0.38)
N_0 <- 10000
#pooled SD for every Kappa
SD_pooled <- rep(0, length(kappa))
SDM <- rep(0, length(kappa))
 for (i in 1:length(kappa)) {
 	
 	SD_pooled[i] <- (((N_0 - 1)*sd_kappa[i]^2 + (J-1)*sd_obs^2)/(N_0+J))^0.5
 	
 	SDM[i] <- abs((mean_kappa[i] - mean_obs)/SD_pooled[i])
  	
 	} #end i loop
 	plot(kappa, SDM, type="l")
 	 
 	SDM_max <- max(SDM)
 	beta <- (-1/SDM_max)*log(0.05)
 	
 	
 	#######BSU######
 	#calculate weighting
 	  	weight_K  <- exp(-beta*SDM) 
 	  	
 	  	#calculate posterior
 	  	f0 = 1
 	  	
 	  	f1_K <- weight_K/sum(weight_K)
 		
  ############
  ###posterior plot
  plot(mu_K, f1_K, type="p", pch=19, xlim=c(-0.316, -0.30))
  abline(v = -0.3060629, lty=3)
 
 
 
 






 