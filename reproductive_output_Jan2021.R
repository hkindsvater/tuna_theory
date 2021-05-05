# install.packages("FSA")
# install.packages("nlstools")

 # install.packages("ggplot2")
library(FSA)
library(nlstools)

setwd("~/Documents/tuna_theory_paper/jeremy_machine_run_results/Rlim.2/constant/")
 
data_files <- list.files(pattern = "\\.csv$")

 
## read in files

repro_filenames <- data_files[((length(data_files)/4)+1):(2*(length(data_files)/4))]   
state_filenames <- data_files[(2*(length(data_files)/4)+1):(3*(length(data_files)/4))]
length_filenames <- data_files[1:(length(data_files)/4)]
surv_filenames <- data_files[(3*(length(data_files)/4)+1):(4*(length(data_files)/4))]

length_filenames <- unique(length_filenames)
repro_filenames <- unique(repro_filenames)
state_filenames <- unique(state_filenames)
surv_filenames <- unique(surv_filenames)

length_data <- lapply(length_filenames, read.csv)
repro_data <- lapply(repro_filenames, read.csv)
state_data <- lapply(state_filenames, read.csv)
surv_data <- lapply(surv_filenames, read.csv)


fit_age <- 13:182 #note we are focused only on growth from 1 year to 15 years of age..... 
 
 
 calc_metrics <-  function(sizedata, survdata, filenames) {
 
 
	data2 <- as.data.frame(t(rbind(fit_age, sizedata[1, 2:169])))
	names(data2) <- c("age_m", "len")
	
	 
	data3_ <- survdata$x
	data3 <- data3_[2:169]
	#names(data3) <- c("survival")

	
	 #SV <- vbStarts(len~age_m, data <- data2)
	 SV <- list(Linf=max(data2[,2]), K = 0.2)
	 #print(SV)
	von_bert <- len~Linf*(1-exp(-K*(age_m)))
	fitVB <- nls(von_bert, data = data2, start = SV)


	wtd_size <- mean(data2[, 2]*data3)
	 params <- substr(filenames, 13, 31)
 return(c(params, SV[[1]], wtd_size, coef(fitVB)[1], coef(fitVB)[2]) )
 
 		} 
 		
 		
results <- mapply(calc_metrics, length_data, surv_data, length_filenames)
results <- gsub("r", "0",results)

seasonal<- rep(0, 34) 
temp_tab <- as.numeric(substr(results[1, ], 1, 3))
mort_tab <- as.numeric(substr(results[1, ], 7, 10))*12
food_tab <- as.numeric(substr(results[1, ], 16, 19))*12 
 tabdata<- cbind(seasonal, temp_tab, mort_tab, food_tab, round(as.numeric(results[2, ])), round(as.numeric(results[3, ]),2), round(as.numeric(results[4, ])), round(as.numeric(results[5, ]), 3))
 colnames(tabdata) <- c("Seasonality", "Temp", "Predator_fullness", "Richness", "Max_length", "Weighted_mean_length", "Linf", "von_Bert_K")
 tabdata


 ###now repeat this for seasonal results 
 setwd("~/Documents/tuna_theory_paper/jeremy_machine_run_results/Rlim.2/seasonal/")
 
data_files <- list.files(pattern = "\\.csv$")

 
## read in files

repro_filenames <- data_files[((length(data_files)/4)+1):(2*(length(data_files)/4))]   
state_filenames <- data_files[(2*(length(data_files)/4)+1):(3*(length(data_files)/4))]
length_filenames <- data_files[1:(length(data_files)/4)]
surv_filenames <- data_files[(3*(length(data_files)/4)+1):(4*(length(data_files)/4))]

length_filenames <- unique(length_filenames)
repro_filenames <- unique(repro_filenames)
state_filenames <- unique(state_filenames)
surv_filenames <- unique(surv_filenames)

length_data <- lapply(length_filenames, read.csv)
repro_data <- lapply(repro_filenames, read.csv)
state_data <- lapply(state_filenames, read.csv)
surv_data <- lapply(surv_filenames, read.csv)


fit_age <- 13:182 #note we are focused only on growth from 1 year to 15 years of age..... 
 
 
 calc_metrics <-  function(sizedata, survdata, filenames) {
 
 
	data2 <- as.data.frame(t(rbind(fit_age, sizedata[1, 2:169])))
	names(data2) <- c("age_m", "len")
	
	 
	data3_ <- survdata$x
	data3 <- data3_[2:169]
	#names(data3) <- c("survival")

	
	 #SV <- vbStarts(len~age_m, data <- data2)
	 SV <- list(Linf=max(data2[,2]), K = 0.2)
	 #print(SV)
	von_bert <- len~Linf*(1-exp(-K*(age_m)))
	fitVB <- nls(von_bert, data = data2, start = SV)


	wtd_size <- mean(data2[, 2]*data3)
	 params <- substr(filenames, 13, 31)
 return(c(params, SV[[1]], wtd_size, coef(fitVB)[1], coef(fitVB)[2]) )
 
 		} 
 		
 		
results <- mapply(calc_metrics, length_data, surv_data, length_filenames)
results <- gsub("r", "0",results)

seasonal<- rep(1, 34) 
temp_tab <- as.numeric(substr(results[1, ], 1, 3))
mort_tab <- as.numeric(substr(results[1, ], 7, 10))*12
food_tab <- as.numeric(substr(results[1, ], 16, 19))*18  
 #NOTE for the seasonal results I shifted the average richness of the ecosystem.  
 tabdata_s<- cbind(seasonal, temp_tab, mort_tab, food_tab, round(as.numeric(results[2, ])), round(as.numeric(results[3, ]),2), round(as.numeric(results[4, ])), round(as.numeric(results[5, ]), 3))
 colnames(tabdata_s) <- c("Seasonality", "Temp", "Predator_fullness", "Richness", "Max_length", "Weighted_mean_length", "Linf", "von_Bert_K")
 tabdata_s
 
 total_data <- as.data.frame(rbind(tabdata, tabdata_s)) 
 
 #now calculate Phi as Marc defined it: Phi =log(L_inf) + log(k)
 
total_data$Phi <- log(total_data$Linf) + log(total_data$von_Bert_K)
 
 library(ggplot2)
 
 ggplot(data = total_data, aes(Richness, Max_length, color = factor(Temp))) +
 	geom_point( size = 3) +
 	labs(title = "Maximum length (cm)", subtitle = "(limited to years 1:15)", y = "Maximum length", x = "K (Average annual richness)") + 	scale_shape_manual(values=c( 20,21))+
      scale_colour_manual(values=c("blue", "red" ))+
      
      facet_grid(Seasonality ~ Temp)
       
 
  ggplot(data = total_data, aes(Richness, Linf,  color = factor(Temp))) +
 	geom_point( size = 3) +
 	labs(title = "Asymptotic length (cm)", subtitle = "(limited to years 1:15)", y = "L_inf", x = "K (Average annual richness)") + 	scale_shape_manual(values=c( 20,21))+
      scale_colour_manual(values=c("blue", "red" ))+
      	 
      facet_grid(Seasonality ~ Temp) 


  ggplot(data = total_data, aes(Richness, von_Bert_K,  color = factor(Temp))) +
 	geom_point( size = 3) +
 	labs(title = "Von Bertalanffy growth coefficient (k)", subtitle = "(limited to years 1:15)", y = "k", x = "Kappa (Average annual richness)") + 	scale_shape_manual(values=c( 20,21))+
      scale_colour_manual(values=c("blue", "red" ))+
       
      facet_grid(Seasonality ~ Temp) 

  
  ggplot(data = total_data, aes(Richness, Weighted_mean_length,  color = factor(Temp))) +
 	geom_point( size = 3) +
 	labs(title = "Mean length of population", subtitle = "(limited to years 1:15)", y =  "Mean length (cm)",  x = "Kappa (Average annual richness)") + 	scale_shape_manual(values=c( 20,21))+
      scale_colour_manual(values=c("blue", "red" )) +
       
      facet_grid(Seasonality ~ Temp) 

 
   
      
      
       

