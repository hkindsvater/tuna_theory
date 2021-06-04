# install.packages("FSA")
# install.packages("nlstools")

 # install.packages("ggplot2")
library(FSA)
library(nlstools)
 
setwd("~/Documents/tuna_theory_paper/seasonal/290K")
 
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


Tmax=18
time=1:(Tmax*12)
quartz()
windowframe=c(4,1)
 
par(mfrow=windowframe)

plot_length <- function(data, filenames) {
  
  
  matplot(t(data[,-1]), type="l", main=substr(filenames, 23, 31), col="darkgray", lwd=1.75, lty=1,  ylab="Length (cm)", ylim=c(0, 300), xlim=c(0.5, Tmax*12), xlab= "Age (years)", xaxt="n")
  axis(1, at = seq(0, (Tmax)*12, by=12), labels = (seq(1, Tmax+1, by=1)))
  
  maxsize <- (min(which(as.numeric(data[1, -1]) == max(as.numeric(data[1, -1]))))) + 1 
  #print(data[1, maxsize])
   #age_m <- min(which(as.numeric(data[1, -1]) >= 0.5*as.numeric(data[1, maxsize])))/12 
  
    
   legend("topleft", legend=paste0("Lmax is ", data[1, maxsize], " cm"), bty="n")
}

mapply(plot_length, length_data, length_filenames)    

plot_repro <- function(repro_data, length_data, repro_filenames) {
  matplot(t(repro_data[,-1]), type="l", main= substr(repro_filenames, 8, 23), col="red", lwd=1.75, lty=1,   
          ylab="Reproduction (J)",   xlab= "Age (years)", xaxt="n",  ylim=c(0, 3e+07), xlim=c(0.5, Tmax*12))
   
  maxsize <- (min(which(as.numeric(repro_data[1, -1]) == max(as.numeric(repro_data[1, -1]), na.rm=TRUE)))) + 1 
  #print(maxsize)
  age_m <- min(which(as.numeric(repro_data[1, -1]) >= 0.5*as.numeric(repro_data[1, maxsize])), na.rm=TRUE)/12 
  size_m <- length_data[1, round(age_m*12)]
  #print(size_m)
  #legend("topleft", legend=c(paste0("Amat_50 is ", round(age_m+1, 2), " years"), paste0("Lmat50 is ", round(size_m), " cm")), bty="n")
  # print(round(age_m*12) )
}

mapply(plot_repro, repro_data, length_data, repro_filenames)

plot_state<- function(data,  filenames) {
  scale <-  4.2e+6 
  matplot(t(data[,-1])/scale, type="l", main= substr(filenames, 8, 23), col="red", lwd=1.75, lty=1,   
          ylab="state (kg)",   xlab= "Age (years)", xaxt="n", xlim=c(0.5, Tmax*12))
   
}

mapply(plot_state, state_data,  state_filenames)


fit_age <- 13:168 #note we are focused only on growth from 1 year to 14 years of age..... 

 
 calc_metrics <-  function(sizedata, survdata, filenames) {
 
 
	data2 <- as.data.frame(t(rbind(fit_age, sizedata[1, 2:157])))
	names(data2) <- c("age_m", "len")
	
	 
	data3_ <- survdata$x
	data3 <- data3_[2:157]
	#names(data3) <- c("survival")

	
	 #SV <- vbStarts(len~age_m, data <- data2)
	 SV <- list(Linf=max(data2[,2]), K = 0.2)
	 #print(SV)
	 
	 
	 if (sd(sizedata[1, -1]) == 0) fitVB <- c(sizedata[1, 2], 0) else {
	      
	    
	von_bert <- len~Linf*(1-exp(-K*(age_m)))
	fitVB <- c(coef(nls(von_bert, data = data2, start = SV)))
	 }

	wtd_size <- mean(data2[, 2]*data3)
	 params <- filenames
 return(c(params, SV[[1]], wtd_size, fitVB ))
 
 		} 
 		
 		
results <- mapply(calc_metrics, length_data, surv_data, length_filenames)
results <- gsub("r", "0",results)

   
   
 repro_metrics <-  function(reprodata, survdata, filenames) {
 
 
	data4 <- reprodata[1, 6:169] 
		 	 	data5_ <- survdata$x
	data5 <- data5_[6:169]
 
	wtd_rep <- mean(data4[, 2]*data5, na.rm=TRUE)
	# print(data4[,2])
	# print(data5)
	 params <- filenames
  
      }
  RE <- mapply(repro_metrics, repro_data, surv_data, length_filenames)      
  RE <- gsub("r", 0, RE)
 
  
  length_filenames <- gsub("r", 0, length_filenames)
  
 
  kappa_index <- regexpr("Kappa", length_filenames) 
  mort_index <- regexpr("f_h", length_filenames) 
  
 storelim=rep("0.6", length(length_filenames))
 
food_tab <- as.numeric(substr(length_filenames, kappa_index+6[1],   kappa_index+8[1]))*12 
 tabdata<- cbind(storelim,  food_tab, round(as.numeric(results[2, ])), round(as.numeric(results[3, ]),2), 
                 round(as.numeric(results[4, ])), round(as.numeric(results[5, ]), 3))
 colnames(tabdata) <- c("Store_limit",  "Richness", "Max_length", "Weighted_mean_length", "Linf", "von_Bert_K")
  tabdata <- as.data.frame(tabdata )
 
 library(ggplot2)
 quartz()
 ggplot(data = tabdata, aes(Richness, Max_length)) +
   geom_point( size = 3) +
   labs(title = "Maximum length (cm)", subtitle = "(limited to years 1:15)", y = "Maximum length", x = "K (Average annual richness)") + 	scale_shape_manual(values=c( 20,21))+
   scale_colour_manual(values=c("blue", "red" )) 
 
 
 
 
 # library(FSA)
 # library(nlstools)
 # 
 # setwd("~/Documents/tuna_theory_paper/jeremy_machine_run_results/finegrid/seasonal/storelim0.6")
 # 
 # data_files <- list.files(pattern = "\\.csv$")
 # 
 # 
 # ## read in files
 # 
 # repro_filenames <- data_files[((length(data_files)/4)+1):(2*(length(data_files)/4))]   
 # state_filenames <- data_files[(2*(length(data_files)/4)+1):(3*(length(data_files)/4))]
 # length_filenames <- data_files[1:(length(data_files)/4)]
 # surv_filenames <- data_files[(3*(length(data_files)/4)+1):(4*(length(data_files)/4))]
 # 
 # length_filenames <- unique(length_filenames)
 # repro_filenames <- unique(repro_filenames)
 # state_filenames <- unique(state_filenames)
 # surv_filenames <- unique(surv_filenames)
 # 
 # length_data <- lapply(length_filenames, read.csv)
 # repro_data <- lapply(repro_filenames, read.csv)
 # state_data <- lapply(state_filenames, read.csv)
 # surv_data <- lapply(surv_filenames, read.csv)
 # 
 # 
 # fit_age <- 13:182 #note we are focused only on growth from 1 year to 15 years of age..... 
 # 
 # 
 # calc_metrics <-  function(sizedata, survdata, filenames) {
 #   
 #   
 #   data2 <- as.data.frame(t(rbind(fit_age, sizedata[1, 2:169])))
 #   names(data2) <- c("age_m", "len")
 #   
 #   
 #   data3_ <- survdata$x
 #   data3 <- data3_[2:169]
 #   #names(data3) <- c("survival")
 #   
 #   
 #   #SV <- vbStarts(len~age_m, data <- data2)
 #   SV <- list(Linf=max(data2[,2]), K = 0.2)
 #   #print(SV)
 #   von_bert <- len~Linf*(1-exp(-K*(age_m)))
 #   fitVB <- nls(von_bert, data = data2, start = SV)
 #   
 #   
 #   wtd_size <- mean(data2[, 2]*data3)
 #   params <- filenames
 #   return(c(params, SV[[1]], wtd_size, coef(fitVB)[1], coef(fitVB)[2]) )
 #   
 # } 
 # 
 # 
 # results <- mapply(calc_metrics, length_data, surv_data, length_filenames)
 # results <- gsub("r", "0",results)
 # 
 # 
 # 
 # repro_metrics <-  function(reprodata, survdata, filenames) {
 #   
 #   
 #   data4 <- reprodata[1, 6:169] 
 #   data5_ <- survdata$x
 #   data5 <- data5_[6:169]
 #   
 #   wtd_rep <- mean(data4[, 2]*data5, na.rm=TRUE)
 #   # print(data4[,2])
 #   # print(data5)
 #   params <- filenames
 #   return(wtd_rep)
 # }
 # RE <- mapply(repro_metrics, repro_data, surv_data, length_filenames)      
 # RE <- gsub("r", 0, RE)
 # 
 # 
 # length_filenames <- gsub("r", 0, length_filenames)
 # 
 # 
 # kappa_index <- regexpr("Kappa", length_filenames) 
 # mort_index <- regexpr("f_h", length_filenames) 
 # 
 # storelim=rep("seasonal", length(length_filenames))
 # 
 # food_tab1 <-  substr(length_filenames, kappa_index+6[1],   kappa_index+8[1]) 
 # food_tab  <- as.numeric(gsub("r", 0, food_tab1))*18
 #               
 # tabdata2<- cbind(storelim,  food_tab, round(as.numeric(results[2, ])), round(as.numeric(results[3, ]),2), 
 #                 round(as.numeric(results[4, ])), round(as.numeric(results[5, ]), 3))
 # colnames(tabdata2) <- c("Store_limit",  "Richness", "Max_length", "Weighted_mean_length", "Linf", "von_Bert_K")
 # tabdata2
 # 
 # total_data <- as.data.frame(rbind(tabdata, tabdata2))
 
 
   
    