# install.packages("FSA")
# install.packages("nlstools")

 # install.packages("ggplot2")
library(FSA)
library(nlstools)
  

Tmax=18
time=1:(Tmax*12)
quartz()
windowframe=c(4,1)
 
par(mfrow=windowframe)

plot_length <- function(data, filenames) {
  
  
  matplot(t(data[,-1]), type="l", main=substr(filenames, 23, 31), col="darkgray", lwd=1.75, lty=1,  ylab="Length (cm)", ylim=c(0, 400), xlim=c(0.5, Tmax*12), xlab= "Age (years)", xaxt="n")
  axis(1, at = seq(0, (Tmax)*12, by=12), labels = (seq(1, Tmax+1, by=1)))
  
  maxsize <- (min(which(as.numeric(data[1, -1]) == max(as.numeric(data[1, -1]))))) + 1 
  #print(data[1, maxsize])
   #age_m <- min(which(as.numeric(data[1, -1]) >= 0.5*as.numeric(data[1, maxsize])))/12 
  
    
   legend("topleft", legend=paste0("Lmax is ", data[1, maxsize], " cm"), bty="n")
}


plot_repro <- function(repro_data, length_data, repro_filenames) {
  matplot(t(repro_data[,-1]), type="l", main= substr(repro_filenames, 8, 23), col="red", lwd=1.75, lty=1,   
          ylab="Reproduction (J)",   xlab= "Age (years)", xaxt="n",  ylim=c(0, 5e+08), xlim=c(0.5, Tmax*12))
   
  maxsize <- (min(which(as.numeric(repro_data[1, -1]) == max(as.numeric(repro_data[1, -1]), na.rm=TRUE)))) + 1 
  #print(maxsize)
  age_m <- min(which(as.numeric(repro_data[1, -1]) >= 0.5*as.numeric(repro_data[1, maxsize])), na.rm=TRUE)/12 
  size_m <- length_data[1, round(age_m*12)]
 
  #print(size_m)
   legend("topleft", legend=c(paste0("Amat_50 is ", round(age_m+1, 2), " years")), bty="n")
  #print(round(age_m*12) )
}


 
# 
fit_age <- 6:204 #note we are focused only on growth from 0.5 year to 18 years of age..... 
# 
# 
#### now define functions to calculate metrics
calc_metrics <-  function(sizedata, survdata, filenames) {
   
  data1 <- (as.data.frame(t(rbind(fit_age, sizedata[1, 2:199]))))
  data2 <- na.omit(data1)
  names(data2) <- c("age_m", "len")
  
  
  data3_ <- survdata$x
  data3 <- data3_[2:176]
  #names(data3) <- c("survival")
  maxage <- length(na.omit(data3))
  
  #SV <- vbStarts(len~age_m, data <- data2)
  SV <- list(Linf=max(data2[,2]), K = 0.2)
  #print(SV)
  
  
  #if (sd(sizedata[1, -1], na.rm=TRUE) == 0) fitVB <- c(sizedata[1, 2], 0) else {


    von_bert <- len~Linf*(1-exp(-K*(age_m)))
    fitVB <- c(coef(nls(von_bert, data = data2, start = SV)))
  #}
  
  wtd_size <- mean(data1[, 2]*data3, na.rm=TRUE)
  params <- filenames
  return(c(params, SV[[1]], wtd_size, fitVB, maxage/12 ))
  
} 

 

#point to the files you want to compare


setwd("~/Documents/tuna_theory_paper/constant/290K/rlim0.2/")

data_files <- list.files(pattern = "\\.csv$")

# 
# ## read in files
# 
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
quartz()
windowframe=c(4,1)

par(mfrow=windowframe)
mapply(plot_length, length_data, length_filenames)    
mapply(plot_repro, repro_data, length_data, repro_filenames)
 		
results <- mapply(calc_metrics, length_data, surv_data, length_filenames)
#results <- gsub("r", "0",results)
###define the environmental context for these results
  env=rep("Constant", length(length_filenames))
  kappa_index <- regexpr("Kappa", length_filenames) 
  food_tab1 <-  substr(length_filenames, kappa_index+6[1],   kappa_index+8[1])
  food_tab  <- as.numeric(gsub("r", 0, food_tab1))*12
  
####create the dataframe summarizing the results of all metrics
  tabdata<- cbind(env,  food_tab, round(as.numeric(results[2, ])), round(as.numeric(results[3, ]),2),
                   round(as.numeric(results[4, ])), round(as.numeric(results[5, ]), 3), round(as.numeric(results[6, ]), 3))
  colnames(tabdata) <- c("env",  "Richness", "Max_length", "Weighted_mean_length", "Linf", "von_Bert_K", "Lifespan")
  tabdata
  
  tabdata_filt <- transform(tabdata, Spectrum_coeff = as.numeric(Richness))
  # filter_data <- (tabdata_filt[tabdata_filt$Spectrum_coeff < 11, ])                        
  # 
  # 
  # 
  # library(ggplot2)
  # quartz()
  # ggplot(data = filter_data, aes(Spectrum_coeff, Max_length)) +
  #   geom_point( size = 4) +
  #   labs(y = "Maximum length (cm)", x = "K (Average annual richness)") + 	scale_shape_manual(values=c( 20,21))+
  #   scale_colour_manual(values=c("light blue")) +
  #   theme_bw() +
  #   theme_light()
  # 
####
  
  #####NOW DO IT AGAIN FOR SEASONAL ENVIRONMENT
 

 setwd("~/Documents/tuna_theory_paper/seasonal/295K/")

 data_files <- list.files(pattern = "\\.csv$")

 # 
 # ## read in files
 # 
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

 
 
 results <- mapply(calc_metrics, length_data, surv_data, length_filenames)
 
 
 quartz()
 windowframe=c(4,1)
 
 par(mfrow=windowframe)
 mapply(plot_length, length_data, length_filenames)    
 mapply(plot_repro, repro_data, length_data, repro_filenames)

 kappa_index <- regexpr("Kappa", length_filenames)
 mort_index <- regexpr("f_h", length_filenames)

env=rep("5 degrees hotter", length(length_filenames))
kaptotpa_index <- regexpr("Kappa", length_filenames) 
 food_tab1 <-  substr(length_filenames, kappa_index+6[1],   kappa_index+8[1])
 food_tab  <- as.numeric(gsub("r", 0, food_tab1))*12

 tabdata2<- cbind(env,  food_tab, round(as.numeric(results[2, ])), round(as.numeric(results[3, ]),2),
                 round(as.numeric(results[4, ])), round(as.numeric(results[5, ]), 3), round(as.numeric(results[6, ]), 3))
 colnames(tabdata) <- c("env",  "Richness", "Max_length", "Weighted_mean_length", "Linf", "von_Bert_K", "Lifespan")
 tabdata2

 total_data <- as.data.frame(rbind(tabdata, tabdata2))
 total_data <- transform(total_data, Spectrum_coeff = as.numeric(Richness))
    filter_data <- (total_data[total_data$Spectrum_coeff < 11, ])                        
total_data
  #   quartz()
  #   plot(filter_data$Spectrum_coeff[filter_data$env=="Constant"], filter_data$Max_length[filter_data$env=="Constant"], ylab="Max length (cm)", xlab="Spectrum richness K", ylim=c(90, 310), col =  "light blue", pch=20, cex=3 ) 
  #   points(filter_data$Spectrum_coeff[filter_data$env=="Seasonal"], filter_data$Max_length[filter_data$env=="Seasonal"],  col =  "dark blue", pch=20, cex=3 )
  #   
  #   
  #   quartz() 
  # 
  # plot(filter_data$Spectrum_coeff[filter_data$env=="Baseline"], filter_data$Max_length[filter_data$env=="Baseline"], ylab="Max length (cm)", xlab="Spectrum richness K", col =  "light blue", pch=20, cex=3 ) 
  # points(filter_data$Spectrum_coeff[filter_data$env=="Baseline"], filter_data$Max_length[filter_data$env!="Baseline"],  col =  "red", pch=20, cex=3 )
  # 
  #   quartz()
  #   
  #     plot(filter_data$Spectrum_coeff[filter_data$env!="Baseline"], filter_data$Max_length[filter_data$env!="Baseline"],  col =  "red", ylab="Max length (cm)", xlab="Spectrum richness K",ylim=c(90, 310),  pch=20, cex=3)
  #     points(seasonal_data290$Spectrum_coeff[seasonal_data290$env=="Seasonal"], seasonal_data290$Max_length[seasonal_data290$env=="Seasonal"],col="dark blue", pch=20, cex=3 )
  #     
  #  