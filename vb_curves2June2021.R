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
          ylab="Reproduction (J)",   xlab= "Age (years)", xaxt="n",  ylim=c(0, 3e+08), xlim=c(0.5, Tmax*12))
   
  maxsize <- (min(which(as.numeric(repro_data[1, -1]) == max(as.numeric(repro_data[1, -1]), na.rm=TRUE)))) + 1 
  #print(maxsize)
  age_m <- min(which(as.numeric(repro_data[1, -1]) >= 0.5*as.numeric(repro_data[1, maxsize])), na.rm=TRUE)/12 
  size_m <- length_data[1, round(age_m*12)]
 
  #print(size_m)
   legend("topleft", legend=c(paste0("Amat_50 is ", round(age_m+1, 2), " years"), paste0("Lmat50 is ", round(size_m), " cm")), bty="n")
  #print(round(age_m*12) )
}

mapply(plot_repro, repro_data, length_data, repro_filenames)

plot_state<- function(data,  filenames) {
  scale <-  4.2e+6 
  matplot(t(data[,-1])/scale, type="l", main= substr(filenames, 8, 23), col="red", lwd=1.75, lty=1,   
          ylab="state (kg)",   xlab= "Age (years)", xaxt="n", xlim=c(0.5, Tmax*12))
   
}

mapply(plot_state, state_data,  state_filenames)


#### now define functions to calculate metrics
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


repro_metrics <-  function(reprodata, survdata, filenames) {
  
  
  data4 <- reprodata[1, 6:169] 
  data5_ <- survdata$x
  data5 <- data5_[6:169]
  
  wtd_rep <- mean(data4[, 2]*data5, na.rm=TRUE)
  
  params <- filenames
  
}
RE <- mapply(repro_metrics, repro_data, surv_data, length_filenames)      


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

# 
fit_age <- 13:182 #note we are focused only on growth from 1 year to 15 years of age..... 
# 
# 

 		
results <- mapply(calc_metrics, length_data, surv_data, length_filenames)
results <- gsub("r", "0",results)
###define the environmental context for these results
  env=rep("constant", length(length_filenames))
  
  food_tab1 <-  substr(length_filenames, kappa_index+6[1],   kappa_index+8[1])
  food_tab  <- as.numeric(gsub("r", 0, food_tab1))*12
  
####create the dataframe summarizing the results of all metrics
  tabdata<- cbind(env,  food_tab, round(as.numeric(results[2, ])), round(as.numeric(results[3, ]),2),
                   round(as.numeric(results[4, ])), round(as.numeric(results[5, ]), 3))
  colnames(tabdata) <- c("env",  "Richness", "Max_length", "Weighted_mean_length", "Linf", "von_Bert_K")
  tabdata
   
####
 

 setwd("~/Documents/tuna_theory_paper/seasonal/290K/")

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
 
 
 RE <- mapply(repro_metrics, repro_data, surv_data, length_filenames)
 

 kappa_index <- regexpr("Kappa", length_filenames)
 mort_index <- regexpr("f_h", length_filenames)

env=rep("seasonal", length(length_filenames))

 food_tab1 <-  substr(length_filenames, kappa_index+6[1],   kappa_index+8[1])
 food_tab  <- as.numeric(gsub("r", 0, food_tab1))*18

 tabdata2<- cbind(env,  food_tab, round(as.numeric(results[2, ])), round(as.numeric(results[3, ]),2),
                 round(as.numeric(results[4, ])), round(as.numeric(results[5, ]), 3))
 colnames(tabdata2) <- c("env",  "Richness", "Max_length", "Weighted_mean_length", "Linf", "von_Bert_K")
 tabdata2

 total_data <- as.data.frame(rbind(tabdata, tabdata2))

 
 library(ggplot2)
 quartz()
 # ggplot(data = totaldata, aes(Richness, Max_length)) +
 #   geom_point( size = 3) +
 #   labs(title = "Maximum length (cm)", subtitle = "(limited to years 1:15)", y = "Maximum length", x = "K (Average annual richness)") + 	scale_shape_manual(values=c( 20,21))+
 #   scale_colour_manual(values=c("blue", "red" )) 
 
 ggplot(data = total_data, aes(Richness, Weighted_mean_length,  color = factor(Temp))) +
   geom_point( size = 3) +
   labs(title = "Mean length of population", subtitle = "(limited to years 1:15)", y =  "Mean length (cm)",  x = "Kappa (Average annual richness)") + 	scale_shape_manual(values=c( 20,21))+
   scale_colour_manual(values=c("blue", "red" )) +
   
   facet_grid(env ~ Temp) 
 
 ggplot(data = total_data, aes(Richness, Mean_RE,  color = factor(Temp))) +
   geom_point( size = 3) +
   labs(title = "Mean Reproductive Investment", subtitle = "(limited to years 2:15)", y =  "Energy devoted to reproduction (J)",  x = "Kappa (Average annual richness)") + 	scale_shape_manual(values=c( 20,21))+
   scale_colour_manual(values=c("blue", "red" )) +
   
   facet_grid(env ~ Temp) 
 

    