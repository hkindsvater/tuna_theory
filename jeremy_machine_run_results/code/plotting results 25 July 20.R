 
 


 
#setwd("~/Desktop/holly_results/Temp290/constant/size_costs_threshold7")
 
 setwd("~/Desktop/holly_results/Temp290/seasonal/size_costs/threshold7/Tmax18")

 
data_files <- list.files(pattern = "\\.csv$")

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
 
 windowframe=c(2,3)
  sel_col=-c(1,Tmax*12, (Tmax*12)+1) 
  quartz()
   par(mfrow=windowframe)
 
plot_length <- function(data, filenames) {
	
     matplot(t(data[,sel_col]), type="l", main=substr(filenames, 9, 38), col="darkgray", lwd=1.75, lty=1,  ylab="Length (cm)", ylim=c(0, 400), xlim=c(0.5, Tmax*12), xlab= "Age (years)", xaxt="n")
     axis(1, at = seq(0, (Tmax-1)*12, by=12), labels = (seq(1, (Tmax-1)+1, by=1)))
     
     maxsize <- (min(which(as.numeric(data[1, sel_col]) == max(as.numeric(data[1, sel_col]), na.rm=TRUE )))) + 1 
     
     age_m <- min(which(as.numeric(data[1, sel_col]) >= 0.5*as.numeric(data[1, maxsize])))/12 
     
   
     legend("topleft", legend=paste0("Lmax is ", round(data[1, maxsize]), " cm"), bty="n")
          }
      mapply(plot_length, length_data, length_filenames)    
     
     
  # plot_hist <- function(data, filenames) {
    # hist(data[, 12], breaks=50, xlim=c(50, 350), main=substr(filenames, 13, 49))
  # }
   
  # mapply(plot_hist, length_data, length_filenames)
  
# #   
 quartz()
   par(mfrow=windowframe)
  plot_repro <- function(repro_data, length_data, repro_filenames) {
	       matplot(t(repro_data[,sel_col]), type="l", main= substr(repro_filenames, 8, 23), col="darkgray", lwd=1.75, lty=1,   ylab="Reproduction (J)",   xlab= "Age (years)", xaxt="n", ylim=c(0, 5.5e+08), xlim=c(0.5, Tmax*12))
     axis(1, at = seq(0, 220, by=12), labels = (seq(1, 19, by=1)))
     
     maxsize <- (min(which(as.numeric(repro_data[1, sel_col]) == max(as.numeric(repro_data[1, sel_col]), na.rm=TRUE)))) + 1 
     #print(maxsize)
      age_m <- min(which(as.numeric(repro_data[1, sel_col]) >= 0.5*as.numeric(repro_data[1, maxsize])), na.rm=TRUE)/12 
      size_m <- length_data[1, round(age_m*12)]
      #print(size_m)
    legend("topleft", legend=c(paste0("Amat_50 is ", round(age_m+1, 2), " years"), paste0("Lmat50 is ", round(size_m), " cm")), bty="n")
   # print(round(age_m*12) )
     }

mapply(plot_repro, repro_data, length_data, repro_filenames)

  quartz()
   par(mfrow=windowframe)
  plot_state <- function(state_data, state_filenames) {
	 
     matplot(t(state_data[,-1]), type="l", main= substr(state_filenames, 8, 23), col="darkgray", lwd=1.75, lty=1,   ylab="State (J)",   xlab= "Age (years)", xaxt="n", ylim=c(0, 6e+08), xlim=c(0.5, Tmax*12))
     axis(1, at = seq(0, 220, by=12), labels = (seq(1, 19, by=1)))
  
 
     }

mapply(plot_state, state_data, state_filenames)
  

quartz()
  par(mfrow=windowframe)
 surv <- function(data, filenames) {
 	
 	  	
 	matplot(data[,-1], type="l", main= substr(filenames, 6, 25), col="darkgray", xlab="Age (years)", ylab="Survival", xaxt="n", ylim=c(0, 1.1),xlim=c(0.5, Tmax*12))
     axis(1, at = seq(0, 220, by=12), labels = (seq(1, 19, by=1)))
     
 
   
 }
 
 	mapply(surv, surv_data, surv_filenames)
 
 quartz()
  par(mfrow=windowframe)
    lnsurv <- function(data, filenames) {
 	
 	data1<-as.numeric(data[,2])
 	print(dim(data1))
 	matplot(log(data1[-1]), type="l", main= substr(filenames, 7, 30), col="darkgray", xlab="Age (years)", ylab="ln(Survival)", xaxt="n",  ylim=c(-10, 0), xlim=c(0.5, Tmax*12))
     axis(1, at = seq(0, 220, by=12), labels = (seq(1, 19, by=1))) 
   m2<-lm(log(data1[sel_col])~time[sel_col])
   abline(m2)
 
   legend("bottomleft",   legend=paste0("slope is ",round(as.numeric(coef(m2)[2])*12, 3)), bty="n")
 }
 
 mapply(lnsurv, surv_data, surv_filenames)
 
 

 