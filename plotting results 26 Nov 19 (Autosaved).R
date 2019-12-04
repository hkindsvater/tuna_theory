 
 


setwd("~/Documents/tuna_theory/HiCosts/Temp290/seasons/Lmaxsens")
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
 windowframe=c(3,1)
  
  
   
  
  
  quartz()
   par(mfrow=windowframe)
 
plot_length <- function(data, filenames) {
	   
	   
     matplot(t(data[,-1]), type="l", main=substr(filenames, 9, 30), col="darkgray", lwd=1.75, lty=1,  ylab="Length (cm)", ylim=c(0, 400), xlim=c(0.5, Tmax*12), xlab= "Age (years)", xaxt="n")
     axis(1, at = seq(0, 220, by=12), labels = (seq(1, 19, by=1)))
     
     maxsize <- (min(which(as.numeric(data[1, -1]) == max(as.numeric(data[1, -1]))))) + 1 
     
     age_m <- min(which(as.numeric(data[1, -1]) >= 0.5*as.numeric(data[1, maxsize])))/12 
     
   
     legend("topleft", legend=paste0("Lmax is ", round(data[1, maxsize]), " cm"), bty="n")
          }
      mapply(plot_length, length_data, length_filenames)    
     
     
  # plot_hist <- function(data, filenames) {
    # hist(data[, 12], breaks=50, xlim=c(50, 350), main=substr(filenames, 13, 49))
  # }
   
  # mapply(plot_hist, length_data, length_filenames)
  
  
 quartz()
   par(mfrow=windowframe)
  plot_repro <- function(repro_data, length_data, repro_filenames) {
	       matplot(t(repro_data[,-1]), type="l", main= substr(repro_filenames, 8, 23), col="darkgray", lwd=1.75, lty=1,   ylab="Reproduction (J)",   xlab= "Age (years)", xaxt="n", ylim=c(0, 5.5e+08), xlim=c(0.5, Tmax*12))
     axis(1, at = seq(0, 220, by=12), labels = (seq(1, 19, by=1)))
     
     maxsize <- (min(which(as.numeric(repro_data[1, -1]) == max(as.numeric(repro_data[1, -1]), na.rm=TRUE)))) + 1 
     #print(maxsize)
      age_m <- min(which(as.numeric(repro_data[1, -1]) >= 0.5*as.numeric(repro_data[1, maxsize])), na.rm=TRUE)/12 
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

fec.exp <-function(ldata1, rdata2, filenames) {
 	data1 <- as.numeric(ldata1[1,-1])
 	data2 <- as.numeric(rdata2[1, -1])
 	  	matplot(t(log(data1[-216])), t(log(data2[-216])), type="p", col="darkgray", pch=20,   main= substr(filenames, 9, 30),  xlab="ln(Length) ", ylim=c(0, 25), xlim=c(0, 7), ylab="ln(Reproduction) ")
 	 	  max.rep.age = as.numeric(which.max(data2))
      
 	  print(max.rep.age)
 
 	  min.rep.age = as.numeric(which.min(data2[-216])) 
 	  print(min.rep.age)
 	   m1<-lm(log(data2[c(min.rep.age:max.rep.age)])~log(data1[c(min.rep.age:max.rep.age)]))
 	   
 	   b= (log(data2[max.rep.age])-log(data2[min.rep.age])) / (log(data1[max.rep.age]) - log(data1[min.rep.age]) )
 	    a=0
 	  if(is.na(coef(m1)[2])==FALSE) abline(m1)
 	 
 	 #abline(a, b)
 	 #print(coef(m1)[2])
 	#val<-paste0("Env is ", substr(filenames, 9, 23),", Slope is ", round(as.numeric(coef(m1)[2]), 3))
    
    legend("topleft",   legend=paste0("slope is ", round(b, 3)), bty="n") #round(as.numeric(coef(m1)[2]), 3)), bty="n")
 	   	} 

mapply(fec.exp, length_data, repro_data, length_filenames)

 

quartz()
  par(mfrow=windowframe)
 surv <- function(data, filenames) {
 	
 	  	
 	matplot(data[,-1], type="l", main= substr(filenames, 6, 25), col="darkgray", xlab="Age (years)", ylab="Survival", xaxt="n", ylim=c(0, 1.1),xlim=c(0.5, Tmax*12))
     axis(1, at = seq(0, 220, by=12), labels = (seq(1, 19, by=1)))
     
 
   
 }
 
 	mapply(surv, surv_data, surv_filenames)
 
    lnsurv <- function(data, filenames) {
 	
 	data1<-as.numeric(data[,2])
 	print(dim(data1))
 	matplot(log(data1[-1]), type="l", main= substr(filenames, 7, 30), col="darkgray", xlab="Age (years)", ylab="ln(Survival)", xaxt="n",  ylim=c(-10, 0), xlim=c(0.5, Tmax*12))
     axis(1, at = seq(0, 220, by=12), labels = (seq(1, 19, by=1))) 
   m2<-lm(log(data1[-1])~time[-length(time)])
   abline(m2)
   print(paste0("Slope is ", round(as.numeric(coef(m2)[2]), 3)))
   legend("bottomleft",   legend=paste0("slope is ",round(as.numeric(coef(m2)[2])*12, 3)), bty="n")
 }
 
 mapply(lnsurv, surv_data, surv_filenames)
 
 

 