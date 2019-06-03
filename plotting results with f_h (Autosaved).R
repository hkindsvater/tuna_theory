 
 


setwd("~/Documents/tuna_theory/model_output/")
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
 
 time=1:64
 
  quartz()
   par(mfrow=c(2,2))
 
plot_length <- function(data, filenames) {
	   
	   
     matplot(t(data[,-1]), type="l", main=substr(filenames, 9, 21), col="darkgray", lwd=1.75, lty=1,  ylab="Length (cm)", ylim=c(0, 400), xlim=c(0.5, 48), xlab= "Age (years)", xaxt="n")
     axis(1, at = seq(0, 64, by=4), labels = (seq(1, 17, by=1)))
     
     maxsize <- (min(which(as.numeric(data[1, -1]) == max(as.numeric(data[1, -1]))))) + 1 
     
     age_m <- min(which(as.numeric(data[1, -1]) >= 0.5*as.numeric(data[1, maxsize])))/4 
     
     legend("topright", legend=paste0("A_50 is ", age_m+1, " years"), bty="n")
          }
     
 mapply(plot_length, length_data, length_filenames)    
     
     
  # plot_hist <- function(data, filenames) {
    # hist(data[, 12], breaks=50, xlim=c(50, 350), main=substr(filenames, 13, 49))
  # }
   
  # mapply(plot_hist, length_data, length_filenames)
  
  
 quartz()
  par(mfrow=c(2, 2))
  plot_repro <- function(repro_data, repro_filenames) {
	 
     matplot(t(repro_data[,-1]), type="l", main= substr(repro_filenames, 8, 17), col="darkgray", lwd=1.75, lty=1,   ylab="Reproduction (J)",   xlab= "Age (years)", xaxt="n", ylim=c(0, 1e+09), xlim=c(0.5, 48))
     axis(1, at = seq(0, 60, by=4), labels = (seq(1, 16, by=1)))
 
     }

mapply(plot_repro, repro_data, repro_filenames)

##make a plot of reproductive output as a function of length
  # quartz()
   # par(mfrow=c(3, 3))
 
 age.length <- function(ldata1, rdata2, filenames) {
 	data1 <- as.numeric(ldata1[1,])
 	data2 <- as.numeric(rdata2[1, ])
 	#need to make a log log plot
 	matplot(t(log(data1[c(-1,-65)])), t(log(data2[c(-1,-65)])), type="p", col="darkgray", pch=20,   main= substr(filenames, 9, 21),  xlab="ln(Length) ", ylim=c(0, 25), xlim=c(0, 6), ylab="ln(Reproduction) ")
 	
 	m1<-lm(log(data2[c(-1,-65)])~log(data1[c(-1,-65)]))
 	if(is.na(coef(m1)[2])==FALSE)  	abline(m1)
 	#val<-paste0("Env is ", substr(filenames, 9, 23),", Slope is ", round(as.numeric(coef(m1)[2]), 3))
   legend("topleft",   legend=paste0("slope is ",round(as.numeric(coef(m1)[2]), 3)), bty="n")
 	   	} 
 	
mapply(age.length, length_data, repro_data, length_filenames)


quartz()
 par(mfrow=c(2,2))

 surv <- function(data, filenames) {
 	
 	  	
 	matplot(data[,-1], type="l", main= substr(filenames, 6, 17), col="darkgray", xlab="Age (years)", ylab="Survival", xaxt="n", ylim=c(0, 1.1), xlim=c(0.5, 48))
     axis(1, at = seq(0, 60, by=4), labels = (seq(1, 16, by=1)))
 
   
 }
 
 	mapply(surv, surv_data, surv_filenames)
 
  lnsurv <- function(data, filenames) {
 	
 	data1<-as.numeric(data[,2])
 	
 	matplot(log(data1[-1]), type="l", main= substr(filenames, 7, 19), col="darkgray", xlab="Age (years)", ylab="ln(Survival)", xaxt="n",  ylim=c(-20, 0), xlim=c(0.5, 48))
     axis(1, at = seq(0, 60, by=4), labels = (seq(1, 16, by=1)))
 
   m2<-lm(log(data1[-1])~time[-64])
   abline(m2)
   #print(paste0("Env is ", substr(surv_filenames, 8, 18))),", Slope is ", round(as.numeric(coef(m2)[2]), 3)))
   legend("bottomleft",   legend=paste0("slope is ",round(as.numeric(coef(m2)[2]), 3)), bty="n")
 }
 
 	mapply(lnsurv, surv_data, surv_filenames)
 
 
 	
 	#####now calculate the number alive as a function of age
     	 
# #  mort.est <- function(idata, filename) {
     # alive_at_age<- 10000.001 - colSums(apply(idata[,-1],2, is.na ))
      # age=1:64
      # plot(alive_at_age, type="l", lwd=2, ylab="Population size", main=c(substr(filename, 8, 14), substr(filename, 15, 20)), xlab= "Age (years)", xaxt="n", ylim=c(0, 10005), xlim=c(1, 56))
     # axis(1, at = seq(0, 60, by=4), labels = (seq(1, 16, by=1)))

      # mu_a <-  log(alive_at_age[age[-64]+1]/  alive_at_age[age[-64]])
      # asp <- min(which(cumprod(mu_a)<=0.01) )
     # mu_a[asp:length(mu_a)]=-5  
      
     # m1<- lm(log(alive_at_age) ~ age) 	
 	 # mu <- as.numeric(coef(m1)[2])
 	 # legend("topright",   legend=round(mu, 5), lty=1)
 	 # c(filename, exp(mu), exp(mu_a))
 	 # }
# quartz()
 # par(mfrow=c(2, 3))
# mapply(mort.est, state_data, state_filenames)




 