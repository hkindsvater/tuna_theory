 
####in Feb 8 results, kappa (environment richness) and costs of spawning (c1) vary and overall very steep for individuals < 50
####in Feb 11 results, kappa (environemnt richness) and temperature vary and c0 = 0.1, c1 is 0.25, so spawning costs are VERY steep for small individuals
### in Feb 13 results, kappa and temperature vary, but there are no spawning costs (c2 = 0.1, c1=0)
### in Feb 19 results, kappa and temperature vary, but c0 is 0.05 (and c1 is 0.15), so spawning costs are severe for individuals < 80 cm - not that interersting
####in Feb 20 the metabolic costs get a lot steeper in warm envieonments, but there are no CoR
###in Feb 20.2 the maximum lifespan is much longer
###Feb 21 maximum lifespan is the same as Feb 20.2, kappa inclueds 4; max state is greater (375); also rep limit is 2
##March 11: same as feb 21 except variance in food is zero and max lifespan is shorter (16 years)


setwd("~/Documents/tuna_theory/model_output/storemin0.4")
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

 
 quartz()
  par(mfrow=c(4, 4))
 
plot_length <- function(data, filenames) {
	  #data[is.na(data)] <- 0
	  
	  
     matplot(t(data[,-1]), type="l", main=c(substr(filenames, 9, 15), substr(filenames, 16, 21)), col="darkgray", lwd=1.75, lty=1,  ylab="Length (cm)", ylim=c(0, 400), xlim=c(0.5, 48), xlab= "Age (years)", xaxt="n")
     axis(1, at = seq(0, 64, by=4), labels = (seq(1, 17, by=1)))
 
     }
     
 mapply(plot_length, length_data, length_filenames)    
     
     
  # plot_hist <- function(data, filenames) {
    # hist(data[, 12], breaks=50, xlim=c(50, 350), main=substr(filenames, 13, 49))
  # }
  

 

  # mapply(plot_hist, length_data, length_filenames)
  
  
 quartz()
  par(mfrow=c(4, 4))
  plot_repro <- function(repro_data, repro_filenames) {
	 
     matplot(t(repro_data[,-1]), type="l", main=c(substr(repro_filenames, 8, 14), substr(repro_filenames, 18, 23)), col="darkgray", lwd=1.75, lty=1,   ylab="Reproduction (J)",   xlab= "Age (years)", xaxt="n", ylim=c(0, 6e+08), xlim=c(1, 56))
     axis(1, at = seq(0, 60, by=4), labels = (seq(1, 16, by=1)))
 
     }

mapply(plot_repro, repro_data, repro_filenames)

##make a plot of reproductive output as a function of length
quartz()
  par(mfrow=c(4, 4))
 
 age.length <- function(length_data, repro_data, filenames) {
 	
 	matplot(t(length_data[,-1]), t(repro_data[,-1]), type="l", col="darkgray", lwd=1.75, lty=1, main=c(substr(filenames, 9, 15), substr(filenames, 16, 21)),  xlab="Length (cm)", xlim=c(0, 350), ylim=c(0, 1.5e+09), ylab="Reproduction (J)")
 	
 	} 
 	
mapply(age.length, length_data, repro_data, length_filenames)

 	
 	
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




 