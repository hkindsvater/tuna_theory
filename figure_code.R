install.packages("readr")
install.packages("ggplot2")


library(readr)
library(ggplot2)

rate_dat<-read_csv("~/Documents/tuna_theory/data_summary.csv") #bring in the summary spreadsheet I made of trait data

quartz()
a <- ggplot(rate_dat,
    aes(x=Predation, y=Amat) 
    ) + 
	geom_point(aes(colour = Temp, shape=Food), size = 3 ) +
 
        scale_size_manual(values=c(4, 1)) +

   scale_shape_manual(values=c(15, 20)) +
   scale_color_manual(values = c("blue", "red")) +
   labs(y="Age of 50% Maturity")+
 
  
facet_wrap(~Seasonality) 
a





quartz()
b <- ggplot(rate_dat,
    aes(x=Predation, y=Fecexp) 
    ) + 
	geom_point(aes(colour = Temp, shape=Food), size = 3 ) +
 
        scale_size_manual(values=c(4, 1)) +

   scale_shape_manual(values=c(15, 20)) +
   scale_color_manual(values = c("blue", "red")) +
   labs(y="Fecundity exponent")+

  
facet_wrap(~Seasonality) 
b



quartz()
c <- ggplot(rate_dat,
    aes(x=Predation, y=Mu_annual) 
    ) + 
	geom_point(aes(colour = Temp, shape=Food), size = 3 ) +
 
        scale_size_manual(values=c(4, 1)) +

   scale_shape_manual(values=c(15, 20)) +
   scale_color_manual(values = c("blue", "red")) +
   labs(y="Annual mortlaity rate (M)")+

  
facet_wrap(~Seasonality) 
c
 
 
 
 quartz()
d <- ggplot(rate_dat,
    aes(x=Predation, y=bodysize) 
    ) + 
	geom_point(aes(colour = Temp, shape=Food), size = 3 ) +
 
        scale_size_manual(values=c(4, 1)) +

   scale_shape_manual(values=c(15, 20)) +
   scale_color_manual(values = c("blue", "red")) +
   labs(y="Maximum Length") +

  
facet_wrap(~Seasonality) 
d
 
 
 
 
 
 
 

 setwd("~/Documents/tuna_theory/Length_data/HighmortHighfood/")
 
 HimuHifood <- list.files(pattern =  "\\.csv")

 HHdata <- lapply(HimuHifood, read.csv)
 
datamat=matrix(nrow=216, ncol=4)
bodysize=rep(NA, 4)
for (i in 1:4) {
	
    	datamat[, i]<-as.numeric(HHdata[[i]][1, -1])
	  bodysize[i]<-max(as.numeric(HHdata[[i]][1, -1]))
}


matplot(datamat, type="l", lty=1, col=c(1, 4, 6, 2), lwd=c(3, 2.5, 2, 1.5),  ylab="Length (cm)", ylim=c(0, 400), xlim=c(0.5, 16*12), xlab= "Age (years)", xaxt="n")
     axis(1, at = seq(0, 220, by=12), labels = (seq(1, 19, by=1)))

legend("bottomright", bty="n", legend=c("Temp290K, Constant Env", "Temp 290K, Seasonal Env", "Temp 295K, Constant Env", "Temp 295K, Seasonal Env"), col=c(1, 4, 6, 2), lwd=c(3, 2.5, 2, 1.5) )




 setwd("~/Documents/tuna_theory/Length_data/HighmortLowfood/")
 
 HimuLofood <- list.files(pattern =  "\\.csv")

 HLdata <- lapply(HimuLofood, read.csv)
 
datamat=matrix(nrow=216, ncol=4)
bodysize=rep(NA, 4)
for (i in 1:4) {
	
    	datamat[, i]<-as.numeric(HLdata[[i]][1, -1])
	  bodysize[i]<-max(as.numeric(HLdata[[i]][1, -1]))
}


matplot(datamat, type="l", lty=1, col=c(1, 4, 6, 2), lwd=c(3, 2.5, 2, 1.5),  ylab="Length (cm)", ylim=c(0, 400), xlim=c(0.5, 16*12), xlab= "Age (years)", xaxt="n")
     axis(1, at = seq(0, 220, by=12), labels = (seq(1, 19, by=1)))

HimuLofood
bodysize


 setwd("~/Documents/tuna_theory/Length_data/LowmortHighfood/")
 
 LomuHifood <- list.files(pattern =  "\\.csv")

 LHdata <- lapply(LomuHifood, read.csv)
 
datamat=matrix(nrow=216, ncol=4)
bodysize=rep(NA, 4)
for (i in 1:4) {
	
    	datamat[, i]<-as.numeric(LHdata[[i]][1, -1])
	  bodysize[i]<-max(as.numeric(LHdata[[i]][1, -1]))
}


matplot(datamat, type="l", lty=1, col=c(1, 4, 6, 2), lwd=c(3, 2.5, 2, 1.5),  ylab="Length (cm)", ylim=c(0, 400), xlim=c(0.5, 16*12), xlab= "Age (years)", xaxt="n")
     axis(1, at = seq(0, 220, by=12), labels = (seq(1, 19, by=1)))


LomuHifood
bodysize




 setwd("~/Documents/tuna_theory/Length_data/LowmortLowfood/")
 
 LomuLofood <- list.files(pattern =  "\\.csv")

 LLdata <- lapply(LomuLofood, read.csv)
 
datamat=matrix(nrow=216, ncol=4)
bodysize=rep(NA, 4)
for (i in 1:4) {
	
    	datamat[, i]<-as.numeric(LLdata[[i]][1, -1])
	  bodysize[i]<-max(as.numeric(LLdata[[i]][1, -1]))
}


matplot(datamat, type="l", lty=1, col=c(1, 4, 6, 2), lwd=c(3, 2.5, 2, 1.5),  ylab="Length (cm)", ylim=c(0, 400), xlim=c(0.5, 16*12), xlab= "Age (years)", xaxt="n")
     axis(1, at = seq(0, 220, by=12), labels = (seq(1, 19, by=1)))


LomuLofood
bodysize











