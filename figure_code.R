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
   labs(y="Age at 50% Maturity (years)")+
 
  
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
   labs(y="Maximum Length (cm)") +

  
facet_wrap(~Seasonality) 
d
 
 
 
 quartz()
E <- ggplot(rate_dat,
    aes(x=Predation, y=Sizemat) 
    ) + 
	geom_point(aes(colour = Temp, shape=Food), size = 3 ) +
 
        scale_size_manual(values=c(4, 1)) +

   scale_shape_manual(values=c(15, 20)) +
   scale_color_manual(values = c("blue", "red")) +
   labs(y="Size at 50% Maturity (cm)")+
 
  
facet_wrap(~Seasonality) 
E

 
 FIGURE 2 CODE
###################################################################################### 
par(mfrow=c(2,4))
 
 
 # LomuLofood <- list.files(pattern =  "\\.csv")

 # LLdata <- lapply(LomuLofood, read.csv)
 
# datamat=matrix(nrow=216, ncol=4)
# bodysize=rep(NA, 4)
# for (i in 1:4) {
	
    	# datamat[, i]<-as.numeric(LLdata[[i]][1, -1])
	  # bodysize[i]<-max(as.numeric(LLdata[[i]][1, -1]))
# }

cool_cons_lomort_vlowfood<-read.csv("~/Desktop/holly_results/Temp290/constant/size_costs_threshold7/01Lengthf_h0.33Kappa0.08reprolimit0.2Tmax216.csv")

cool_cons_lomort_lowfood<-read.csv("~/Desktop/holly_results/Temp290/constant/size_costs_threshold7/01Lengthf_h0.33Kappa0.21reprolimit0.2Tmax216.csv")
 
 cool_cons_lomort_medfood<-read.csv("~/Desktop/holly_results/Temp290/constant/size_costs_threshold7/01Lengthf_h0.33Kappa0.42reprolimit0.2Tmax216.csv")

cool_cons_lomort_hifood<-read.csv("~/Desktop/holly_results/Temp290/constant/size_costs_threshold7/01Lengthf_h0.33Kappa0.83reprolimit0.2Tmax216.csv")

cool_cons_himort_vlowfood<-read.csv("~/Desktop/holly_results/Temp290/constant/size_costs_threshold7/01Lengthf_h0.67Kappa0.08reprolimit0.2Tmax216.csv")
 
cool_cons_himort_lowfood<-read.csv("~/Desktop/holly_results/Temp290/constant/size_costs_threshold7/01Lengthf_h0.67Kappa0.21reprolimit0.2Tmax216.csv")
 
 cool_cons_himort_medfood<-read.csv("~/Desktop/holly_results/Temp290/constant/size_costs_threshold7/01Lengthf_h0.67Kappa0.42reprolimit0.2Tmax216.csv")

cool_cons_himort_hifood<-read.csv("~/Desktop/holly_results/Temp290/constant/size_costs_threshold7/01Lengthf_h0.67Kappa0.83reprolimit0.2Tmax216.csv")

warm_cons_lomort_vlowfood<-read.csv("~/Desktop/holly_results/Temp295/constant/size_costs_threshold7/01Lengthf_h0.33Kappa0.08reprolimit0.2Tmax216.csv")
warm_cons_lomort_lowfood<-read.csv("~/Desktop/holly_results/Temp295/constant/size_costs_threshold7/01Lengthf_h0.33Kappa0.21reprolimit0.2Tmax216.csv")
 
 warm_cons_lomort_medfood<-read.csv("~/Desktop/holly_results/Temp295/constant/size_costs_threshold7/01Lengthf_h0.33Kappa0.42reprolimit0.2Tmax216.csv")

warm_cons_lomort_hifood<-read.csv("~/Desktop/holly_results/Temp295/constant/size_costs_threshold7/01Lengthf_h0.33Kappa0.83reprolimit0.2Tmax216.csv")

warm_cons_himort_vlowfood<-read.csv("~/Desktop/holly_results/Temp295/constant/size_costs_threshold7/01Lengthf_h0.67Kappa0.08reprolimit0.2Tmax216.csv")
warm_cons_himort_lowfood<-read.csv("~/Desktop/holly_results/Temp295/constant/size_costs_threshold7/01Lengthf_h0.67Kappa0.21reprolimit0.2Tmax216.csv")
 
 warm_cons_himort_medfood<-read.csv("~/Desktop/holly_results/Temp295/constant/size_costs_threshold7/01Lengthf_h0.67Kappa0.42reprolimit0.2Tmax216.csv")

warm_cons_himort_hifood<-read.csv("~/Desktop/holly_results/Temp295/constant/size_costs_threshold7/01Lengthf_h0.67Kappa0.83reprolimit0.2Tmax216.csv")


cool_seas_lomort_vlowfood<-read.csv("~/Desktop/holly_results/Temp290/seasonal/size_costs_threshold7/01Lengthf_h0.33Kappa0.08reprolimit0.2Tmax216.csv")
 
cool_seas_lomort_lowfood<-read.csv("~/Desktop/holly_results/Temp290/seasonal/size_costs_threshold7/01Lengthf_h0.33Kappa0.21reprolimit0.2Tmax216.csv")
 
 cool_seas_lomort_medfood<-read.csv("~/Desktop/holly_results/Temp290/seasonal/size_costs_threshold7/01Lengthf_h0.33Kappa0.42reprolimit0.2Tmax216.csv")
 
 cool_seas_lomort_hifood<-read.csv("~/Desktop/holly_results/Temp290/seasonal/size_costs_threshold7/01Lengthf_h0.33Kappa0.83reprolimit0.2Tmax216.csv")
 
 
 cool_seas_himort_vlowfood<-read.csv("~/Desktop/holly_results/Temp290/seasonal/size_costs_threshold7/01Lengthf_h0.67Kappa0.08reprolimit0.2Tmax216.csv")
 
 cool_seas_himort_lowfood<-read.csv("~/Desktop/holly_results/Temp290/seasonal/size_costs_threshold7/01Lengthf_h0.67Kappa0.21reprolimit0.2Tmax216.csv")
 
 cool_seas_himort_medfood<-read.csv("~/Desktop/holly_results/Temp290/seasonal/size_costs_threshold7/01Lengthf_h0.67Kappa0.42reprolimit0.2Tmax216.csv")
 
 cool_seas_himort_hifood<-read.csv("~/Desktop/holly_results/Temp290/seasonal/size_costs_threshold7/01Lengthf_h0.67Kappa0.83reprolimit0.2Tmax216.csv")
 

warm_seas_lomort_vlowfood<-read.csv("~/Desktop/holly_results/Temp295/seasonal/size_costs_threshold7/01Lengthf_h0.33Kappa0.08reprolimit0.2Tmax216.csv")
warm_seas_lomort_lowfood<-read.csv("~/Desktop/holly_results/Temp295/seasonal/size_costs_threshold7/01Lengthf_h0.33Kappa0.21reprolimit0.2Tmax216.csv")
 
 warm_seas_lomort_medfood<-read.csv("~/Desktop/holly_results/Temp295/seasonal/size_costs_threshold7/01Lengthf_h0.33Kappa0.42reprolimit0.2Tmax216.csv")
 
 

  warm_seas_himort_vlowfood<-read.csv("~/Desktop/holly_results/Temp295/seasonal/size_costs_threshold7/01Lengthf_h0.67Kappa0.08reprolimit0.2Tmax216.csv")
  warm_seas_himort_lowfood<-read.csv("~/Desktop/holly_results/Temp295/seasonal/size_costs_threshold7/01Lengthf_h0.67Kappa0.21reprolimit0.2Tmax216.csv")
 
 warm_seas_himort_medfood<-read.csv("~/Desktop/holly_results/Temp295/seasonal/size_costs_threshold7/01Lengthf_h0.67Kappa0.42reprolimit0.2Tmax216.csv")

datamat0<-rbind(cool_cons_lomort_vlowfood[1,], cool_seas_lomort_vlowfood[1,], warm_cons_lomort_vlowfood[1,], warm_seas_lomort_vlowfood[1,])
matplot(t(datamat0), type="l", lty=1, col=c(1, 4, 6, 2), lwd=c(3, 2.5, 2, 1.5),  ylab="Length (cm)", ylim=c(0, 400), xlim=c(0.5, 16*12), xlab= "Age (years)", xaxt="n")
     axis(1, at = seq(0, 220, by=12), labels = (seq(1, 19, by=1)))
    

 datamat1<-rbind(cool_cons_lomort_lowfood[1,], cool_seas_lomort_lowfood[1,], warm_cons_lomort_lowfood[1,], warm_seas_lomort_lowfood[1,])

matplot(t(datamat1), type="l", lty=1, col=c(1, 4, 6, 2), lwd=c(3, 2.5, 2, 1.5),  ylab="Length (cm)", ylim=c(0, 400), xlim=c(0.5, 16*12), xlab= "Age (years)", xaxt="n")
     axis(1, at = seq(0, 220, by=12), labels = (seq(1, 19, by=1)))
     

  datamat2<-rbind(cool_cons_lomort_medfood[1,], cool_seas_lomort_medfood[1,], warm_cons_lomort_medfood[1,], warm_seas_lomort_medfood[1,])

matplot(t(datamat2), type="l", lty=1, col=c(1, 4, 6, 2), lwd=c(3, 2.5, 2, 1.5),  ylab="Length (cm)", ylim=c(0, 400), xlim=c(0.5, 16*12), xlab= "Age (years)", xaxt="n")
     axis(1, at = seq(0, 220, by=12), labels = (seq(1, 19, by=1)))     
     
 datamat3<-rbind(cool_cons_lomort_hifood[1,],  warm_cons_lomort_hifood[1,])

matplot(t(datamat3), type="l", lty=1, col=c(1, 4, 6, 2), lwd=c(3, 2.5, 2, 1.5),  ylab="Length (cm)", ylim=c(0, 400), xlim=c(0.5, 16*12), xlab= "Age (years)", xaxt="n")
     axis(1, at = seq(0, 220, by=12), labels = (seq(1, 19, by=1)))    
     
 datamat4.0<-rbind(cool_cons_himort_vlowfood[1,], cool_seas_himort_vlowfood[1,], warm_cons_himort_vlowfood[1,], warm_seas_himort_vlowfood[1,])
matplot(t(datamat4.0), type="l", lty=1, col=c(1, 4, 6, 2), lwd=c(3, 2.5, 2, 1.5),  ylab="Length (cm)", ylim=c(0, 400), xlim=c(0.5, 16*12), xlab= "Age (years)", xaxt="n")
     axis(1, at = seq(0, 220, by=12), labels = (seq(1, 19, by=1)))


  datamat4<-rbind(cool_cons_himort_lowfood[1,], cool_seas_himort_lowfood[1,], warm_cons_himort_lowfood[1,], warm_seas_himort_lowfood[1,])
  
matplot(t(datamat4), type="l", lty=1, col=c(1, 4, 6, 2), lwd=c(3, 2.5, 2, 1.5),  ylab="Length (cm)", ylim=c(0, 400), xlim=c(0.5, 16*12), xlab= "Age (years)", xaxt="n")
     axis(1, at = seq(0, 220, by=12), labels = (seq(1, 19, by=1)))



  datamat5<-rbind(cool_cons_himort_medfood[1,], cool_seas_himort_medfood[1,], warm_cons_himort_medfood[1,], warm_seas_himort_medfood[1,])
  
matplot(t(datamat5), type="l", lty=1, col=c(1, 4, 6, 2), lwd=c(3, 2.5, 2, 1.5),  ylab="Length (cm)", ylim=c(0, 400), xlim=c(0.5, 16*12), xlab= "Age (years)", xaxt="n")
     axis(1, at = seq(0, 220, by=12), labels = (seq(1, 19, by=1)))


  datamat6<-rbind(cool_cons_himort_hifood[1,],  warm_cons_himort_hifood[1,])

matplot(t(datamat6), type="l", lty=1, col=c(1, 4, 6, 2), lwd=c(3, 2.5, 2, 1.5),  ylab="Length (cm)", ylim=c(0, 400), xlim=c(0.5, 16*12), xlab= "Age (years)", xaxt="n")
     axis(1, at = seq(0, 220, by=12), labels = (seq(1, 19, by=1)))

legend("bottomright", bty="n", legend=c("Temp290K, Constant Env", "Temp 290K, Seasonal Env", "Temp 295K, Constant Env", "Temp 295K, Seasonal Env"), col=c(1, 4, 6, 2), lwd=c(3, 2.5, 2, 1.5) )














