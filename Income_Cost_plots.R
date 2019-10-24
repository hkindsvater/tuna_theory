counter = 1
reprolimit=0.2
Kappa=5
f_h=20

timebin=12
c1=1
 Temp <- 295

Tmax = 18*timebin  #monthly stime steps, maximum lifespan is 18 years

#describe temperature dependent costs
k=1.3e-23
E = 1.04e-19
theta=0.66
coef1  = 5e+16 ##normalization constant puts tuna SMR in the same ballpark as the costs Kitchell et al. (1978) Bioenergetic spectra of skipjack and yellowfin tunas, pp 359 IN Sharp G.D. and Dizon A.E. eds. The Physiological Ecology of Tunas, Academic press.  

#physiological parameters
a <- 1e-5 #from ICCAT 2015 BFT length-weight relationship
scale <-  4.2e+6 #J/kg #from Chapman et al. 2011
b=1.8
d = 2.4

#STATE VARIABLES
phi=1 # only 1 environment
Lmax=375  #maximum size of 4 meters
Lmin = 1 
Estoresmax=350 #maximum stores in loop  

storelimit= 0.75 #proportion of structural mass that inidivduals can devote to energy storage
storemin = 0.1
 

###################################################################################################################################################################################################
###Lookup Tables - look up costs and food functions so they are not calculated every time

###Sizespectra allow us to descripbe prey preference, encounter, consumption to predict prey availability and mass-specific mortality:
  #total mass maximum in kg
##Prey availability   
phi_a <- 3 #from table 2.2 in Andersen book
K_c <- 10 #from table 2.2, this is averaged over "all" - so PP in stomach of all preds and preys have a MR of 1224 independently of body size - but htis is something that changes with ecosystem according to KAPPA, eg less in deep sea, more in upwelling
lam <- 1.95

##mass dependent mortality
phi_p <- 0.07 #from table 2.2 in Andersen book
f_0 <- 0.6 #somewhere between 0 and 1, but predators rarely caught with totally full stomach
hprime <- 17.2
 
#coefficient on the consumption rate from table 2.2
met_mort <- -0.25 #the argument in Andersen book is that mass-specific rates such as mortality scales with the metabolic esp of 3/4 (Brown et al. 2004). 

 
####ADD SEASONALITY IN RESOURCES AND FOOD TO SOME MONTHS
kmult <- rep(1, timebin) #c(rep(1, 6), rep(2, 6)) # 
raiseT <- rep(0, timebin)#c(rep(4,6), rep(0, 6))  # 
Mass <- a*(Lmin:Lmax)^3 

mu<- phi_p*f_h*Mass^met_mort #note we are excluding "background" mortality that is independent of size.... 

Income = matrix(nrow = timebin, ncol = length(Mass))
MTcosts = matrix(nrow = timebin, ncol = length(Mass))
for (kap in 1:timebin) {
  
  Income[kap, ] <- kmult[kap]*Kappa*phi_a*K_c*Mass^(2-lam) #this describes the scaling with size and ecostystem richness
  MTcosts[kap, ] <-coef1*(Mass)^theta*(exp(-E/(k*(Temp+raiseT[kap])))) 
  
}
 
# plot(MTcosts[7, ], type="l", ylab="Monthly Metabolic Costs (J)", xaxt="n", lwd=5, ylim=c(0, 2600000), col=4)
# plot(MTcosts[7, ], type="l", ylab="Monthly Metabolic Costs (J)", xaxt="n", lwd=5, ylim=c(0, 7800000), col=4)
# lines(MTcosts[1, ], lty=1, col=1, lwd=2)
# plot(MTcosts[7, ], type="l", ylab="Monthly Metabolic Costs (J)", xaxt="n", lwd=5, ylim=c(0, 7800000), col=1)
# lines(MTcosts[1, ], lty=1, col=2, lwd=2)
 