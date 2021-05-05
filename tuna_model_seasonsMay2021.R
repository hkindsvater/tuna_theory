 
#!/usr/bin/env Rscript --vanilla
#install.packages("fields")
# library(fields)
 

f_h=4/12 #predation (low = 4)
filepath <- "~/Desktop/holly_results/2021/may/constant/"
seasons = "NO"

timebin=12
# args <-  commandArgs(trailingOnly = TRUE)
# counter <- as.numeric(args[1])
# reprolimit <- as.numeric(args[2])
# Kappa <- as.numeric(args[3])/12
# Temp <- as.numeric(args[4]) 
# Tmax <- as.numeric(args[5])*timebin
 
Temp <- 290
 
  reprolimit=0.2
  Kappa = 5/12  
   
  Tmax = 18*timebin  #monthly stime steps, maximum lifespan is 18 years
 
#describe temperature dependent costs
k=1.3e-23
E = 1.04e-19
theta=0.66
 coef1  =5e+16 ##normalization constant puts tuna SMR in the same ballpark as the costs Kitchell et al. (1978) Bioenergetic spectra of skipjack and yellowfin tunas, pp 359 IN Sharp G.D. and Dizon A.E. eds. The Physiological Ecology of Tunas, Academic press.  

#physiological parameters
a <- 1e-5 #from ICCAT 2015 BFT length-weight relationship
scale <-  4.2e+6 #J/kg #from Chapman et al. 2011
b=1.8
d = 2.4

#STATE VARIABLES
phi <- 1 # only 1 environment
Lmax <- 400  #maximum size of 4 meters
Lmin <- 1 
Yindexmax <- 400 #maximum index of stores in state loop  

storemax= 0.6 #proportion of structural mass that individuals can devote to energy storage
storemin = 0.1
 

###################################################################################################################################################################################################
###Lookup Tables - look up costs and food functions so they are not calculated every time

###Sizespectra allow us to descripbe prey preference, encounter, consumption to predict prey availability and mass-specific mortality:
  #total mass maximum in kg
##Prey availability   
phi_a <- 3 #from table 2.2 in Andersen book
lam <- 1.95
K_c=10 #from Table 2.2, this changes with season (and is scaled by Kappa)
##mass dependent mortality
phi_p <- 0.07 #from table 2.2 in Andersen book
f_0 <- 0.6 #somewhere between 0 and 1, but predators rarely caught with totally full stomach
hprime <- 17.2
 
#coefficient on the consumption rate from table 2.2
met_mort <- -0.25 #the argument in Andersen book is that mass-specific rates such as mortality scales with the metabolic esp of 3/4 (Brown et al. 2004). 

 
####ADD SEASONALITY IN RESOURCES AND FOOD TO SOME MONTHS


if(seasons == "NO") {	
	kmult <-   rep(1, timebin)   
	raiseT <-      rep(0, timebin)  	
		} else {
		#hardcoded for timebin = 12
	kmult <-    c(rep(1, 6), rep(2, 6)) 
	raiseT <-   c(rep(4,6), rep(0, 6))#

		} #end if 

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
#Income = Kappa*phi_a*K_c*Mass^(2-lam) #this describes the scaling with size and ecostystem richness
  
#plot(Income)
 
# ###plot metabolic cost functions for each temp to check they are sensible
 # matplot( ((1:length(Mass))), t((MTcosts)), type = "l", lty=1, lwd=2,   xlab=" (Mass (kg))", ylab="Metabolic rate in J/season", col=c(4, 3, "orange", 2, "dark red"))    
#################################################################################################################################################################################################


#DYNAMIC MODEL: life history in a single environment

#dynamic behaviors
u <-  seq(0, 1, by = 0.1) #fraction stored (rest allocated to structure)

r<-  seq(0, 1, by = 0.1) #fraction allocated to reproduction (rest is saved)


#set up arrays to store fitness values  
MaxF=array(dim=c(Yindexmax, Lmax, phi, Tmax),data=-1) #this will store the max fitness for every combination of state and times
MaxF[,,,Tmax]=0 #Fill fitness of Tmax column with 0
MaxF_wE <- MaxF
prob_denom <- MaxF

Vmat=array(dim=c(Yindexmax, Lmax, phi, Tmax-1, length(u), length(r) ))
#Vmat is the fitness value of all possible actions
Vmat[,,,,,]=rep(-1, length(Vmat))
 
#store optimal behaviors
optU=array(dim=c(Yindexmax, Lmax, phi, Tmax-1))
optR=array(dim=c(Yindexmax, Lmax, phi, Tmax-1))


#the dynamic loop:
Y <- 1
 for (Y in 1:Yindexmax) { #for all values of Energy Stores in loop (unscaled)
  
  #setTxtProgressBar(pb, Y) #progress bar tracks which value of Y we are on
  L <- Lmin
  for (L in Lmin:Lmax) { #for all possible values of Length
    p <- 1
    for (p in 1:phi) { #for every temp environment
      i <- Tmax-1	
      for (i in (Tmax-1):1) { #where i is time (age) in months
    		month = i %% 12 #for the seasonality convert time in months to specific season-month (1-12)

        ############################
                            #calculate the critical stored energy needed for this length to be viable 
            Wstructure<-a*L^3 #  find structural mass in kg
            EcritL <- Wstructure*storemin*scale
            EstoresmaxL <-Wstructure*storemax*scale #following Chapman et al
            Estores<- Y*scale
            if (Y*scale > EstoresmaxL)  Estores <- EstoresmaxL #stored energy (Estores) capped at the maximum allowed (60% of the structural body mass in J) 
	       
	        #two loops over allocation strategies:
	        g <- 1
	        for (g in 1:length(u)) { #fractional placeholder (placeholder variable so we can loop over non-integers)
	            growth = u[g] #Convert integer loop index to fractional value for allocation     
	          
	        h <- 1
	        for(h in 1:length(r)) { #where r is fractional allocation to reproductive effort	
	            
	           reprod = r[h]  #Convert integer loop index to fractional value for allocation
	           
	           
	           #check if this combination of allocation is viable, and this number of stores is adequate for this length: 
		          if (growth + reprod > 1  | EcritL >= Estores)   Vmat[Y,L, p, i, g, h]	<- 0 else 
		            { #given the above conditions are met calculate all states:
		               
		                Wstores<-Estores/scale  #stored mass in kG
		                Wtotal <- Wstores+Wstructure  #body mass in KG
		                Estructure <- Wstructure*scale 
		                Rlimit <- Estructure*reprolimit
		              
		                #future state calculations		              
		                EstoresP <- Estores*(1-reprod-growth) +Income[month, L]*scale - MTcosts[month, L] #combines mass-dependent food intake and mass-dependent metabolic costs
		                EstructureP <- Estructure + growth*Estores
		                WstructureP <- Wstructure + growth*Estores/scale 
		                LengthP <- (L^3 + growth*(Estores /(a*scale)) )^(1/3)
		                EstoresmaxLP <- EstructureP*storemax 
		                EstoresP <- min(EstoresP, EstoresmaxLP) # this statement caps stores max storage allowed for that size
		                EstoresP <- max(EstoresP, 0)	 #if future stored energy is negative, it is cut off at zero.   
		              
		              
		              ####Interpolation of future fitness between non-integer values of stored energy
		              
		              dx <- EstoresP/scale - floor(EstoresP/scale)
		              Yindex <- floor(EstoresP/scale)
		                
		              if (ceiling(LengthP) > Lmax) Lindex <- Lmax else
		                Lindex <- ceiling(LengthP)
		             
		              if(Yindex >= Yindexmax ) FutureFitness <- MaxF[Yindexmax,Lindex,p,i+1] else 
		                {
		              	
		              	FutureFitness <- dx*MaxF[Yindex+1, Lindex, p, i+1]
		                  
		              	if(Yindex > 0) FutureFitness <- FutureFitness + (1-dx)*MaxF[Yindex,Lindex,p,i+1] 
		                   	} #end else if
		              	
		                Vmat[Y,L, p, i, g, h]	<-  min(reprod*Estores, Rlimit)  + exp(-mu[L])*FutureFitness 
		              
		                 } #end else if growth + reprod < 1 and EcritL < Estores	   
            
	            
	         		 } #end h loop
	      			 } #end g loop
	        
        #find and store the highest fitness from growth and reproduction combinations given the other values for length, stored energy, and age
        MaxF[Y,L,p,i] <-  max(Vmat[Y,L, p, i, , ])
        
        #find out the differences in fitness of all strategies from the maximum, use the mean if a tie, record the difference for later error introduction:
        
        mult_u <- rep(NA, length(u))
        mult_r <- rep(NA, length(r))
        
        for (g in length(u):1) {
          for(h in length(r):1) {
             
            if (MaxF[Y, L,p,i] - Vmat[Y,L, p, i, g, h]   == 0) {
              
              mult_u[g] <- u[g]	   #record all behaviors that have the same fitness as the max.         
              mult_r[h] <- r[h]	
              
            }  #end if
            
          } # end 2nd h loop  
        } #end 2nd g loop
        
        #take mean of all behaviors with same fitness as max
        
        optU[Y, L,p,i]=min(mult_u, na.rm=TRUE) 
        optR[Y, L,p,i]=min(mult_r, na.rm=TRUE)  
        
        
        
        
      } #end i loop
    }	#end p loop
  } #end L loop
  
} #end Y loop
 
# close(pb) #close progress bar
# require(fields)
# pal=terrain.colors(n=100)# ##set the palette
# quartz()
# par(mfrow=c(2,2)) 	
# image(optU[1:36, 45:100, 1,1], col=pal,   ylab="Size", xlab="Energy Stores", main="Growth, Age is 1" )
# image(optU[  , 100  , 1,   ], col=pal, ylab="Age", xlab="growth", main="Growth, Length is 100" ) 

# image(optU[ , 200 , 1,  ] , col=pal, ylab="Age", xlab="Energy Stores", main="Growth, Length is 200" ) 


 # quartz()
 # par(mfrow=c(2,2)) 	
 # 
 # image( optR[1:36, 45:100 ,1,1], col=pal, ylab="Size", xlab="Energy Stores", main="Repro, Age is 1 " )	   	   
 # 
 # image(optR[ , 100  , 1,  ],col=pal, ylab="Age", xlab="Energy Stores", main="R, Length is 100" ) 
 # 
 # image(optR[, 200 , 1, ], col=pal, ylab="Age", xlab="Energy Stores", main="R, Length is 200" ) 
 # 

# # 	  # # image(optR[,  300  , 1 , ], col=pal  ) 
set.seed(2001)
 
nindiv=2  
 
Ngroups=1
group=1

initialsize <- rep(50, 2) #as.integer(rnorm(nindiv, mean=50, sd=2.5))
alive=matrix(ncol = Tmax, nrow= Ngroups, data=0)

idist=matrix(data=NA, nrow=nindiv, ncol=Tmax) #keeps track of energetic state over time
sizedist=matrix(data=NA, nrow=nindiv, ncol=Tmax)
g_allo= array(dim=c(nindiv, Tmax), data = 0 )
repro= array(dim=c(nindiv, Tmax), data = 0 ) 
income=array(dim=c(nindiv, Tmax), data = 0 )
#these will give storage fraction and reproduction for each individual, given its two states at each time

#z=rnorm(nindiv, mean=scale*a*initialsize^3*(storemax - 0.05), sd=0) ## Generate a population (z) of indivdiuals, condition based on weight  (95% of max for size, with some variation)
z<- scale*a*initialsize^3*(storemax - 0.05) #fixed initial state

idist[,1]=ceiling(z) #this rounds every z up to the nearest integer for the first time step  
 sizedist[,1]<- initialsize   
#stores number of survivors at each time

reproduction=matrix(0, nindiv, Tmax) #stores how much they reproduce at each time. 

#draw random numbers for every individual's survival chance at every time (compare randroaw to exp(-mu))  
randraw=matrix(runif(nindiv*(Tmax), max=1, min=0), nrow=nindiv, ncol=Tmax)
randraw2=matrix(runif(nindiv*(Tmax), max=1, min=0), nrow=nindiv, ncol=Tmax)
normdraw=matrix(rnorm(nindiv*(Tmax), mean=1, sd=0.005), nrow=nindiv, ncol=Tmax) #add stochasticity in food intake

survival=rep(0, Tmax)
survival[1]<-1

	for (i in 1:(Tmax-1)) { 
	   month = i %% 12 
	   state  <- idist[,i] 
	   size <- round(sizedist[,i])  
	   EcritL <-  scale*a*size^3*storemin   
	   index <- which(state > EcritL) #which individuals are still alive (didn't starve)
	       
	       if(length(index) > 1) {
	    
		    ##find interpolated behaviors
		    Ilo <- floor(state[index]/scale)
		    
		    #these ifelse statments deal with the upper boundary on Estores (energetic state):
		    #find the allocation strategies of individuals of these states and age, regardless of whether they survive  (since they allocate first) 
		    Ilo <- ifelse(Ilo >= Yindexmax, Yindexmax-1, Ilo)   
		    dx <- ifelse(Ilo >= Yindexmax, 1, state[index]/scale - Ilo) 
		    
		    ##NOW WE NEED TO LOOK UP THE OPTIMAL BEHAVIORS FROM THE BACKWARD SOLUTION
		    #CAREFULLY DEALING WITH THE CASE OF LOW STATE 
		    condind <- Ilo == 0
		    
		    ##   if state is between 0 and 1 (Ilo = 0)the strategy is only dx*(state=1)
		    g_allo[index[condind==TRUE], i] <- round((dx[condind==TRUE])*diag(optU[Ilo[condind==TRUE]+1, size[index[condind==TRUE]], p, i]),1)  
		    repro[index[condind==TRUE], i] <- round((dx[condind==TRUE])*diag(optR[Ilo[condind==TRUE]+1, size[index[condind==TRUE]], p, i]),1) 
		    
		    g_allo[index[condind==FALSE], i] <- round(1-dx[condind==FALSE]*diag(optU[Ilo[condind==FALSE], size[index[condind==FALSE]], p, i]) + (dx[condind==FALSE])*diag(optU[Ilo[condind==FALSE]+1, size[index[condind==FALSE]], p, i]),1)  
		    repro[index[condind==FALSE], i] <-round(1-dx[condind==FALSE]*diag(optR[Ilo[condind==FALSE], size[index[condind==FALSE]], p, i]) +  (dx[condind==FALSE])*diag(optR[Ilo[condind==FALSE]+1, size[index[condind==FALSE]], p, i]),1	)	  					  
	    
		    #now calculate Wtotal, Costs, and Net energy intake
		    
		    Wstructure<- a*size[index]^3 #structural mass in kilograms 
		    Estructure <- Wstructure*scale
		    Replim <- Estructure*reprolimit       
		    EstoresmaxL <-Wstructure*storemax*scale #modified from Chapman et al.  
		    #Energy stores are capped to be a fraction if TOTAL body mass
		     
		    state[index] <- ifelse(state[index] > EstoresmaxL, EstoresmaxL, state[index]) #stored energy capped at a certain body size
		    
		    Wstores<-state[index]/scale
		    
		    Wtotal <-  Wstores+Wstructure   #body mass
		    
		    
		    # REPRODUCE AND GROW BEFORE SURVIAL IS DETERMINED    
		    
		    reproduction[index, i]<- ifelse(repro[index, i]*state[index] < Replim, repro[index, i]*state[index], Replim)    
		    
		    nextsize <-   (size[index]^3 +  g_allo[index,i]*(state[index] /(a*scale)) )^(1/3)
		    
		    #survival <- randraw[index,i] <= exp(-mu[size[index]]) 
		    
		    #########################
		    survival[i+1] <- survival[i]*exp(-mu[size[index[1]]]) #this stores the cumulative probability an individual survives to age i+1
		    #right now all individuals survive, we are not doing the randraw comparison
		    
		    #future state calculation:
		    idist[index,i+1] <- (1-repro[index, i]-g_allo[index,i])*state[index] + Income[month, size[index]]*scale - MTcosts[month, size[index]] 
		    
		    sizedist[index, i+1] <- nextsize
		    
		    #alive[group, i+1]=sum(idist[,i+1] > 0, na.rm=TRUE) #number of survivors
		     
	 	    } #end if
		}     #end time (i) loop

	x1=3*timebin+1
	y1= (colSums(alive))[x1]
	
	x2=4*timebin 
	
	y2=(colSums(alive))[x2]

reproduction[, -Tmax]<-ifelse(reproduction[, -Tmax]>0,  reproduction[, -Tmax], NA)

idist[, -Tmax]<-ifelse(idist[, -Tmax]>0,  idist[, -Tmax], NA)
write.csv(idist, file=paste0(filepath,"03State", "Temp", Temp, "f_h", round(f_h, 2),  "Kappa", round(Kappa,2), "reprolimit", reprolimit, "Tmax", Tmax, ".csv"))
write.csv(sizedist, file=paste0(filepath,"01Length","Temp", Temp, "f_h", round(f_h, 2),   "Kappa", round(Kappa,2),  "reprolimit", reprolimit, "Tmax", Tmax, ".csv"))
write.csv(reproduction, file=paste0(filepath,"02Repro", "Temp", Temp, "f_h", round(f_h, 2),   "Kappa", round(Kappa,2),  "reprolimit", reprolimit, "Tmax", Tmax, ".csv")) 
write.csv(survival, file=paste0(filepath,"04Surv", "Temp", Temp, "f_h", round(f_h, 2),   "Kappa", round(Kappa,2),   "reprolimit", reprolimit, "Tmax", Tmax, ".csv")) 
 
 
earlyfit <- MaxF[earlystate, earlysize, 1,  ]
optfit <-   MaxF[optstate, optsize, 1,  ]


write.csv(earlyfit, file=paste0(filepath,"age3fit", "Temp", Temp,"f_h", round(f_h, 2),  "Kappa", round(Kappa,2), "reprolimit", reprolimit, "Tmax", Tmax, ".csv"))


write.csv(optfit, file=paste0(filepath,"age15fit", "Temp", Temp, "f_h", round(f_h, 2),  "Kappa", round(Kappa,2), "reprolimit", reprolimit, "Tmax", Tmax, ".csv"))

 