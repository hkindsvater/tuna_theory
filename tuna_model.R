 
#!/usr/bin/env Rscript --vanilla
#install.packages("fields")
# library(fields)
setwd("/Users/hollykindsvater/Documents/tuna_theory/")

set.seed(1001)
timebin <- 12
args <-  commandArgs(trailingOnly = TRUE)
counter <- as.numeric(args[1]) 
c1 = as.numeric(args[2])
Kappa = as.numeric(args[3])/3
f_h = as.numeric(args[4])/3

 Temp <- 293

Tmax = 16*timebin  #seasonal time steps, maximum lifespan is 16 years

#describe temperature dependent costs
k=1.3e-23
E = 1.04e-19
theta=0.66
coef1  = 5e+17 ##normalization constant puts tuna SMR in the same ballpark as the costs Kitchell et al. (1978) Bioenergetic spectra of skipjack and yellowfin tunas, pp 359 IN Sharp G.D. and Dizon A.E. eds. The Physiological Ecology of Tunas, Academic press.  

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

storelimit= 1 #proportion of structural mass that inidivduals can devote to energy storage
storemin = 0.1
reprolimit = .5

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
#f_h<-f_0*hprime
#coefficient on the consumption rate from table 2.2
met_mort <- -0.25 #the argument in Andersen book is that mass-specific rates such as mortality scales with the metabolic esp of 3/4 (Brown et al. 2004). 

 
#Kappa=1
kmult <- c(1, 1,1, 1)
raiseT <- c(0, 0, 0, 0)
Mass <- a*(Lmin:Lmax)^3

mu<- phi_p*f_h*Mass^met_mort #note we are excluding "background" mortality that is independent of size.... 

Income = matrix(nrow = 4, ncol = length(Mass))
MTcosts = matrix(nrow = 4, ncol = length(Mass))
for (kap in 1:4) {
  
  Income[kap, ] <- kmult[kap]*Kappa*phi_a*K_c*Mass^(2-lam) #this describes the scaling with size and ecostystem richness
  MTcosts[kap, ] <-coef1*(Mass)^theta*(exp(-E/(k*(Temp+raiseT[kap]))))/3 
  
}

#Income = Kappa*phi_a*K_c*Mass^(2-lam) #this describes the scaling with size and ecostystem richness
# plot(Income)
SDfood=0
minI = Income - SDfood * 2
maxI = Income + SDfood * 2
bins = 20
foodmatrix=matrix(ncol=bins, nrow=length(Mass))
weightmatrix=matrix(ncol=bins, nrow=length(Mass))
for(p in 1:length(Mass)) { 
  
  binEdges = seq(minI[p], maxI[p], length.out=bins+1)
  binMids = (binEdges[-1] + binEdges[-(bins+1)])/2
  binWeights = pnorm(binEdges[-1], Income[p], SDfood[p]) - pnorm(binEdges[-(bins+1)], Income[p], SDfood[p])
  
  foodmatrix[p, ] <- binMids
  
}
binWeights <- binWeights / sum(binWeights)
binWeights = ifelse(is.na(binWeights) == TRUE, 1/bins, binWeights)

sto.food <- function (i) {
  sample(foodmatrix[i, ], size=1, prob=binWeights)
}

# matplot(foodmatrix, type="l", lty=1, lwd=2, col=c(gray.colors(bins,start=0.3, end=0.9)), ylab="Income (J)", xlab="Mass (kg)")

###COST FUNCTION  - assume metabolic requirements scale with body size and temperature
# MTcosts <- coef1*(Mass)^theta*exp(-E/(k*Temp))  #costs in J



##MTcosts<- matrix(nrow=length(tempK), ncol=Smax)
# tempK<-293:297
# for (S in (1:Smax)) {
# for(p in 1:length(tempK)) {
# S_g <- S 
# MTcosts[p, S] <-  coef*S_g^theta*exp(-E/(k*tempK[p]))/scale   
# }
# }

# ###plot metabolic cost functions for each temp to check they are sensible
# matplot( ((1:Smax)), t( (MTcosts)), type = "l", lty=1, lwd=2,   xlab=" (Mass (kg))", ylab="Metabolic rate in J/season", col=c(4, 3, "orange", 2, "dark red"))    
#################################################################################################################################################################################################


#DYNAMIC MODEL: life history in a single environment

#dynamic behaviors
u <-  seq(0, 1, by = 0.1) #fraction stored (rest allocated to structure)

r<-  seq(0, 1, by = 0.1) #fraction allocated to reproduction (rest is saved)


#set up arrays to store fitness values  
MaxF=array(dim=c(Estoresmax, Lmax, phi, Tmax),data=-1) #this will store the max fitness for every combination of state and times
MaxF[,,,Tmax]=0 #Fill fitness of Tmax column with 0
MaxF_wE <- MaxF
prob_denom <- MaxF

Vmat=array(dim=c(Estoresmax, Lmax, phi, Tmax-1, length(u), length(r) ))
#Vmat is the fitness value of all possible actions
Vmat[,,,,,]=rep(-1, length(Vmat))

delta <- array(dim=c(Estoresmax, Lmax, phi, Tmax-1, length(u), length(r) ))
prob_numer <- array(dim=c(Estoresmax, Lmax, phi, Tmax-1, length(u), length(r) ))
prob_error <- array(dim=c(Estoresmax, Lmax, phi, Tmax-1, length(u), length(r) ))

#store optimal behaviors
optU=array(dim=c(Estoresmax, Lmax, phi, Tmax-1))
optR=array(dim=c(Estoresmax, Lmax, phi, Tmax-1))


#pb <- txtProgressBar(min=0, max=Estoresmax, style=3) #set up progress bar in console window

#the dynamic loop:
Y <- 1
for (Y in 1:(Estoresmax)) { #for all   values of Energy Stores in loop (unscaled)
  
  #setTxtProgressBar(pb, Y) #progress bar tracks which value of Y we are on
  L <- Lmin
  for (L in Lmin:Lmax) { #for all possible values of Length
    p <- 1
    for (p in 1:phi) { #for every temp environment
      i <- Tmax-1	
      for (i in (Tmax-1):1) { #where i is time (age) in months
        
        
        if( i %in% c((1:16)*4-3) ==TRUE) season=1
        if( i %in% c((1:16)*4-2) ==TRUE) season=2
        if( i %in% c((1:16)*4-1) ==TRUE) season=3
        if( i %in% c((1:16)*4) ==TRUE) season=4
        g <- 1
        for (g in 1:length(u)) { #fractional placeholder (placeholder variable so we can loop over non-integers)
          
          growth = u[g] #Convert integer loop index to fractional value for allocation     
          
          
          h <- 1
          for(h in 1:length(r)) { #where r is fractional allocation to reproductive effort	
            
            reprod = r[h]  #Convert integer loop index to fractional value for allocation
            
            if (growth + reprod > 1)  Vmat[Y,L, p, i, g, h]	<- 0 else 
              
            {
             
             
               
              #calculate all states
              Wstructure<-a*L^3 #  structural mass in kg
              
              EstoresmaxL <-Wstructure*storelimit*scale #modified from Chapman et al.  
              #Energy stores are capped to be a fraction if TOTAL body mass
              
              EcritL <- Wstructure*storemin*scale
              
              if (Y*scale < EstoresmaxL)  Estores<- Y*scale  else 
                Estores=EstoresmaxL	#stored energy capped at a certain body size
              
              
              Wstores<-Estores/(scale) #stores mass in kG
              
              Wtotal <- Wstores+Wstructure  #body mass in KG
              
              Estructure <- Wstructure*scale
              Rlimit <- Estructure*reprolimit
              
              #state dynamics
              
              EstoresP <- Estores*(1-reprod-growth) +Income[season, L]*scale - MTcosts[season, L] #combines mass-dependent food intake and mass-dependent metabolic costs
              ##EstoresP <- Estores*(1-reprod-growth) +foodmatrix[ceiling(Wtotal), ] - MTcosts[ceiling(Wtotal)] #combines mass-dependent food intake and mass-dependent metabolic costs

              EstructureP <- Estructure + growth*Estores
              
              WstructureP <- EstructureP/scale 
              
              LengthP <-(WstructureP/a) ^(1/3)
              
              
              EstoresmaxLP <- EstructureP*storelimit 
              EcritLP <- EstructureP*storemin 
              EstoresP <- min(EstoresP, EstoresmaxLP) # this statement caps stores max storage allowed for that size
              EstoresP <- max(EstoresP, 0)	 #if Estores is negative, it is cut off at zero.   
              
              
              ####set up Interpolation of Estores
              
              dx <- EstoresP/scale - floor(EstoresP/scale)
              
              Yindex <- floor(EstoresP/scale)
              
              #make sure the state index does not exceed MAX possible state (AT HIGH END OF STATE RANGE)
              if(Yindex >= Estoresmax ) {
                Yindex = Estoresmax - 1 
                dx = 0 
              } #end if
               #Yindex <- ifelse(Yindex >= Estoresmax,  Estoresmax - 1, Yindex)

              if (ceiling(LengthP) > Lmax) Lindex <- Lmax else
                Lindex <- round(LengthP)
              
              #calculate future fitness with interpolation
              ###also make sure current state is not at critical value; otherwise store fitness from reproduction (which is current stores) plus anticipated future fitness if indvidual survives	
              #if future state is less than EcritL you DO NOT get future fitness  
              
              if(Estores >= EcritL & EstoresP >= EcritLP) {
                if (Yindex > 0 )  FutureFitness <- dx*MaxF[Yindex,Lindex,p,i+1] + (1-dx)*MaxF[Yindex+1, Lindex, p, i+1] else FutureFitness <- (1-dx)*MaxF[Yindex+1, Lindex, p, i+1]
              } else  FutureFitness <- 0
              
              # foodfit = rep(0, length(EstoresP))
              
              # for (f in 1:length(EstoresP)) {  
            #   if(Estores >= EcritL &   EstoresP[f] >= EcritLP) {
            #     if (Yindex[f] > 0 )  foodfit[f] <- dx[f]*MaxF[Yindex[f],Lindex,p,i+1] + (1-dx[f])*MaxF[Yindex[f]+1, Lindex, p, i+1] else foodfit[f] <- (1-dx[f])*MaxF[Yindex[f]+1, Lindex, p, i+1]
            #   } else  foodfit[f] <- 0
            #       }#end f loop
            # Wtfood<-foodfit*binWeights
            # FutureFitness = mean(Wtfood)
              
              
              
              #####if current state is greater than EcritL you get current fitness    	 
              
              if(Estores >= EcritL)  currentR <- min(reprod*Estores, Rlimit) else
                currentR <- 0
              
              
               Vmat[Y,L, p, i, g, h]	<-  currentR + exp(-mu[L])*FutureFitness 
              
              
            } #end if growth + reprod < 1	   
            
            
          } #end h loop
        } #end g loop
        
        #find and store the highest fitness from growth and reproduction combinations for all other variables.
        MaxF[Y,L,p,i] <-  max(Vmat[Y,L, p, i, , ])
        
        #find out th differences in fitness of all strategies from the maximum, use the mean if a tie, record the difference for later error introduction:
        
        mult_u <- rep(NA, length(u))
        mult_r <- rep(NA, length(r))
        
        for (g in length(u):1) {
          for(h in length(r):1) {
            
            
            
            if (MaxF[Y, L,p,i] - Vmat[Y,L, p, i, g, h]   == 0) {
              
              mult_u[g] <- u[g]	   #record all behaviors that have the same fitnes as the max.         
              mult_r[h] <- r[h]	
              
            }  #end if
            
          } # end 2nd h loop  
        } #end 2nd g loop
        
        #take mean of all behaviors with same fitness as max
        
        optU[Y, L,p,i]=mean(mult_u, na.rm=TRUE) 
        optR[Y, L,p,i]=mean(mult_r, na.rm=TRUE)  
        
        #if(optU[Y, L,p,i]+optR[Y, L,p,i] > 1) optR[Y, L,p,i]<- optR[Y, L,p,i] - 0.1 
        
        
      } #end i loop
    }	#end p loop
  } #end L loop
  
} #end Y loop
 
# close(pb) #close progress bar
# require(fields)
# pal=terrain.colors(n=100)# ##set the palette
# quartz()
# par(mfrow=c(2,2)) 	
# image(1:10, 45:100, optU[1:10 , 45:100 ,1,1], col=pal, ylab="Size", xlab="Energy Stores", main="Growth, Age is 1" )
# image(optU[  , 100  , 1,   ], col=pal, ylab="Age", xlab="growth", main="Growth, Length is 100" ) 

# image(optU[ , 200 , 1,  ] , col=pal, ylab="Age", xlab="Energy Stores", main="Growth, Length is 200" ) 


# quartz()
# par(mfrow=c(2,2)) 	

# image(1:10, 45:100, optR[1:10, 45:100 ,1,1], col=pal, ylab="Size", xlab="Energy Stores", main="Repro, Age is 1 " )	   	   

# image(optR[ , 100  , 1,  ],col=pal, ylab="Age", xlab="Energy Stores", main="R, Length is 100" ) 

# image(optR[, 200 , 1, ], col=pal, ylab="Age", xlab="Energy Stores", main="R, Length is 200" ) 


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

#z=rnorm(nindiv, mean=scale*a*initialsize^3*(storelimit - 0.05), sd=0) ## Generate a population (z) of indivdiuals, condition based on weight  (95% of max for size, with some variation)
z<- scale*a*initialsize^3*(storelimit - 0.05)

idist[,1]=ceiling(z) #this rounds every z up to the nearest integer for the first time step  
#idist[,1] is the initial state -- better to use ceiling() than floor()
sizedist[,1]<- initialsize   
#stores number of survivors at each time

reproduction=matrix(0, nindiv, Tmax) #stores how much they reproduce at each time. 

#draw random numbers for every individual's survival chance at every time (above or below exp(-mu))  
randraw=matrix(runif(nindiv*(Tmax), max=1, min=0), nrow=nindiv, ncol=Tmax)
randraw2=matrix(runif(nindiv*(Tmax), max=1, min=0), nrow=nindiv, ncol=Tmax)
normdraw=matrix(rnorm(nindiv*(Tmax), mean=1, sd=0.005), nrow=nindiv, ncol=Tmax) #add stochasticity in food intake

survival=rep(0, Tmax)
survival[1]<-1

for (i in 1:(Tmax-1)) { 
  if( i %in% c((1:16)*4-3) ==TRUE) season=1
  if( i %in% c((1:16)*4-2) ==TRUE) season=2
  if( i %in% c((1:16)*4-1) ==TRUE) season=3
  if( i %in% c((1:16)*4) ==TRUE) season=4
  
  state  <- idist[,i] 
  
  size <- round(sizedist[,i])  
   
  
  EcritL <-  scale*a*size^3*storemin   
  
  index <- which(state >= EcritL) #which individuals are still alive (didn't starve)
  
  if(length(index) > 1) {
    
    ##find interpolated behaviors
    Ilo <- floor(state[index]/scale)
    
    #these ifelse statemnts deal with the upper boundary on Estores (energetic state):
    #find the allocation strategies of individuals of these states and age, regardless of   whether they survive  (since they behave first) 
    Ilo <- ifelse(Ilo >= Estoresmax, Estoresmax-1, Ilo)   
    dx <- ifelse(Ilo >= Estoresmax, 1, state[index]/scale - Ilo) 
    
    ##NOW WE NEED TO LOOK UP THE OPTIMAL BEHAVIORS FROM THE BACKWARD SOLUTION
    #CAREFULLY DEALING WITH THE CASE OF LOW STATE 
    condind <- Ilo == 0
    
    ##   if state is between 0 and 1 (Ilo = 0)the strategy is only (1 - dx)*(state=1)
    g_allo[index[condind==TRUE], i] <- round((1 - dx[condind==TRUE])*diag(optU[Ilo[condind==TRUE]+1, size[index[condind==TRUE]], p, i]),1)  
    repro[index[condind==TRUE], i] <- round((1 - dx[condind==TRUE])*diag(optR[Ilo[condind==TRUE]+1, size[index[condind==TRUE]], p, i]),1) 
    
    g_allo[index[condind==FALSE], i] <- round(dx[condind==FALSE]*diag(optU[Ilo[condind==FALSE], size[index[condind==FALSE]], p, i]) + (1 - dx[condind==FALSE])*diag(optU[Ilo[condind==FALSE]+1, size[index[condind==FALSE]], p, i]),1)  
    repro[index[condind==FALSE], i] <-round(dx[condind==FALSE]*diag(optR[Ilo[condind==FALSE], size[index[condind==FALSE]], p, i]) +  (1 - dx[condind==FALSE])*diag(optR[Ilo[condind==FALSE]+1, size[index[condind==FALSE]], p, i]),1	)	  					  
    
    #now calculate Wtotal, Costs, and Net energy intake
    
    Wstructure<- a*size[index]^3 
     
    
    Estructure <- Wstructure*scale
    Replim <- Estructure*reprolimit       
    EstoresmaxL <-Wstructure*storelimit*scale #modified from Chapman et al.  
    #Energy stores are capped to be a fraction if TOTAL body mass
    
    EcritL <- Wstructure*storemin*scale
    
    if (Y*scale < EstoresmaxL)  Estores<- Y*scale  else 
      Estores=EstoresmaxL	#stored energy capped at a certain body size
    
    Wstores<-state[index]/(scale)
    
    Wtotal <-  Wstores+Wstructure   #body mass
    
    
    # REPRODUCE AND GROW BEFORE SURVIAL IS DETERMINED    
    
    reproduction[index, i]<- ifelse(repro[index, i]*state[index] < Replim, repro[index, i]*state[index], Replim)    
    
    nextsize <-   ((Wstructure +  g_allo[index,i]*Wstores)/a)^(1/3)
    
    #survival <- randraw[index,i] <= exp(-mu[size[index]])  
    survival[i+1] <- survival[i]*exp(-mu[size[index]])
    
    critstores <- a*nextsize^3*storemin*scale
    
    Food<-sapply(size[index], sto.food) #calculates stochastic food quantity for every index individual
    
    #####future state calculation:
   #survival2<- ifelse(((1-repro[index, i]-g_allo[index,i])*state[index] + Income[season, size[index]]*scale - MTcosts[season, size[index]])  > critstores, 1, 0) #check that future state will be greater than current EcritL 
    
    #idist[index,i+1] <- ifelse(survival+survival2==2, ((1-repro[index, i]-g_allo[index,i])*state[index] + Income[season, size[index]]*scale - MTcosts[season, size[index]]),  NA)
    idist[index,i+1] <- (1-repro[index, i]-g_allo[index,i])*state[index] + Income[season, size[index]]*scale - MTcosts[season, size[index]] 
    # survival2<- ifelse(((1-repro[index, i]-g_allo[index,i])*state[index] + Food - MTcosts[size[index]])  > critstores, 1, 0) #check that future state will be greater than current EcritL
    # idist[index,i+1] <- ifelse(survival+survival2==2, ((1-repro[index, i]-g_allo[index,i])*state[index] + Food - MTcosts[size[index]]),  NA)

    #sizedist[index, i+1] <- ifelse(survival+survival2==2,  nextsize, NA)   
   sizedist[index, i+1] <- nextsize
    
    #alive[group, i+1]=sum(idist[,i+1] > 0, na.rm=TRUE) #number of survivors
    
    
  }#end if
}     #end time (i) loop

x1=3*timebin+1
y1= (colSums(alive))[x1]

x2=4*timebin 

y2=(colSums(alive))[x2]

reproduction[, -Tmax]<-ifelse(reproduction[, -Tmax]>0,  reproduction[, -Tmax], NA)

idist[, -Tmax]<-ifelse(idist[, -Tmax]>0,  idist[, -Tmax], NA)

write.csv(idist, file=paste0("monthly_model_output/03StateRlim0.5",  "f_h", f_h,  "Kappa", Kappa,  ".csv"))

write.csv(sizedist, file=paste0("monthly_model_output/01LengthRlim0.5",  "f_h", f_h,   "Kappa", Kappa,  ".csv"))
write.csv(reproduction, file=paste0("monthly_model_output/02ReproRlim0.5",  "f_h", f_h,   "Kappa", Kappa,   ".csv")) 
write.csv(survival, file=paste0("monthly_model_output/04SurvRlim0.5",  "f_h", f_h,   "Kappa", Kappa,   ".csv")) 
  
