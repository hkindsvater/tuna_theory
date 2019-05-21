 
 
 Mass=1:1500
phi_a <- 3 #from table 2.2 in Andersen book
K_c <- 10 #from table 2.2, this is averaged over "all" - so PP in stomach of all preds and preys have a MR of 1224 independently of body size - but htis is something that changes with ecosystem according to KAPPA, eg less in deep sea, more in upwelling
lam <- 1.95
Kappa=3
 
Income = Kappa*phi_a*K_c*Mass^(0.05) #this describes the scaling with size and ecostystem richness
 
plot(Income, type="l")



foodmat <- matrix(nrow = 1500, ncol=200)

for (i in 1:1500) {
  foodmat[i, ] <- rnorm(100, mean=Income[i], sd=0.015*Income[i])
  
}
  
 matplot((foodmat), type="p", pch=19, col="darkgray")
 lines(Income, lwd=2)
 
 par(mfrow=c(3, 3))
 Isub=seq(1, 1500, 60)
 for (j in 1:25) {
 hist(foodmat[Isub[j], ])
   
 }
 
 
 