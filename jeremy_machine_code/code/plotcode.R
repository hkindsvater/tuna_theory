plot(Jdensity*a*Lmax^3*((1:100)^3)*storemax/(Yindexmax^3), Fmat[,30, 1,Tmax-2, 1, 1])
quartz()
par(mfrow=c(3,2))
gcol=c("orange", two.colors(n=10, start="white", middle = "gray",  end="black"))

 
image.plot(1:Lmax, 1:Yindexmax, t(optU[, , 1,30]), breaks = c(-1.1, -0.9, seq(0.1,1, by=0.1)), col=gcol,  ylab="State", xlab="Length", main="growth")       

image.plot(1:Lmax, 1:Yindexmax, t(optR[, , 1,30]),breaks = c(-1.1, -0.9, seq(0.1,1, by=0.1)), col=gcol,  ylab="State", xlab="Length", main="reproduction")       
# 
#  image.plot(1:Lmax, 1:Yindexmax, t(optU[,, 1,Tmax-2]), breaks = c(-1.5, -0.5, seq(0.1,1, by=0.1)), col=gcol,  ylab="State", xlab="Length", main="growth")       
#  
#  image.plot(1:Lmax, 1:Yindexmax, t(optR[,, 1,Tmax-2]),breaks = c(-1.5, -0.5, seq(0.1,1, by=0.1)), col=gcol,  ylab="State", xlab="Length", main="reproduction")       
#   
 image.plot(1:(Tmax-1), 1:Yindexmax, t(optU[,20, 1,]),breaks = c(-1.1, -0.9, seq(0.1,1, by=0.1)), col=gcol,  ylab="State", xlab="Age", main="growth")       
 
 image.plot(1:(Tmax-1), 1:Yindexmax, t(optR[,20, 1,]),breaks = c(-1.1, -0.9, seq(0.1,1, by=0.1)), col=gcol,  ylab="State", xlab="Age", main="reproduction")       
 
 
 image.plot(1:(Tmax-1), 1:Lmax, t(optU[80, , 1,]),breaks = c(-1.1, -0.9, seq(0.1,1, by=0.1)), col=gcol,  ylab="Length", xlab="Age", main="growth")       
 
 image.plot(1:(Tmax-1), 1:Lmax, t(optR[80, , 1,]),breaks = c(-1.1, -0.9, seq(0.1,1, by=0.1)), col=gcol,  ylab="Length", xlab="Age", main="reproduction")       
 