 
 windowframe=c(3,1)
  
# #    quartz()
    # par(mfrow=windowframe)

setwd("~/Documents/tuna_theory_paper/october_run_results/Length_data/hi_kappa/")
 
data_files <- list.files(pattern = "\\.csv$")
 
length_filenames <- data_files[1:(length(data_files))]
 
length_data <- lapply(length_filenames, read.csv)
  Tmax=18
 time=1:(Tmax*12)
 
  
 	   
# 	   
     matplot(cbind(as.numeric(length_data[[1]][1,]), as.numeric(length_data[[2]][1,]), as.numeric(length_data[[3]][1,]), as.numeric(length_data[[4]][1,])), type="l", col=c(4, 4, 2, 2), lwd=3, lty=c(1, 3, 1, 3),  ylab="Length (cm)", ylim=c(0, 300), xlim=c(0.5, 11*12), xlab= "Age (years)", xaxt="n")
     axis(1, at = seq(0, (Tmax)*12, by=12), labels = (seq(1, Tmax+1, by=1)))
      
          
           
  # plot_hist <- function(data, filenames) {
    # hist(data[, 12], breaks=50, xlim=c(50, 350), main=substr(filenames, 13, 49))
  # }
   
  # mapply(plot_hist, length_data, length_filenames)
  
# #  

# # 	   
     # matplot(cbind(as.numeric(length_data[[1]][1,]), as.numeric(length_data[[2]][1,])), type="l", col=c(4,   2), lwd=3, lty=c(3, 3),  ylab="Length (cm)", ylim=c(0, 300), xlim=c(0.5, 11*12), xlab= "Age (years)", xaxt="n")
     # axis(1, at = seq(0, (Tmax)*12, by=12), labels = (seq(1, Tmax+1, by=1)))
      
