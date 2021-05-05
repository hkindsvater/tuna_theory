install.packages("FSA")
install.packages("nlstools")

library(FSA)
library(nlstools)
age <- 13:156 #note we are focused only on growth from 1 year to 13 years of age..... 
data1 <- read.csv("~/Documents/tuna_theory_paper/october_run_results/Length_data/med_kappa/4consLengthTemp295f_h0.33Kappa0.21reprolimit0.2Tmax216.csv")

data2 <- as.data.frame(t(rbind(age, data1[1, 2:145])))
names(data2) <- c("age", "len")

SV <- vbStarts(len~age, data <- data2)
SV

von_bert <- len~Linf*(1-exp(-K*(age - t0)))
fitVB <- nls(von_bert, data = data2, start = SV)

von_bert
coef(fitVB)