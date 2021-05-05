install.packages("readr")
install.packages("ggplot2")
install.packages("tidyverse")


library(readr)
library(ggplot2)
library(tidyverse)
mort <- 0.33
rate_dat<-read_csv("/Users/hollykindsvater/Documents/tuna_theory_paper/october_run_results/october_results.csv") #bring in the summary spreadsheet I made of trait data
 
 a <- ggplot(rate_dat %>% 
                filter(F_H==mort),
    aes(x=Kappa, y=Amat) 
    ) + 
   # geom_point(size = 2, shape = factor(Temp)) +
	geom_point(aes(colour = factor(Temp), shape=factor(Temp)), size = 4) +
  
  scale_shape_manual(values=c(8, 16)) +
    scale_color_manual(values = c("blue", "red")) +
   labs(y="Age at 50% Maturity (years)")  +
   # 
  facet_wrap(~Seasonality) 
quartz()
a +theme_bw()

  
 
c <- ggplot(rate_dat %>% 
            filter(F_H==mort),
            aes(x=Kappa, y=survival) 
) + 
  # geom_point(size = 2, shape = factor(Temp)) +
  geom_point(aes(colour = factor(Temp), shape=factor(Temp)), size = 4) +
  
  scale_shape_manual(values=c(8, 16)) +
  scale_color_manual(values = c("blue", "red")) +
   labs(y="Annual survival")+

  
 facet_wrap(~Seasonality) 
quartz()
c +theme_bw()
 
 
 
d <- ggplot(rate_dat %>%
              filter(F_H==mort),
            aes(x=Kappa, y=alive_at_15) 
) + 
  # geom_point(size = 2, shape = factor(Temp)) +
  geom_point(aes(colour = factor(Temp), shape=factor(Temp)), size = 4) +
  
  scale_shape_manual(values=c(8, 16)) +
  scale_color_manual(values = c("blue", "red")) +
  labs(y="Survivors at age 15")+
  
  
  facet_wrap(~Seasonality) 
quartz()
 d +theme_bw()
 
  
E <- ggplot(rate_dat %>%
              filter(F_H==mort),
            aes(x=Kappa, y=Sizemat) 
) + 
  # geom_point(size = 2, shape = factor(Temp)) +
  geom_point(aes(colour = factor(Temp), shape=factor(Temp)), size = 4) +
  
  scale_shape_manual(values=c(8, 16)) +
  scale_color_manual(values = c("blue", "red")) +
  labs(y="Size at maturity (cm)")+
  
  
   facet_wrap(~Seasonality) 
quartz()
E +theme_bw()
ff <- ggplot(rate_dat %>%
              filter(F_H==mort),
            aes(x=Kappa, y=bodysize) 
) + 
  # geom_point(size = 2, shape = factor(Temp)) +
  geom_point(aes(colour = factor(Temp), shape=factor(Temp)), size = 4) +
  
  scale_shape_manual(values=c(8, 16)) +
  scale_color_manual(values = c("blue", "red")) +
  labs(y="Body Length (cm)")+
  
  
   facet_wrap(~Seasonality) 
quartz()
ff +theme_bw()

  