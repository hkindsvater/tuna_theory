#!/usr/bin/env Rscript


# Defaults
paras = c(reprolimit=0.2, Kappa = 1, Temp=295, Tmax=18)

# Name the set of results
name = "constant/"
# newDir = paste("mkdir ~/Desktop/holly_results/", name, sep="") 
# system(newDir)
 
# Pick two variables to vary factorially
  
v1 = "Temp"
v2 = "Kappa"

L1 =  c(290, 295)
L2 = seq(0.8, 4, by=0.2)

n1 = length(L1)
n2 = length(L2)
reps = 1
p1 = rep(rep(L1, n2), reps)
p2 = rep(sort(rep(L2, n1)), reps)
limit = 1
total = n1 * n2 * reps
index = 1

while(index <= total)
{
	if(length(suppressWarnings(system2("pgrep", "-f tuna_model_seasonsJan2021.R", stdout=TRUE))) < limit)
	{
		argList = ""
		argList = paste(argList, index, " ", sep="")
		for(i in 1:length(paras))
		{
			x = paras[i]
			if(names(paras)[i] == v1) x = p1[index]
			if(names(paras)[i] == v2) x = p2[index]
			argList = paste(argList, x, " ", sep="")
		}

		system(paste("Rscript ~/Desktop/holly_results/code/tuna_model_seasonsJan2021.R", argList) ,wait=FALSE)
		print(index)
		index = index + 1
	} else {
		Sys.sleep(60)
	}
}








