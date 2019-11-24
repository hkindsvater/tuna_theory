#!/usr/bin/env Rscript


# Defaults
paras = c(reprolimit=0.2, Kappa = 1, f_h=5)

# Name the set of results
name = "seasons"
newDir = paste("mkdir ~/Documents/tuna_theory/Temp290/", name, sep="") 
system(newDir)


# Pick two variables to vary factorially
  
v1 = "f_h"
v2 = "Kappa"

L1 =  c(15.25 )

L2 = c(15)
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
	if(length(suppressWarnings(system2("pgrep", "-f tuna_model_seasons.R", stdout=TRUE))) < limit)
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

		system(paste("Rscript ~/Documents/tuna_theory/tuna_model_seasons.R", argList) ,wait=FALSE)
		print(index)
		index = index + 1
	} else {
		Sys.sleep(60)
	}
}








