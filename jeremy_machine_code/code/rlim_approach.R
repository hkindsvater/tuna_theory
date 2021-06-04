
plot(Estores, reprod * Estores, type="l", lty="dotted")
points(range(Estores), rep(Rlimit, 2), type="l", lty="dashed")
points(Estores, (reprod * Estores) / (1 + ((reprod*Estores) / Rlimit)), type="l", col="orange")
points(Estores, Rlimit / ( (Rlimit / (reprod * Estores)) + 1), type="l", col="dodgerblue")