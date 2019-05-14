sam <- matrix(ncol=1500, nrow = 1000)
for (i in 1:1000) {
   sam[i, ] <- (1:1500)*rnorm(1500, mean = 1, sd = 0.1)
    }
 matplot(sam)