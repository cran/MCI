var.zetas <-
function (x) {
  mean_x <- mean(x)
  sd_x <- sd(x)
  z_x <- (x-mean_x)/sd_x

  i <- 0
  l_zx <- length(z_x)
  zeta <- vector(mode = "numeric")

  for (i in 1:l_zx)
  {
    if (z_x[i] >= 0) zeta[i] <- 1+z_x[i]^2
    if (z_x[i] <= 0) zeta[i] <- 1/(1+z_x[i]^2)
  }
  
  return(zeta)

}
