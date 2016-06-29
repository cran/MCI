var.correct <-
function (x, corr.mode = "inc", incby = 1) 
{
  
  y <- 0
  
  if (corr.mode == "inc") { 
    y <- x+incby   
  }
  
  if (corr.mode == "incabs") {
    xmin <- abs(min(x))

    y <- x+xmin+incby
  }
 
  if (corr.mode == "zetas") {

    mean_x <- mean(x)
    sd_x <- sd(x)
    z_x <- (x-mean_x)/sd_x

    i <- 0
    l_zx <- length(z_x)
    zeta <- vector(mode = "numeric")

    for (i in 1:l_zx)
    {
      if (z_x[i] >= 0) y[i] <- 1+z_x[i]^2
      if (z_x[i] <= 0) y[i] <- 1/(1+z_x[i]^2)
    }
    
  }
  
  return(y)
}
