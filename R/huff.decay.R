huff.decay <-
function (dataset, x, y, plots = FALSE) 
{
  
  if (exists(as.character(substitute(dataset)))) { 
    checkdf(dataset, x, y)
  }
  else {
    stop(paste("Dataset", as.character(substitute(dataset))), " not found", call. = FALSE)
  }

  if (checkvar(dataset[[x]]) == "invalid_s") stop(paste("Variable", names(dataset[x]), "is invalid (contains strings) \n"), call. = FALSE)
  if (checkvar(dataset[[x]]) == "invalid_zn") stop(paste("Variable", names(dataset[x]), "is invalid (contains zero and/or negative values) \n"), call. = FALSE)
  if (checkvar(dataset[[y]]) == "invalid_s") stop(paste("Variable", names(dataset[y]), "is invalid (contains strings) \n"), call. = FALSE)
  if (checkvar(dataset[[y]]) == "invalid_zn") stop(paste("Variable", names(dataset[y]), "is invalid (contains zero and/or negative values) \n"), call. = FALSE)
  
  
  decayworkfile <- dataset
  
  results <- data.frame()
  
  lin <- lm(decayworkfile[[y]]~decayworkfile[[x]])

  lin_intercept <- round(as.numeric(lin$coefficients[1]), 4)
  lin_intercept_p <- round(as.numeric(summary(lin)$coefficients[1,4]), 4)
  lin_slope <- round(as.numeric(lin$coefficients[2]), 4)
  lin_slope_p <- round(as.numeric(summary(lin)$coefficients[2,4]), 4)
  lin_rsq <- round(as.numeric(summary(lin)$r.squared), 4)
  lin_rsqa <- round(as.numeric(summary(lin)$adj.r.squared), 4)
  
  lin_results <- as.data.frame(cbind("Linear", lin_intercept, lin_intercept_p, lin_slope, 
                                     lin_slope_p, lin_rsq, lin_rsqa))
  colnames(lin_results) <- c("Model type", "Intercept", "p Intercept", "Slope", "p Slope", 
                             "R-Squared", "Adj. R-squared")

  results <- rbind(results, lin_results)

  pow <- lm(log10(decayworkfile[[y]])~log10(decayworkfile[[x]]))
  
  pow_intercept_log <- round(as.numeric(pow$coefficients[1]), 4)
  pow_intercept <- round(10^pow_intercept_log, 4)
  pow_intercept_p <- round(as.numeric(summary(pow)$coefficients[1,4]), 4)
  pow_slope <- round(as.numeric(pow$coefficients[2]), 4)
  pow_slope_p <- round(as.numeric(summary(pow)$coefficients[2,4]), 4)
  pow_rsq <- round(as.numeric(summary(pow)$r.squared), 4)
  pow_rsqa <- round(as.numeric(summary(pow)$adj.r.squared), 4)
  
  pow_results <- as.data.frame(cbind("Power", pow_intercept, pow_intercept_p, pow_slope, 
                                     pow_slope_p, pow_rsq, pow_rsqa))
  colnames(pow_results) <- c("Model type", "Intercept", "p Intercept", "Slope", "p Slope", 
                             "R-Squared", "Adj. R-squared")
  
  results <- rbind(results, pow_results)
  
  expo <- lm(log(decayworkfile[[y]])~decayworkfile[[x]])
  
  expo_intercept_log <- round(as.numeric(expo$coefficients[1]), 4)
  expo_intercept <- round(exp(expo_intercept_log), 4)
  expo_intercept_p <- round(as.numeric(summary(expo)$coefficients[1,4]), 4)
  expo_slope <- round(as.numeric(expo$coefficients[2]), 4)
  expo_slope_p <- round(as.numeric(summary(expo)$coefficients[2,4]), 4)
  expo_rsq <- round(as.numeric(summary(expo)$r.squared), 4)
  expo_rsqa <- round(as.numeric(summary(expo)$adj.r.squared), 4)
  
  expo_results <- as.data.frame(cbind("Exponential", expo_intercept, expo_intercept_p, 
                                      expo_slope, expo_slope_p, expo_rsq, expo_rsqa))
  colnames(expo_results) <- c("Model type", "Intercept", "p Intercept", "Slope", "p Slope", 
                              "R-Squared", "Adj. R-squared")
  
  results <- rbind(results, expo_results)
  
  
  y_limit <- max(decayworkfile[[y]])*1.01

  z <- as.numeric(log((y_limit-decayworkfile[[y]])/decayworkfile[[y]]))

  logist <- lm(z~decayworkfile[[x]])

  logist_intercept <- round(as.numeric(logist$coefficients[1]), 4)
  logist_intercept_p <- round(as.numeric(summary(logist)$coefficients[1,4]), 4)
  logist_slope <- round(as.numeric(logist$coefficients[2]), 4)
  logist_slope_p <- round(as.numeric(summary(logist)$coefficients[2,4]), 4)
  logist_rsq <- round(as.numeric(summary(logist)$r.squared), 4)
  logist_rsqa <- round(as.numeric(summary(logist)$adj.r.squared), 4)
  
  logist_results <- as.data.frame(cbind("Logistic", logist_intercept, logist_intercept_p, 
                                        logist_slope, logist_slope_p, logist_rsq, logist_rsqa))
  colnames(logist_results) <- c("Model type", "Intercept", "p Intercept", "Slope", "p Slope", 
                                "R-Squared", "Adj. R-squared")
  
  results <- rbind(results, logist_results)
  
  plot(decayworkfile[[x]], decayworkfile[[y]], xlab=x, ylab=y, pch=20, col="black", 
       main="Distance decay functions")

  x_val <- seq(min(decayworkfile[[x]]), max(decayworkfile[[x]]))

  lin_y_p <- lin_intercept+lin_slope*x_val
  lines(x_val, lin_y_p, col="orange", lwd=1.5)
  
  pow_y_p <- pow_intercept*x_val^pow_slope
  lines(x_val, pow_y_p, col="green", lwd=1.5)
  
  expo_y_p <- expo_intercept*exp(expo_slope*x_val)
  lines(x_val, expo_y_p, col="red", lwd=1.5)
  
  logist_y_p <- (y_limit/(1+exp(logist_intercept+logist_slope*x_val)))
  lines(x_val, logist_y_p, type="l", col="blue", lwd=1.5)
  
  legend ("topright", c("Linear", "Power", "Exponential", "Logistic"), lty=c(1,1), 
          col=c("orange","green", "red", "blue"), cex=0.8)
  
  return (results)
  
}
