ijmatrix.shares <-
function (rawmatrix, submarkets, suppliers, observations, 
                             varname_total = "freq_i_total", varname_shares = "p_ij_obs") 
  
  {
  
  if (exists(as.character(substitute(rawmatrix)))) { 
    checkdf(rawmatrix, submarkets, suppliers, observations)
  }
  else {
    stop(paste("Dataset", as.character(substitute(rawmatrix))), " not found", call. = FALSE)
  }

  if (checkvar(rawmatrix[[observations]]) == "invalid_s")
    { 
    stop(paste("Variable", names(rawmatrix[observations]), "is invalid (contains strings) \n"), 
         call. = FALSE) 
    }
  
  mcirawmatrix <- rawmatrix   

  submarkets_single <- levels(as.factor(as.character(mcirawmatrix[[submarkets]])))

  suppliers_single <- levels(as.factor(as.character(mcirawmatrix[[suppliers]])))

  submarkets_colname <- names(rawmatrix[submarkets])
  suppliers_colname <- names(rawmatrix[suppliers])
  
  submarkets_count <- nlevels(as.factor(as.character(mcirawmatrix[[submarkets]])))

  suppliers_count <- nlevels(as.factor(as.character(mcirawmatrix[[suppliers]])))

  p_ij_obs <- vector()
  freq_ij_rel <- vector()
  freq_i_abs <- vector()
  freq_ij_rel_j <- vector()
  submarket_i_total <- 0
  
  for(i in 1:submarkets_count){   

    submarket_i <- subset (mcirawmatrix, mcirawmatrix[[submarkets_colname]] == submarkets_single[i])  

    submarket_i_total[i] <- sum (submarket_i[[observations]])

    for(j in 1:suppliers_count) {   

      freq_ij_rel_j[j] <- submarket_i[[observations]][j]/submarket_i_total[i]
      
      freq_i_abs <- rbind(freq_i_abs, as.numeric(list(submarket_i_total[i])))
      
    }   
  }   
  
  mcirawmatrix[varname_total] <- freq_i_abs 
  
  mcirawmatrix[varname_shares] <- mcirawmatrix[[observations]]/mcirawmatrix[[varname_total]]
  
  return(mcirawmatrix)
  
}
