mci.transvar <-
function (mcidataset, submarkets, suppliers, mcivariable, output_ij = FALSE, output_var = "numeric", show_proc = FALSE, check_df = TRUE) 
{   

  if (check_df == TRUE) 
  {
    if (exists(as.character(substitute(mcidataset)))) { 
      checkdf(mcidataset, submarkets, suppliers, mcivariable)
    }
    else {
      stop(paste("Dataset", as.character(substitute(mcidataset))), " not found", call. = FALSE)
    }
  }
    
  if (show_proc == TRUE)
  {
    cat ("Processing variable", names(mcidataset[mcivariable]), "...", "\n")
  }
    
  sort_i_j <- order(mcidataset[[submarkets]], mcidataset[[suppliers]])

  mciworkfile <- mcidataset[sort_i_j,]   

  if (checkvar(mciworkfile[[mcivariable]]) == "valid_d") {

    if (show_proc == TRUE)
    {
      cat(names(mciworkfile[mcivariable]), "is treated as dummy variable (no log-centering transformation) \n")
    }
    
    return(mciworkfile[mcivariable]) 
  }
  
  if (checkvar(mciworkfile[[mcivariable]]) == "valid_n") {

    logvarnewname <- paste(names(mciworkfile[mcivariable]), "_t", sep="")   

    submarkets_single <- levels(as.factor(as.character(mciworkfile[[submarkets]])))
    suppliers_single <- levels(as.factor(as.character(mciworkfile[[suppliers]])))
    submarkets_count <- nlevels(as.factor(as.character(mciworkfile[[submarkets]])))
    suppliers_count <- nlevels(as.factor(as.character(mciworkfile[[suppliers]])))

    submarket_i_geom <- 0   
    submarket_i_rel <- 0   
    submarket_i_rel_log <- 0   

    mcivariablelog <- vector()  

    for(i in 1:submarkets_count){   

      submarket_i <- subset (mciworkfile, mciworkfile[[submarkets]] == submarkets_single[i])  
      submarket_i_geom[i] <- geom (submarket_i[[mcivariable]])  

      for(j in 1:suppliers_count) {   
        submarket_i_rel[j] <- submarket_i[[mcivariable]][j]/submarket_i_geom[i]  
        submarket_i_rel_log[j] <- log10(submarket_i_rel[j])   
        mcivariablelog <- rbind(mcivariablelog, list(submarket_i_rel_log[j]))  

      }   
    }   
    
    mcilinvar <- as.data.frame(mcivariablelog)   
    names(mcilinvar) <- logvarnewname   

    if (output_ij == TRUE) {
      mcilinoutput <- cbind(mciworkfile[submarkets], mciworkfile[suppliers], mcilinvar)
      
      if (output_var == "numeric") { mcilinoutput[3] <- as.numeric(unlist(mcilinoutput[3])) } 
      
      return(mcilinoutput)
    }
    else {
      if (output_var == "numeric") { return(as.numeric(unlist(mcilinvar))) }
      if (output_var == "list") { return(mcilinvar) }  
    }
  }   
  else {
    if (checkvar(mciworkfile[[mcivariable]]) == "invalid_s")
    {
      stop(paste("Variable", names(mciworkfile[mcivariable]), "is invalid (contains strings) \n"), call. = FALSE)
    }
    
    if (checkvar(mciworkfile[[mcivariable]]) == "invalid_zn")
    {
      stop(paste("Variable", names(mciworkfile[mcivariable]), "is invalid (contains zero and/or negative values) \n"), call. = FALSE)
    }
  }   

}
