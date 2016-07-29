huff.attrac <-
function (huffdataset, origins, locations, attrac, dist, lambda = -2, dtype= "pow", lambda2 = NULL, localmarket_dataset, localmarket, location_dataset, location_id, location_total, tolerance = 5, output = "matrix", show_proc = FALSE, check_df = TRUE)
{
  
  if (check_df == TRUE)
  {
    if (exists(as.character(substitute(huffdataset)))) { 
      checkdf(huffdataset, origins, locations, attrac, dist)
    }
    else {
      stop(paste("Dataset", as.character(substitute(huffdataset))), " not found", call. = FALSE)
    }

    if (exists(as.character(substitute(localmarket_dataset)))) { 
      checkdf(localmarket_dataset, localmarket)
    }
    else {
      stop(paste("Dataset", as.character(substitute(localmarket_dataset))), " not found", call. = FALSE)
    }
    
    if (exists(as.character(substitute(location_dataset)))) { 
      checkdf(location_dataset, location_id, location_total)
    }
    else {
      stop(paste("Dataset", as.character(substitute(location_dataset))), " not found", call. = FALSE)
    }
    
  }

  sort_i_j <- order(huffdataset[[origins]], huffdataset[[locations]])   

  huffworkfile <- huffdataset[sort_i_j,]   

  origins_single <- levels(as.factor(as.character(huffdataset[[origins]])))

  origins_count <- nlevels(as.factor(as.character(huffdataset[[origins]])))

  locations_single <- levels(as.factor(as.character(huffdataset[[locations]])))

  locations_count <- nlevels(as.factor(as.character(huffdataset[[locations]])))

  huffworkfile <- merge (huffdataset, localmarket_dataset)

  huff_shares <- huff.shares(huffworkfile, origins, locations, attrac, dist, lambda = lambda, dtype = dtype, lambda2 = lambda2, check_df = FALSE)

  huff_total <- shares.total(huff_shares, origins, locations, "p_ij", localmarket, check_df = FALSE)

  huff_total_suppdata <- merge (huff_total, location_dataset, by.x="suppliers_single", by.y=location_id)

  locations_attrac <- paste0(huffdataset[[locations]], ":", huffdataset[[attrac]])
  locations_attrac_fac <- levels(as.factor(locations_attrac))
  locations_attrac_split <- strsplit(locations_attrac_fac, ":")
  locations_attrac_df <- data.frame(do.call(rbind, locations_attrac_split))
  colnames(locations_attrac_df) <- c("suppliers_single", names(huffworkfile[attrac]))

  huff_total_suppdata_complete <- merge (huff_total_suppdata, locations_attrac_df)

  k <- 0
  
  total_obs <- vector()
  total_exp1 <- vector()
  total_exp2 <- vector()

  attrac_new <- vector()
  attrac_old <- vector()
  attrac_new_opt <- vector()
  
  diff_rel_old <- vector()
  diff_rel_new <- vector()
  
  a <- vector()
  b <- vector()
  
  for (k in 1:locations_count)
  {
    
    attrac_old[k] <- as.numeric(as.character(huff_total_suppdata_complete[[attrac]][k]))

    total_obs[k] <- huff_total_suppdata_complete[[location_total]][k]

    if (show_proc == TRUE) cat("Processing location", locations_single[k], "...", "\n")
    
    total_exp1[k] <- huff_total_suppdata_complete$sum_E_j[k]

    diff_rel_old[k] <- (total_exp1[k]-total_obs[k])/total_obs[k]*100


    if (abs(diff_rel_old[k]) > tolerance)
    {
      
      attrac_new[k] <- 0

      huffworkfile[huffworkfile[[locations]] == locations_single[k],][[attrac]] <- attrac_new[k]

      huff_shares_new <- huff.shares(huffworkfile, origins, locations, attrac, dist, lambda = lambda, dtype = dtype, lambda2 = lambda2, check_df = FALSE)

      huff_total_new <- shares.total(huff_shares_new, origins, locations, "p_ij", localmarket, check_df = FALSE)

      total_exp2[k] <- huff_total_new$sum_E_j[k]

      diff_rel_new[k] <- (total_exp2[k]-total_obs[k])/total_obs[k]*100

      b[k] <- (attrac_new[k]-attrac_old[k])/(total_exp2[k]-total_exp1[k])

      a[k] <- b[k] * total_exp2[k] - attrac_new[k]  # just for control

      attrac_new_opt[k] <- a[k] + b[k] * total_obs[k]

    }
    
    else 
    { 
      attrac_new_opt[k] <- as.numeric(as.character(huff_total_suppdata_complete[[attrac]][k]))
    }  
    

    huffworkfile[huffworkfile[[locations]] == locations_single[k],][[attrac]] <- attrac_new_opt[k]

    huff_shares_new <- huff.shares(huffworkfile, origins, locations, attrac, dist, check_df = FALSE)

  }
  
  huff_attrac <- data.frame(huff_total_suppdata_complete$suppliers_single, attrac_new_opt)
  colnames(huff_attrac) <- c("suppliers_single", "attrac_new_opt")

  cat("\n")
  
  if (output == "matrix")
  {
    return(huff_shares_new)  
  }
  
  if (output == "attrac")
  {
    return(huff_attrac)
  }
  
  if (output == "total")
  {
    huff_total_new <- shares.total(huff_shares_new, origins, locations, "p_ij", localmarket, check_df = FALSE)

    huff_total_new$total_obs <- total_obs

    huff_total_new$diff <- huff_total_new$total_obs-huff_total_new$sum_E_j
    
    huff_total_new_output <- merge (huff_total_new, huff_attrac)
    
    return(huff_total_new_output)
  }
  
}
