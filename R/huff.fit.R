huff.fit <-
function (huffdataset, origins, locations, attrac, dist, 
                      lambda = -2, dtype= "pow", lambda2 = NULL, 
                      localmarket_dataset, origin_id, localmarket, 
                      location_dataset, location_id, location_total, 
                      tolerance = 5, iterations = 3, output = "total",
                      show_proc = FALSE, check_df = TRUE)
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
      checkdf(localmarket_dataset, origin_id, localmarket)
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
  
  
  i <- 0
  
  locations_count <- nrow(location_dataset)
  
  model_diag_df <- data.frame()
  
  huffworkfile <- huffdataset
  
  for (i in 1:iterations)
  {
    if (show_proc == TRUE) { cat ("Iteration", i, "of", iterations, "...", "\n")}
    
    huffworkfile_total <- huff.attrac(huffworkfile, origins, locations, attrac, dist, 
                                      lambda = lambda, dtype = dtype, lambda2 = lambda2, 
                                      localmarket_dataset, origin_id, localmarket, 
                                      location_dataset, location_id, location_total, 
                                      tolerance = tolerance,  output = "total", show_proc = show_proc,
                                      check_df = FALSE)
    
    
    
    locations_single <- huffworkfile_total$suppliers_single

    model_diag <- model.fit(huffworkfile_total$total_obs, huffworkfile_total$sum_E_j)
    model_diag_df <- rbind(model_diag_df, as.data.frame(model_diag))

    huffworkfile_total_attrac_new <- data.frame (huffworkfile_total$suppliers_single, huffworkfile_total$attrac_new_opt)
    colnames(huffworkfile_total_attrac_new) <- c("suppliers_single", "attrac_new_opt")

    huffworkfile <- merge (huffworkfile, huffworkfile_total_attrac_new, by.x = locations, by.y = "suppliers_single")

    huffworkfile[[attrac]] <- huffworkfile$attrac_new_opt

    huffworkfile$total_obs <- NULL
    huffworkfile$diff <- NULL
    huffworkfile$attrac_new_opt <- NULL

  }
  
  if (output == "matrix")
  {
    huffworkfile_shares_final <- huff.shares(huffworkfile, origins, locations, attrac, dist, lambda = lambda, dtype = dtype, lambda2 = lambda2, check_df = FALSE)

    return (huffworkfile_shares_final)
  }
  
  if (output == "total")
  {
    return (huffworkfile_total)
  }
  
  if (output == "diag")
  {
    iterations_count <- 1:iterations
    huffworkfile_total_diag <- data.frame(iterations_count, model_diag_df)
    return(huffworkfile_total_diag)
  }
  
}
