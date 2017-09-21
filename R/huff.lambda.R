huff.lambda <-
function (huffdataset, origins, locations, attrac, dist,
                         gamma = 1, atype = "pow", gamma2 = NULL,
                         lambda_startv = -1, lambda_endv = -3, dtype= "pow",  
                         localmarket_dataset, origin_id, localmarket, 
                         location_dataset, location_id, location_total,
                         method = "bisection", iterations = 10, output = "matrix", 
                         plotVal = FALSE, show_proc = FALSE, check_df = TRUE)

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
  
  lambda2 <- 0
  
  huffworkfile <- merge (huffdataset, localmarket_dataset, by.x = origins, by.y = origin_id)

  i <- 0
  
  if (method == "compare")
  {
    
    lambda_startv <- round(lambda_startv, digits = 2) 
    lambda_endv <- round(lambda_endv, digits = 2) 

    lambda_seq <- seq(lambda_startv, lambda_endv, -0.01)   

    iterations <- length(lambda_seq)

    residsq_lambda <- vector()
    
    for (i in 1:iterations)
    {
      if (show_proc == TRUE) { cat ("Iteration", i, "of", iterations, "...", "\n")}
      

      huff_shares <- huff.shares(huffworkfile, origins, locations, attrac, dist, gamma = gamma, lambda = lambda_seq[i], atype = atype, dtype = dtype, lambda2 = lambda2, check_df = FALSE)
      
      huff_total <- shares.total(huff_shares, origins, locations, "p_ij", localmarket, check_df = FALSE)
      

      huff_compare <- data.frame(huff_total$suppliers_single, huff_total$sum_E_j)
    
      colnames(huff_compare) <- c("suppliers_single", "exp_lambda")
      
      huff_compare_suppdata <- merge (huff_compare, location_dataset, by.x="suppliers_single", by.y=location_id)

      residsq_lambda[i] <- sum((huff_compare_suppdata$exp_lambda-huff_compare_suppdata[[location_total]])^2)
    
      lambda_values <- lambda_seq
    }
    
    if (plotVal == TRUE)
    {
      plot (lambda_values, residsq_lambda, "l", xlab = "Lambda value", ylab = "Sum of squared residuals")
    }
  
    
  }
  
  else
  {
    intervall_m <- vector()
    residsq_lambda_m <- vector()
    
    for (i in 1:iterations)
    {
      if (show_proc == TRUE) { cat ("Iteration", i, "of", iterations, "...", "\n")}
      
      intervall_m[i] <- (lambda_startv+lambda_endv)/2
      

      huff_shares_start <- huff.shares(huffworkfile, origins, locations, attrac, dist, gamma = gamma, lambda = lambda_startv, atype = atype, dtype = dtype, lambda2 = lambda2, check_df = FALSE)
      
      huff_shares_m <- huff.shares(huffworkfile, origins, locations, attrac, dist, gamma = gamma, lambda = intervall_m[i], atype = atype, dtype = dtype, lambda2 = lambda2, check_df = FALSE)
      
      huff_shares_end <- huff.shares(huffworkfile, origins, locations, attrac, dist, gamma = gamma, lambda = lambda_endv, atype = atype, dtype = dtype, lambda2 = lambda2, check_df = FALSE)
      
      

      huff_total_start <- shares.total(huff_shares_start, origins, locations, "p_ij", localmarket, check_df = FALSE)
      
      huff_total_m <- shares.total(huff_shares_m, origins, locations, "p_ij", localmarket, check_df = FALSE)
      
      huff_total_end <- shares.total(huff_shares_end, origins, locations, "p_ij", localmarket, check_df = FALSE)
      
      

      huff_compare <- data.frame(huff_total_start$suppliers_single, huff_total_start$sum_E_j, huff_total_m$sum_E_j, huff_total_end$sum_E_j)
   
      colnames(huff_compare) <- c("suppliers_single", "exp_start", "exp_m", "exp_end")
      
      huff_compare_suppdata <- merge (huff_compare, location_dataset, by.x="suppliers_single", by.y=location_id)

      
      residsq_lambda_start <- sum((huff_compare_suppdata$exp_start-huff_compare_suppdata[[location_total]])^2)
    
      residsq_lambda_m[i] <- sum((huff_compare_suppdata$exp_m-huff_compare_suppdata[[location_total]])^2)

      residsq_lambda_end <- sum((huff_compare_suppdata$exp_end-huff_compare_suppdata[[location_total]])^2)

      if (residsq_lambda_start < residsq_lambda_end)
      {
        lambda_startv <- lambda_startv
        lambda_endv <- intervall_m[i]
      }
      else
      {
        lambda_startv <- intervall_m[i]
        lambda_endv <- lambda_endv
      }
      
    }
    lambda <- intervall_m[i]
    
    lambda_values <- intervall_m
    
    if (plotVal == TRUE)
    {
      plot (lambda_values, residsq_lambda_m, "l", xlab = "Lambda value", ylab = "Sum of squared residuals")
    }
    
  }
  
  cat("\n")
  
  if (output == "iterations")
  {
    iterations_count <- 1:iterations
    iterations_results <- data.frame(iterations_count, lambda_values)
    colnames(iterations_results) <- c("Iteration", "Lambda")
    return(iterations_results)
  }
  
  if (output == "total")
  {
    huff_shares <- huff.shares(huffworkfile, origins, locations, attrac, dist, gamma = gamma, lambda = lambda, atype = atype, dtype = dtype, lambda2 = lambda2, check_df = FALSE)
    huff_total <- shares.total(huff_shares, origins, locations, "p_ij", localmarket, check_df = FALSE)
    return(huff_total)
  }
  
  else
  { return(list(gamma=gamma, lambda=lambda)) }
  
}
