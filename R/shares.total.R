shares.total <-
function (mcidataset, submarkets, suppliers, shares, localmarket)

{
  
  if (exists(as.character(substitute(mcidataset)))) { 
    checkdf(mcidataset, submarkets, suppliers, shares, localmarket)
  }
  else {
    stop(paste("Dataset", as.character(substitute(mcidataset))), " not found", call. = FALSE)
  }

  mciworkfile <- mcidataset
  
  mciworkfile[[shares]][is.na(mciworkfile[[shares]])] <- 0
  mciworkfile[[localmarket]][is.na(mciworkfile[[localmarket]])] <- 0

  mciworkfile$E_ij <- mciworkfile[[shares]] * mcidataset[[localmarket]]   

  suppliers_single <- levels(as.factor(as.character(mciworkfile[[suppliers]])))
  suppliers_count <- nlevels(as.factor(as.character(mciworkfile[[suppliers]])))

  sum_E_j <- numeric()

  for (j in 1:suppliers_count) {   
    location_j <- subset (mciworkfile, mciworkfile[[suppliers]] == suppliers_single[j])   
    sum_E_j[j] <- sum(location_j$E_ij, na.rm = TRUE)   
  }
  
  E_j_output <- data.frame(suppliers_single, sum_E_j)   
  E_j_output$share_j <- E_j_output$sum_E_j/sum(E_j_output$sum_E_j, na.rm = TRUE)   

  return(E_j_output)
}
