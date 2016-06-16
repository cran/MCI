shares.total <-
function (mcidataset, submarkets, suppliers, shares, localmarket)
{
  mciworkfile <- mcidataset
  mciworkfile$C_i <- mcidataset[[localmarket]]   
  mciworkfile$E_ij <- mciworkfile$p_ij*mciworkfile$C_i   
  suppliers_single <- levels(as.factor(mciworkfile[[suppliers]]))
  suppliers_count <- nlevels(as.factor(mciworkfile[[suppliers]]))
  sum_E_j <- numeric()

  for (j in 1:suppliers_count) {   
    location_j <- subset (mciworkfile, mciworkfile[[suppliers]] == suppliers_single[j])   
    sum_E_j[j] <- sum(location_j$E_ij)   
  }
  
  E_j_output <- data.frame(suppliers_single, sum_E_j)   
  E_j_output$share_j <- E_j_output$sum_E_j/sum(E_j_output$sum_E_j)   

  return(E_j_output)
}
