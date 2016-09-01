shares.total <-
function (mcidataset, submarkets, suppliers, shares, localmarket,   
                          plotChart = FALSE, plotChart.title = "Total sales", 
                          plotChart.unit = "sales", check_df = TRUE)   

{
  
  if (check_df == TRUE)
  {
    if (exists(as.character(substitute(mcidataset)))) { 
      checkdf(mcidataset, submarkets, suppliers, shares, localmarket)
    }
    else {
      stop(paste("Dataset", as.character(substitute(mcidataset))), " not found", call. = FALSE)
    }
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

  if (plotChart == TRUE) 
  {
    E_j_output_sorted <- E_j_output[order(E_j_output$share_j),]
    max_x <- max(E_j_output_sorted$sum_E_j*1.2)
    barplot(E_j_output_sorted$sum_E_j, names.arg = E_j_output_sorted$suppliers_single, 
            horiz = TRUE, main = plotChart.title, sub = plotChart.unit, 
            xlim = c(0, max_x))
  }
  
  return(E_j_output)
}
