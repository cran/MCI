ijmatrix.crosstab <-
function (mcidataset, submarkets, suppliers, shares)   

{
  
  mciworkfile <- mcidataset   

  submarkets_single <- levels(as.factor(as.character(mciworkfile[[submarkets]])))

  suppliers_single <- levels(as.factor(as.character(mciworkfile[[suppliers]])))

  submarkets_count <- nlevels(as.factor(as.character(mciworkfile[[submarkets]])))

  suppliers_count <- nlevels(as.factor(as.character(mciworkfile[[suppliers]])))

  i <- 0
  j <- 0

  ij <- matrix(nrow=submarkets_count, ncol=suppliers_count)

  for (i in 1:submarkets_count)
  {
    submarket_i <- mciworkfile[mciworkfile[[submarkets]] == submarkets_single[i],]

    for (j in 1:suppliers_count)
    {
      ij[i,j] <- submarket_i[[shares]][j]
    }
  }
  
  ij_df <- data.frame(ij)
  colnames(ij_df) <- paste0("p_ij_", suppliers_single)
  ij_df$submarkets <- submarkets_single

  return(ij_df)
}
