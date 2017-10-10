mci.shares.elast <- 
function (mcidataset, submarkets, suppliers, shares, mcivar, mciparam,
                            check_df = TRUE)   
{
  
  if (check_df == TRUE)
  {
    if (exists(as.character(substitute(mcidataset)))) { 
     
      checkdf(mcidataset, submarkets, suppliers, shares, mcivar)
    }
    else {
      stop(paste("Dataset", as.character(substitute(mcidataset))), " not found", call. = FALSE)
    }
  
  }
  
  mciworkfile <- mcidataset
  
  shel_name <- paste0("sh_el_", mcivar)

  mci_elas <- mciparam * (1-mciworkfile[[shares]])
   
  mci_elas <- as.data.frame(mci_elas)
  names (mci_elas) <- shel_name
  mciworkfile <- cbind(mciworkfile, mci_elas)
  
  return (mciworkfile)  
}