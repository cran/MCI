mci.shares <-
function (mcidataset, submarkets, suppliers, ..., mcitrans = "lc", interc = NULL) 
{   

  if (!exists(as.character(substitute(mcidataset)))) 
  {
    stop(paste("Dataset", as.character(substitute(mcidataset))), " not found", call. = FALSE)
  }
  
  mcivarsparams <- unlist(list(...))
  mcivars <- vector()
  mciparams <- vector()
  mcivarsweigths <- data.frame()
  
  attracfunction <- NULL
  
  mcivarsparams_length <- length(mcivarsparams)-2
  
  i <- 0
  for (i in 1:mcivarsparams_length)
  {
    mcivars[i] <- mcivarsparams[i+i-1]
    mciparams[i] <- mcivarsparams[i+i]
  }
  
  checkdf(mcidataset, submarkets, suppliers, mcivars[which(is.na(mcivars)==FALSE)])

  mcivarsweigths <- cbind.data.frame(mcivars, mciparams)

  mcivarsweigths <- mcivarsweigths[which(is.na(mcivarsweigths$mcivars)==FALSE),]

  sort_i_j <- order(mcidataset[[submarkets]], mcidataset[[suppliers]])   

  mciworkfile <- mcidataset[sort_i_j,]   

  submarkets_single <- levels(as.factor(as.character(mciworkfile[[submarkets]])))

  submarkets_count <- nlevels(as.factor(as.character(mciworkfile[[submarkets]])))

  suppliers_single <- levels(as.factor(as.character(mciworkfile[[suppliers]])))

  suppliers_count <- nlevels(as.factor(as.character(mciworkfile[[suppliers]])))

  i <- 0

  ij_combinations <- submarkets_count*suppliers_count

  attrac_w <- matrix(nrow = ij_combinations, ncol = (nrow(mcivarsweigths)+1))

  if (mcitrans == "ilc")
  {
   
    for (i in 1:nrow(mcivarsweigths))
    {

      mcivar_name <- mcivarsweigths[i,1]
      mcivar_name_c <- as.character(mcivar_name)
      mcivar_data <- mciworkfile[[mcivar_name_c]]
      mcivar_data_t <- mci.transvar (mciworkfile, submarkets, suppliers, mcivar_name_c, check_df = FALSE) 

      mcivarparam <- as.numeric(as.character(mcivarsweigths[i,2]))

      attrac_w[,i] <- mcivarparam*mcivar_data_t

    }
    
    i <- 0

    for (i in 1:nrow(attrac_w)) {
      if (is.null(interc) == FALSE) { 
        attrac_w[i,ncol(attrac_w)] <- exp(interc+sum(attrac_w[i,1:(ncol(attrac_w)-1)]))
      }
      else {
      attrac_w[i,ncol(attrac_w)] <- exp(sum(attrac_w[i,1:(ncol(attrac_w)-1)]))
      }
    } 
  }
  
  else
  {  

  for (i in 1:nrow(mcivarsweigths))
  {

    mcivar_name <- mcivarsweigths[i,1]
    mcivar_name_c <- as.character(mcivar_name)
    mcivar_data <- mciworkfile[[mcivar_name_c]]

    mcivarparam <- as.numeric(as.character(mcivarsweigths[i,2]))

    attrac_w[,i] <- mcivar_data^mcivarparam

  }
  
  i <- 0

  for (i in 1:nrow(attrac_w)) {
    attrac_w[i,ncol(attrac_w)] <- prod(attrac_w[i,1:(ncol(attrac_w)-1)])
  }
  }
  
  mciworkfile$U_ij <- as.numeric(attrac_w[,ncol(attrac_w)])

  sum_U_ij <- vector()
  sum_U_ij_all <- vector()
  
  for (i in 1:submarkets_count) {   

    origin_i <- subset (mciworkfile, mciworkfile[[submarkets]] == submarkets_single[i])   

    sum_U_ij_i <- sum(origin_i$U_ij)   

    for (j in 1:suppliers_count) {   

      sum_U_ij_all <- rbind(sum_U_ij_all, list(sum_U_ij_i)) 
    }
  }
  
  mciworkfile$sum_U_ij <- sum_U_ij_all   

  mciworkfile$p_ij <- as.numeric(mciworkfile$U_ij)/as.numeric(mciworkfile$sum_U_ij)   

  return(mciworkfile)   

}
