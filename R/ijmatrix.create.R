ijmatrix.create <-
function (rawdataset, submarkets, suppliers, ..., remNA = TRUE, 
                             remSing = FALSE, remSing.val = 1) 
  {
  
  if (exists(as.character(substitute(rawdataset)))) { 
    checkdf(rawdataset, submarkets, suppliers, ...)
  }
  else {
    stop(paste("Dataset", as.character(substitute(rawdataset))), " not found", call. = FALSE)
  }

  addvars <- unlist(list(...))
  addvars_count <- length(addvars)

  if (remNA == FALSE)
  {
    mcirawdata <- rawdataset 

    mcirawdata[[submarkets]] <- as.character(mcirawdata[[submarkets]])
    subm_colnr <- which(colnames(mcirawdata)==submarkets)
    mcirawdata[is.na(mcirawdata[[submarkets]]), subm_colnr] <- "99999"
    mcirawdata[[submarkets]] <- as.factor(mcirawdata[[submarkets]])

    mcirawdata[[suppliers]] <- as.character(mcirawdata[[suppliers]])
    supp_colnr <- which(colnames(mcirawdata)==suppliers)
    mcirawdata[is.na(mcirawdata[[suppliers]]), supp_colnr] <- "99999"
    mcirawdata[[suppliers]] <- as.factor(mcirawdata[[suppliers]])

  }
  
  else
  {
    mcirawdata <- rawdataset[((is.na(rawdataset[[submarkets]])) == FALSE) & ((is.na(rawdataset[[suppliers]])) == FALSE) ,]  
  }
  
  
  if (remSing == TRUE)
  {
    freq_subm <- table(mcirawdata[[submarkets]])

    freq_subm_df <- as.data.frame(freq_subm)
    names(freq_subm_df)[1] = names(mcirawdata[submarkets])

    freq_subm_df_adj <- freq_subm_df[freq_subm_df$Freq > remSing.val,1]

    mcirawdata <- mcirawdata[(mcirawdata[[submarkets]] %in% freq_subm_df_adj),]

  }
  
  submarkets_single <- levels(as.factor(as.character(mcirawdata[[submarkets]])))

  suppliers_single <- levels(as.factor(as.character(mcirawdata[[suppliers]])))

  matrix_ij <- merge (submarkets_single, suppliers_single)

  submarkets_colname <- names(mcirawdata[submarkets])
  suppliers_colname <- names(mcirawdata[suppliers])
  
  names(matrix_ij) <- c(submarkets_colname, suppliers_colname)

  matrix_ij$interaction <- paste(matrix_ij[[submarkets_colname]], "-", matrix_ij[[suppliers_colname]], sep="")   

  mcirawdata$interaction <- paste(mcirawdata[[submarkets]], "-", mcirawdata[[suppliers]], sep="")

  interactions <- mcirawdata$interaction

  interactions_count <- as.data.frame(table(interactions))

  names(interactions_count) <- c("interaction", "freq_ij_abs")

  mciworkfile <- merge (matrix_ij, interactions_count, by="interaction", all=TRUE)

  mciworkfile$freq_ij_abs[is.na(mciworkfile$freq_ij_abs)] <- 0

  submarkets_count <- nlevels(as.factor(as.character(mcirawdata[[submarkets]])))

  suppliers_count <- nlevels(as.factor(as.character(mcirawdata[[suppliers]])))

  p_ij_obs <- vector()
  freq_i_abs <- vector()
  submarket_i_total <- 0
  i <- 0
  j <- 0
  
  for(i in 1:submarkets_count)
    {   

    submarket_i <- subset (mciworkfile, mciworkfile[[submarkets_colname]] == submarkets_single[i])  

    submarket_i_total[i] <- sum (submarket_i$freq_ij_abs)

    for(j in 1:suppliers_count) {   

      freq_i_abs <- rbind(freq_i_abs, as.numeric(list(submarket_i_total[i])))

      }   
  }   

  mciworkfile$freq_i_total <- freq_i_abs 

  mciworkfile$p_ij_obs <- mciworkfile$freq_ij_abs/mciworkfile$freq_i_total

  if (addvars_count > 0)

  {
    
    v <- 0

    for (v in 1:addvars_count)
    {
      
      if (checkvar(mcirawdata[[addvars[v]]]) == "valid_n" | checkvar(mcirawdata[[addvars[v]]]) == "invalid_zn")
      {
        
        addvar_p_ij <- paste0("p_ij_obs_", addvars[v])
        addvar_abs_name <- paste0("freq_ij_abs_", addvars[v])
        addvar_total_name <- paste0("freq_i_total_", addvars[v])

        interaction <- vector()
        
        mciaddvardf <- data.frame(matrix(ncol=0, nrow=nrow(mciworkfile)))
        
        p_ij_obs <- vector()
        obs_i_abs <- vector()
        obs_ij_abs <- vector()
        
        submarket_i_total <- 0
        supplier_j_total <- 0
        
        i <- 0
        j <- 0
        ij <- 0
        
        for(i in 1:submarkets_count)
        {   

          submarket_i <- subset (mcirawdata, mcirawdata[[submarkets]] == submarkets_single[i])  

          submarket_i_total[i] <- sum (submarket_i[[addvars[v]]], na.rm = TRUE)

          for (j in 1:suppliers_count) {   

            supplier_j <- subset (submarket_i, submarket_i[[suppliers]] == suppliers_single[j])

            supplier_j_total[j] <- sum (supplier_j[[addvars[v]]], na.rm = TRUE)

            obs_i_abs <- rbind(obs_i_abs, as.numeric(submarket_i_total[i]))

            obs_ij_abs <- rbind(obs_ij_abs, as.numeric(supplier_j_total[j]))

            interaction <- rbind(interaction, paste(as.character(submarkets_single[i]), "-", as.character(suppliers_single[j]), sep=""))

          }  
        }  
        
        mciaddvardf[addvar_abs_name] <- obs_ij_abs
        
        mciaddvardf[addvar_total_name] <- obs_i_abs 
        
        mciaddvardf[addvar_p_ij] <- mciaddvardf[addvar_abs_name]/mciaddvardf[addvar_total_name] 

        mciaddvardf$interaction <- interaction

        mciworkfile <- merge(mciworkfile, mciaddvardf)

      }
      else 
      {
        stop(paste("Variable", addvars[v], "is invalid (contains strings) \n"), call. = FALSE)
      }
    }
  
  }
 
  return(mciworkfile)
}
