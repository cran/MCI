huff.shares <-
function (huffdataset, origins, locations, attrac, dist,    
                         gamma = 1, lambda = -2, atype= "pow", dtype= "pow", 
                         gamma2 = NULL, lambda2 = NULL, check_df = TRUE)
{   
  
  if (check_df == TRUE)
  {
    if (exists(as.character(substitute(huffdataset)))) { 
      checkdf(huffdataset, origins, locations, attrac, dist)
    }
    else {
      stop(paste("Dataset", as.character(substitute(huffdataset))), " not found", call. = FALSE)
    }

  }
  
  sort_i_j <- order(huffdataset[[origins]], huffdataset[[locations]])   

  huffworkfile <- huffdataset[sort_i_j,]   

  origins_single <- levels(as.factor(as.character(huffworkfile[[origins]])))

  origins_count <- nlevels(as.factor(as.character(huffworkfile[[origins]])))

  locations_single <- levels(as.factor(as.character(huffworkfile[[locations]])))

  locations_count <- nlevels(as.factor(as.character(huffworkfile[[locations]])))

  
  if (atype=="pow") { attrac_w <- huffworkfile[[attrac]]^gamma }
  if (atype=="exp") { attrac_w <- exp(gamma*huffworkfile[[attrac]]) }
  if (atype=="logistic") { attrac_w <- (max(huffworkfile[[attrac]]))/(1+exp(gamma2+gamma*huffworkfile[[attrac]])) }
  
  if (dtype=="pow") { dist_w <- huffworkfile[[dist]]^lambda } 
  if (dtype=="exp") { dist_w <- exp(lambda*huffworkfile[[dist]]) }
  if (dtype=="logistic") { dist_w <- (max(huffworkfile[[dist]]))/(1+exp(lambda2+lambda*huffworkfile[[dist]])) }
  
  U_ij <- attrac_w * dist_w 

  huffworkfile$U_ij <- U_ij

  sum_U_ij <- vector()
  sum_U_ij_all <- vector()
  
  for (i in 1:origins_count) {   

    origin_i <- subset (huffworkfile, huffworkfile[[origins]] == origins_single[i])   

    sum_U_ij_i <- sum(origin_i$U_ij)   

    for (j in 1:locations_count) {   

      sum_U_ij_all <- rbind(sum_U_ij_all, list(sum_U_ij_i)) 
    }
  }
  
  huffworkfile$sum_U_ij <- sum_U_ij_all   

  huffworkfile$p_ij <- as.numeric(huffworkfile$U_ij)/as.numeric(huffworkfile$sum_U_ij)   

  return(huffworkfile)   

}
