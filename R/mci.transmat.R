mci.transmat <-
function (mcidataset, submarkets, suppliers, mcivariable1, ...) {

  if (exists(as.character(substitute(mcidataset)))) { 
    checkdf(mcidataset, submarkets, suppliers, mcivariable1, ...)
  }
  else {
    stop(paste("Dataset", as.character(substitute(mcidataset))), " not found", call. = FALSE)
  }

  mcidataset_rows <- nrow(mcidataset)

  addmcivars <- unlist(list(...))
  addmcivars_count <- length(addmcivars)

  mcivariablelog1 <- mci.transvar (mcidataset, submarkets, suppliers, mcivariable1, output_ij=TRUE)

  if (addmcivars_count > 0)
  {
  
  v <- 0

  addmcivariablelog <- data.frame(matrix(0, nrow=mcidataset_rows, ncol=addmcivars_count))

  varname <- character()

  for (v in 1:addmcivars_count) {

    varname <- addmcivars[[v]]

    addmcivariablelog[v] <- mci.transvar (mcidataset, submarkets, suppliers, varname, output_ij=FALSE)

    if (checkvar(addmcivariablelog[v]) == "valid_d") 
    {
      colnames(addmcivariablelog)[v] <- varname
    }
    else { 
      colnames(addmcivariablelog)[v] <- paste(varname, "_t", sep="")  
    }
    
  }
  
  mcilinoutput <- data.frame(mcivariablelog1, addmcivariablelog)

  return(mcilinoutput)

  }
  else
  {
    return(mcivariablelog1)
  }
}
