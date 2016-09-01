shares.segm <-
function (mcidataset, submarkets, suppliers, segmentation, observations, ..., check_df = TRUE)
{
  if (check_df == TRUE)
  {
    if (exists(as.character(substitute(mcidataset)))) { 
      checkdf(mcidataset, submarkets, suppliers, segmentation, observations)
    }
    else {
      stop(paste("Dataset", as.character(substitute(mcidataset))), " not found", call. = FALSE)
    }
  }
  
  mciworkfile <- mcidataset
  
  suppliers_single <- levels(as.factor(as.character(mciworkfile[[suppliers]])))
  suppliers_count <- nlevels(as.factor(as.character(mciworkfile[[suppliers]])))

  ma_breaks <- as.numeric(unlist(list(...)))

  ma_class <- as.character(cut(mciworkfile[[segmentation]], ma_breaks))

  ma_class <- gsub("[:(:]", "", ma_class)
  ma_class <- gsub("[:,:]", "-", ma_class)
  ma_class <- gsub("]", "", ma_class)
  ma_class[is.na(ma_class)] <- "Other"

  mciworkfile$ma_class <- as.data.frame(as.character(ma_class))
  
  ma_class_colname <- paste0(names(mciworkfile[segmentation]), "_class")
  colnames(mciworkfile$ma_class) <- ma_class_colname

  j <- 0

  ma_class_aggregate_df <- as.data.frame(levels(as.factor(ma_class)))
  colnames(ma_class_aggregate_df) <- ma_class_colname

  for (j in 1:suppliers_count)
  {
    supplier_j <- mciworkfile[mciworkfile[[suppliers]] == suppliers_single[j],] 

    ma_class_aggregate <- aggregate(supplier_j[[observations]], by = as.list(supplier_j$ma_class), FUN = sum, na.rm = TRUE)

    ma_class_abs <- ma_class_aggregate[,2]
    ma_class_rel <- ma_class_aggregate[,2]/sum(ma_class_aggregate[,2])*100

    ma_class_aggregate_df <- cbind(ma_class_aggregate_df, ma_class_abs, ma_class_rel)

    colnames(ma_class_aggregate_df)[j+j] <- paste0(suppliers_single[j], "_abs")
    colnames(ma_class_aggregate_df)[j+j+1] <- paste0(suppliers_single[j], "_rel")

  }
  
  return(ma_class_aggregate_df)
}
