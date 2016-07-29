checkvar <-
function (checkvar) 
{
  x <- checkvar[!is.na(checkvar)]
  
  if (class(x) == "numeric" | class(x) == "integer")
  {
    if (all(x>0) == FALSE) 
    { 
      if (all(x %in% c(1,0)))
      { return("valid_d") }
      else
      { return ("invalid_zn") }
    }
    else { return("valid_n") }
  }
  else
  { return ("invalid_s")  }
  
}
