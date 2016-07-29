model.fit <-
function (y_obs, y_exp)
{
  n <- length(y_obs)

  sum_y_obs <- sum(y_obs)

  resids <- y_obs-y_exp
  resids_sq <- resids^2
  resids_abs <- abs(resids)

  resids_abs_sum <- sum(resids_abs)
  resids_sq_sum <- sum(resids_sq)

  pseudorsq <- (var(y_obs)-var(resids))/var(y_obs)

  mape <- mean(resids_abs/y_obs)

  globerr <- resids_abs_sum/sum_y_obs

  return (list(resids_sq_sum=resids_sq_sum, pseudorsq=pseudorsq, globerr=globerr, mape=mape))
}
