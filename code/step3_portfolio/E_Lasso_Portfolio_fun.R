eval.pred.fun <- function(data_pred = e_lasso_pred$prediction,
                                    date_col = "Trdmnt",
                                    id_col = "Stkcd",
                                    outcome_col = "value",
                                    actual_col = 'actual',
                                    pred_col = 'pred',
                                    weight_col = "weight",
                                    feature_cols = character(),
                                    indicator_cols = character(),
                                    smoothing_windows = 120L,
                                    year_min = 2005){
  
  require.packages(packages = c('data.table','broom', 'lubridate','sandwich'))
  message('Evaluating prediction.')
  
  pred_df <- data_pred 
  MSFE_monthly <- pred_df |> 
    group_by(.data[[date_col]]) |> 
    summarise(centered_error = mean(.data[[weight_col]] * ((.data[[actual_col]]-mean(.data[[weight_col]] * .data[[actual_col]])-(.data[[pred_col]]-mean(.data[[weight_col]] * .data[[pred_col]]))))^2))
  MSFE_vw <- MSFE_monthly |> 
    ungroup() |> 
    summarise(MSFE_vw = mean(centered_error))
  
  R2_CSOS_monthly <- pred_df |> 
    group_by(.data[[date_col]]) |> 
    summarise(R2_CSOS_monthly = 1-sum(.data[[weight_col]]*(.data[[actual_col]]-mean(.data[[weight_col]]*.data[[actual_col]])-(.data[[pred_col]]-mean(.data[[weight_col]]*.data[[pred_col]])))^2)/sum(.data[[weight_col]]*(.data[[actual_col]]-mean(.data[[weight_col]]*.data[[actual_col]]))^2))
  
  R2_CSOS <- R2_CSOS_monthly |> ungroup() |> 
    summarise(R2_CSOS = mean(R2_CSOS_monthly))
  r2_test <- nw_mean_test(
    x = R2_CSOS_monthly$R2_CSOS_monthly,
    lag = 12,
    alternative = "two.sided"
  ) |> 
    merge(R2_CSOS)
  return(r2_test)
}

nw_mean_test <- function(
    x,
    lag = 12,
    alternative = c("greater", "two.sided", "less")
) {
  
  alternative <- match.arg(alternative)
  
  x <- x[is.finite(x)]
  
  fit <- lm(x ~ 1)
  
  vcov_nw <- sandwich::NeweyWest(
    fit,
    lag = lag,
    prewhite = FALSE,
    adjust = TRUE
  )
  
  estimate <- unname(coef(fit)[1])
  nw_se <- sqrt(vcov_nw[1, 1])
  z_value <- estimate / nw_se
  
  p_value <- switch(
    alternative,
    greater = 1 - pnorm(z_value),
    less = pnorm(z_value),
    two.sided = 2 * pnorm(-abs(z_value))
  )
  
  data.frame(
    estimate = estimate,
    nw_se = nw_se,
    z_value = z_value,
    p_value = p_value,
    n_month = length(x)
  )
}
