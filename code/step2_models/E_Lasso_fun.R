
# Step0 Prepare Function --------------------------------------------------

lasso.model.fit <- function(x_mat1 = x_mat, y_vec1 = y_vec, w_vec1 = w_vec, nlambda1 = 100, intercept1 = T, standardize1 = F){
  lasso_fit <- glmnet::glmnet(
    x = x_mat1,
    y = y_vec1,
    weights = w_vec1,
    family = "gaussian",
    alpha = 1,
    intercept = intercept1,
    standardize = standardize1,
    nlambda = nlambda1,
    lambda.min.ratio = 1e-4
  )
  return(lasso_fit)
}

select.lambda.caic <- function(fit, x_mat1 = x_mat, y_vec1 = y_vec, w_vec1 = w_vec) {
  pred_mat <- as.matrix(stats::predict(fit, newx = x_mat1, s = fit$lambda))
  
  n_obs <- length(y_vec1)
  rss <- colSums((y_vec1 - pred_mat)^2 * w_vec1)
  df_model <- fit$df + 1L
  
  # The paper selects lambda_t by corrected AIC for the LASSO objective
  # in equation (5): PDF page 6, Section 2.2.1. The PDF does not print
  # the expanded AICc formula, so we use the standard Hurvich-Tsai
  # correction: n*log(RSS/n) + 2*k + 2*k*(k+1)/(n-k-1). [page.5 equation (4)]
  # uncorrection is n*log(RSS/n) + 2*k. [page.4 equation (3)].
  corrected_aic <- n_obs * log(pmax(rss / n_obs, .Machine$double.eps)) +
    2 * df_model +
    (2 * df_model * (df_model + 1)) / pmax(n_obs - df_model - 1, 1)
  corrected_aic[n_obs <= df_model + 1] <- Inf
  
  if (all(!is.finite(corrected_aic))) {
    return(list(
      lambda = NA_real_,
      corrected_aic = NA_real_,
      df_lasso = NA_integer_,
      path = data.frame(
        lambda = fit$lambda,
        corrected_aic = corrected_aic,
        df_lasso = df_model
      ),
      status = "no_finite_aicc"
    ))
  }
  
  best_id <- which.min(corrected_aic)
  list(
    lambda = fit$lambda[best_id],
    corrected_aic = corrected_aic[best_id],
    df_lasso = df_model[best_id],
    path = data.frame(
      lambda = fit$lambda,
      corrected_aic = corrected_aic,
      df_lasso = df_model
    ),
    status = "ok"
  )
}

# Step1 S-LASSO -----------------------------------------------------------------

smoothing.lasso.pred <- function(data = dataset$panel,
                                 date_col = "Trdmnt",
                                 id_col = "Stkcd",
                                 outcome_col = "value",
                                 weight_col = "weight",
                                 feature_cols = character(),
                                 indicator_cols = character(),
                                 smoothing_windows = 120L,
                                 year_min = 1995L) {
  
  
  require.packages(packages = c('data.table','broom', 'lubridate'))
  
  # out <- data.table::copy(data)
  out <- preprocess.monthly.characteristics(data = data)
  data.table::setDT(out)
  
  if(length(feature_cols) == 0){
    feature_cols <- setdiff(names(out),c(id_col, date_col, outcome_col, weight_col, indicator_cols))
  }
  
  out <- out[
    as.integer(format(get(date_col), "%Y")) >= year_min
  ]
  
  model_formula <- reformulate(
    c(feature_cols, indicator_cols),
    response = outcome_col
  )
  
  
  out[
    ,
    `:=`(
      weight =
        get(weight_col) /
        sum(get(weight_col), na.rm = TRUE) *
        .N
    ),
    by = date_col
  ]
  train_dates <- sort(unique(as.Date(out[[date_col]])))
  message('Calculate coefficients. ')
  model_coef <- lapply(train_dates, function(current_date){
    out_filter <- out[get(date_col) == current_date]
    setDT(out_filter)
    w_vec <- out_filter[[weight_col]]
    y_vec <- out_filter[[outcome_col]]
    x_mat <- as.matrix(out_filter[,c(feature_cols, indicator_cols),with = F])
    
    model_fit <- lasso.model.fit(x_mat1 = x_mat, y_vec1 = y_vec, w_vec1 = w_vec)
    lambda_choice <- select.lambda.caic(fit = model_fit,x_mat1 = x_mat, y_vec1 = y_vec, w_vec1 = w_vec)
    coef_best <- as.matrix(stats::coef(model_fit, s = lambda_choice$lambda))
    coef_best <- data.table::data.table(
      term = rownames(coef_best),
      estimate = as.numeric(coef_best[, 1])
    )
    return(coef_best)
  }) |> 
    setNames(as.character(train_dates)) |> 
    rbindlist(use.names = T, fill = T, idcol = date_col)
  
  setDT(model_coef)
  setorderv(model_coef, c("term", date_col))
  model_coef[, estimate_ma_lag1 := shift(frollmean(estimate, n = smoothing_windows, align = "right", fill = NA_real_, na.rm = TRUE), n = 1L), by = term]
  
  data.table::setorderv(
    model_coef,
    c(date_col, "term")
  )
  
  model_coef <- model_coef[is.finite(estimate_ma_lag1)]
  valid_dates <- sort(unique(as.Date(model_coef[[date_col]])))
  
  fix_cols <- out[,c(id_col, date_col, outcome_col, weight_col), with = F]
  setDT(fix_cols)
  # setnames(fix_cols, old = outcome_col, new = 'actual')
  
  message('Calculate S-Lasso prediction. ')
  pred <- lapply(valid_dates, function(current_date){
    panel_filter <- out[get(date_col) == current_date, c(id_col,feature_cols, indicator_cols), with = F]
    panel_filter <- panel_filter |> 
      mutate(`(Intercept)` = as.numeric(1)) |> 
      pivot_longer(cols = where(is.numeric), names_to = 'term', values_to = 'feature_value')
    coef_filter <- model_coef[get(date_col) == current_date, c('term','estimate_ma_lag1'), with = F]
    panel_filter <- panel_filter |> 
      left_join(coef_filter, by = 'term') |> 
      group_by(.data[[id_col]]) |> 
      summarise(pred = sum(feature_value*estimate_ma_lag1, na.rm = T)) |> 
      mutate(
        !!date_col := as.Date(current_date)
      )
  }) |> 
    setNames(valid_dates) |> 
    rbindlist(use.names = F) |> 
    left_join(fix_cols, by = c(id_col, date_col)) |> 
    select(any_of(id_col), any_of(date_col), any_of(outcome_col), any_of(weight_col), pred) |> 
    rename(actual = value)
  
  list(prediction = pred, 
       formula = model_formula, 
       model_coef = model_coef,
       id_col = id_col,
       date_col = date_col,
       value_col = 'actual',
       pred_col = 'pred',
       weight_col = "weight",
       func = 'smoothing.lasso.pred')
}


# Step2 C-LASSO -----------------------------------------------------------

combination.lasso.pred <- function(data = dataset$panel,
                                   date_col = "Trdmnt",
                                   id_col = "Stkcd",
                                   outcome_col = "value",
                                   weight_col = "weight",
                                   feature_cols = character(),
                                   indicator_cols = character(),
                                   smoothing_windows = 120L,
                                   year_min = 2005){
  require.packages(packages = c('data.table','broom', 'lubridate','future.apply'))
  
  # out <- data.table::copy(data)
  out <- preprocess.monthly.characteristics(data = data)
  data.table::setDT(out)
  
  if(length(feature_cols) == 0){
    feature_cols <- setdiff(names(out),c(id_col, date_col, outcome_col, weight_col, indicator_cols))
  }
  data.table::setorderv(out, c(id_col, date_col))
  out <- out |> 
    mutate(across(all_of(indicator_cols), ~as.numeric(.x)))
  out[
    ,
    (c(feature_cols, indicator_cols)) := lapply(.SD, data.table::nafill, type = "locf"),
    by = id_col,
    .SDcols = c(feature_cols, indicator_cols)
  ]
  
  data.table::setnafill(
    out,
    type = "const",
    fill = 0,
    cols = c(feature_cols,indicator_cols)
  )
  out <- out |> 
    mutate(across(all_of(indicator_cols), ~as.factor(.x)))
  out <- out[
    as.integer(format(get(date_col), "%Y")) >= year_min
  ]
  
  fix_cols <- out[,c(id_col, date_col, outcome_col, weight_col), with = F]
  train_dates <- sort(unique(as.Date(out[[date_col]])))
  
  library(future.apply)
  plan(multicore, workers = availableCores()-1)
  message("Number of parallel workers: ", nbrOfWorkers())
  model_coef_pred <- lapply(train_dates[-length(train_dates)], function(current_date){
    out_filter <- out[get(date_col) == current_date]
    count_id <- which(train_dates == current_date)
    next_date <- train_dates[count_id + 1L]
    estimate_filter <- out[get(date_col) == next_date]
    
    model_coef <- future_lapply(feature_cols, function(feature){
      formula_set <- reformulate(feature, response = outcome_col)
      fit <- lm(
        formula = formula_set,
        data = out_filter,
        weights = out_filter[[weight_col]]
      )
      fit <- as.data.table(broom::tidy(fit))
      pred_characteristics <- estimate_filter |> 
        select(any_of(id_col), any_of(date_col), any_of(feature)) |> 
        mutate(`(Intercept)` = as.numeric(1)) |> 
        pivot_longer(cols = where(is.numeric), names_to = 'term', values_to = 'feature_value') |> 
        left_join(fit |> select('term', 'estimate'), by = 'term') |> 
        group_by(.data[[id_col]]) |> 
        summarise(return_pred = sum(feature_value*estimate, na.rm = T))
      
      return(list(pred_characteristics = pred_characteristics,
                  fit = fit,
                  formula = formula_set))
    }) |> 
      setNames(feature_cols)
    message('Estimated pred_characteristics: ', current_date,'\t')
    return(model_coef)
  }) |> 
    setNames(train_dates[-1])
  plan(sequential)
  
  pred_combine <- lapply(model_coef_pred, function(data_list){
    lapply(data_list, function(df) {
      df[['pred_characteristics']] |> 
        as_tibble() 
    })|> 
      rbindlist(use.names = T, fill = T, idcol = 'feature')
  }) |> 
    rbindlist(use.names = T, fill = T, idcol = date_col) |> 
    pivot_wider(id_cols = all_of(c(id_col, date_col)), names_from = feature, values_from = return_pred) |> 
    mutate(!!date_col := as.Date(.data[[date_col]])) |> 
    left_join(fix_cols, by = c(id_col, date_col))
  setDT(pred_combine)
  
  valid_dates <- train_dates[-1]
  
  pred_list <- lapply(valid_dates[-length(valid_dates)], function(current_date){
    message('Estimated combination pred: ', current_date,'\t')
    pred_combine_filter <- pred_combine[get(date_col) == current_date]
    count_id <- which(valid_dates == current_date)
    next_date <- valid_dates[count_id + 1L]
    estimate_filter <- pred_combine[get(date_col) == next_date]
    
    w_vec <- pred_combine_filter[[weight_col]]
    y_vec <- pred_combine_filter[[outcome_col]]
    x_mat <- as.matrix(pred_combine_filter[,c(feature_cols),with = F])
    model_fit <- lasso.model.fit(x_mat1 = x_mat, y_vec1 = y_vec, w_vec1 = w_vec,intercept1 = T, standardize1 = T)
    lambda_choice <- select.lambda.caic(fit = model_fit,x_mat1 = x_mat, y_vec1 = y_vec, w_vec1 = w_vec)
    coef_best <- as.matrix(stats::coef(model_fit, s = lambda_choice$lambda))
    coef_best <- data.table::data.table(
      term = rownames(coef_best),
      estimate = as.numeric(coef_best[, 1])
    ) 
    setDT(coef_best)
    coef_best <- coef_best |> 
      filter(estimate != 0) |> 
      filter(term != "(Intercept)")
    pred_combine_filter_longer <- estimate_filter |> 
      select(-any_of(indicator_cols)) |> 
      pivot_longer(cols = -all_of(c(id_col, date_col, weight_col)), names_to = 'term', values_to = 'univariate_pred')
    combine_pred <- coef_best |> 
      left_join(pred_combine_filter_longer, by = 'term') |> 
      group_by(dplyr::across(dplyr::all_of(id_col))) |> 
      mutate(
        pred = mean(univariate_pred, na.rm = T)) |> 
      select(any_of(c(id_col, date_col, weight_col)), pred) |> 
      distinct() |> 
      left_join(select(out, any_of(c(id_col, date_col, outcome_col))), by = c(id_col, date_col)) |> 
      select(any_of(id_col), any_of(outcome_col), any_of(weight_col), pred) 
    if(nrow(combine_pred) == 0){
      combine_pred <- estimate_filter
      combine_pred[
        ,
        pred := rowMeans(
          .SD,
          na.rm = TRUE
        ),
        .SDcols = setdiff(
          names(combine_pred),
          c(id_col, date_col)
        )
      ]
      combine_pred <- combine_pred[
        ,
        c(id_col, date_col, "pred"),
        with = FALSE
      ]
      combine_pred <- combine_pred |> 
        left_join(select(out, any_of(c(id_col, date_col, outcome_col, weight_col))), by = c(id_col, date_col)) |> 
        select(any_of(id_col), any_of(outcome_col), any_of(weight_col), pred) 
      
    }
    
    return(list(pred = combine_pred, coef_best = coef_best))
  }) |> 
    setNames(valid_dates[-1]) 
  
  pred <- pred_list |> 
    lapply(function(list1){
      list1[['pred']]
    }) |> 
    rbindlist(use.names = T, fill = T, idcol = date_col) |> 
    select(any_of(id_col), any_of(date_col), any_of(outcome_col), any_of(weight_col), pred) |> 
    rename(actual = all_of(outcome_col)) |> 
    arrange(.data[[id_col]],
            .data[[date_col]]) |> 
    group_by(.data[[id_col]]) |> 
    fill(.direction = 'down') |> 
    ungroup()
  
  coef_best <- pred_list |> 
    lapply(function(list1){
      list1[['coef_best']]
    }) |> 
    rbindlist(use.names = T, fill = T, idcol = date_col)
  
  return(list(prediction = pred, 
              coef_best = coef_best,
              id_col = id_col,
              date_col = date_col,
              value_col = 'actual',
              pred_col = 'pred',
              weight_col = "weight",
              func = 'combination.lasso.pred'))
  
}


# Step3 E-LASSO -----------------------------------------------------------

encompassing.lasso.pred <- function(data = dataset$panel,
                                    s_lasso_pred_df = s_lasso_pred$prediction,
                                    c_lasso_pred_df = c_lasso_pred$prediction,
                                    date_col = "Trdmnt",
                                    id_col = "Stkcd",
                                    outcome_col = "value",
                                    weight_col = "weight",
                                    feature_cols = character(),
                                    indicator_cols = character(),
                                    smoothing_windows = 120L,
                                    year_min = 2005){
  
  # Page.8 equation(15) formulation is equal to "(r_{i,t} - \hat{r}^{s}_{t}) ~ \eta_{t} + \theta_{t} * (\hat{r}^c_t - \hat{r}^{s}_{t}) "
  require.packages(packages = c('data.table','broom', 'lubridate'))
  message('Processing E-LASSO.')
  s_lasso_pred_df <- s_lasso_pred_df |> rename(s_lasso_pred = pred) |> 
    mutate(!!date_col := as.Date(.data[[date_col]]))
  c_lasso_pred_df <- c_lasso_pred_df |> rename(c_lasso_pred = pred) |> 
    select(any_of(c(id_col, date_col)), c_lasso_pred) |> 
    mutate(!!date_col := as.Date(.data[[date_col]]))
  lasso_df <- s_lasso_pred_df |> 
    left_join(c_lasso_pred_df, by = c(id_col, date_col)) |> 
    mutate(y_hat = actual - s_lasso_pred, x_hat = c_lasso_pred - s_lasso_pred) |> 
    drop_na()
  
  setDT(lasso_df)
  valid_dates <- sort(unique(as.Date(lasso_df[[date_col]])))
  formula_set <- reformulate('x_hat', response = 'y_hat')
  
  theta_df <- lasso_df[
    ,
    {
      fit <- lm(
        formula = formula_set,
        data = .SD,
        weights = .SD[[weight_col]]
      )
      as.data.table(broom::tidy(fit))
    },
    by = c(date_col)
  ]
  
  setorderv(theta_df, c("term", date_col))
  theta_df[, mean_theta := shift(frollmean(estimate, n = smoothing_windows, align = "right", fill = NA_real_, na.rm = TRUE), n = 1L), by = term]
  
  data.table::setorderv(
    theta_df,
    c(date_col, "term")
  )
  
  theta_df <- theta_df[is.finite(mean_theta)]
  valid_dates <- sort(unique(as.Date(theta_df[[date_col]])))
  
  pred <- lapply(valid_dates, function(current_date){
    lasso_df_filter <- lasso_df[get(date_col) == current_date]
    theta_df_filter <- theta_df[get(date_col) == current_date, c("term", "mean_theta")]
    
    lasso_df_filter[
      ,
      pred := (1-theta_df_filter[term == "x_hat", mean_theta]) * s_lasso_pred +
        theta_df_filter[term == "x_hat", mean_theta] * c_lasso_pred
    ]
    e_lasso_pred <- lasso_df_filter[,-c('x_hat','y_hat'), with = F]
    return(e_lasso_pred)
  }) |> 
    setNames(valid_dates) |> 
    rbindlist(use.names = F) |> 
    drop_na()
  message('Finish E-LASSO')
  
  return(list(prediction = pred, 
              formula = formula_set, 
              model_coef = theta_df,
              id_col = id_col,
              date_col = date_col,
              value_col = 'actual',
              pred_col = 'pred',
              weight_col = "weight",
              func = 'encompassing.lasso.pred'))
}
