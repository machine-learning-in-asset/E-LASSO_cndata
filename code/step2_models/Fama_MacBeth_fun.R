smoothed.WLS.pred <- function(data = dataset$panel,
                              date_col = "Trdmnt",
                              id_col = "Stkcd",
                              outcome_col = "value",
                              weight_col = "weight",
                              feature_cols = character(),
                              indicator_cols = character(),
                              smoothing_windows = 120L,
                              year_min = 2005L) {
  
  require.packages(packages = c('data.table','broom', 'lubridate'))
  
  out <- data.table::copy(data)
  data.table::setDT(out)
  
  required_cols <- c(
    date_col,
    id_col,
    outcome_col,
    weight_col,
    "Msmvosd_lag1",
    "Msmvttl_lag1",
    "A003000000_lag1",
    "Mretwd_lag1"
  )
  
  missing_cols <- setdiff(required_cols, names(out))
  
  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  numeric_cols <- c(
    outcome_col,
    weight_col,
    "Msmvosd_lag1",
    "Msmvttl_lag1",
    "A003000000_lag1",
    "Mretwd_lag1"
  )
  
  feature_cols <- c('size','bm','mom12m_raw')
  
  out[
    ,
    (numeric_cols) := lapply(
      .SD,
      function(x) suppressWarnings(as.numeric(x))
    ),
    .SDcols = numeric_cols
  ]
  
  data.table::setorderv(
    out,
    c(id_col, date_col)
  )
  
  out[
    ,
    A003000000_lag1 := data.table::nafill(
      A003000000_lag1,
      type = "locf"
    ),
    by = c(id_col)
  ]
  
  out[
    ,
    size := data.table::fifelse(
      is.finite(Msmvosd_lag1) & Msmvosd_lag1 > 0,
      log(Msmvosd_lag1),
      NA_real_
    )
  ]
  
  out[
    ,
    bm := data.table::fifelse(
      is.finite(A003000000_lag1) &
        is.finite(Msmvttl_lag1) &
        Msmvttl_lag1 > 0,
      A003000000_lag1 / Msmvttl_lag1,
      NA_real_
    )
  ]
  
  out[
    ,
    log_bm := data.table::fifelse(
      is.finite(bm) & bm > 0,
      log(bm),
      NA_real_
    )
  ]
  
  out[
    ,
    mom12m_raw := data.table::frollapply(
      1 + Mretwd_lag1,
      n = 12L,
      function(x) {
        if (all(is.finite(x))) {
          prod(x) - 1
        } else {
          NA_real_
        }
      },
      align = "right",
      fill = NA_real_
    ),
    by = c(id_col)
  ]
  out <- out[is.finite(mom12m_raw)]
 
  
  out[
    ,
    (feature_cols) := lapply(
      .SD,
      function(x) suppressWarnings(as.numeric(x))
    ),
    .SDcols = feature_cols
  ]
  
  out <- out[
    as.integer(format(get(date_col), "%Y")) >= year_min
  ]
  
  model_cols <- unique(
    c(
      outcome_col,
      weight_col,
      feature_cols
    )
  )
  
  coef_terms <- c(
    "(Intercept)",
    feature_cols
  )
  
  fm_formula <- reformulate( c('size', 'bm', 'mom12m_raw'), response = outcome_col)
  out_selected <- out[, c(date_col, id_col, model_cols), with = F]
  setDT(out_selected)
  fm_coef <- out_selected[
    ,
    {
      fit <- lm(
        formula = fm_formula,
        data = .SD,
        weights = .SD[[weight_col]]
      )
      as.data.table(broom::tidy(fit))
    },
    by = c(date_col)
  ]
  setDT(fm_coef) 
  setorderv(fm_coef, c("term", date_col))
  
  fm_coef[, estimate_ma_lag1 := shift(frollmean(estimate, n = smoothing_windows, align = "right", fill = NA_real_, na.rm = TRUE), n = 1L), by = term]
 fm_coef <- fm_coef[, .SD[seq_len(.N) > smoothing_windows], by = term]
  
  data.table::setorderv(
    fm_coef,
    c(date_col, "term")
  )
  
  valid_dates <- fm_coef |> 
    select(any_of(date_col)) |> 
    pull()
  
  valid_dates <- sort(unique(valid_dates))
  
  pred_cols <- unique(
    c(
      date_col,
      id_col,
      outcome_col,
      weight_col
    )
  )
  
  pred <- out[
    get(date_col) %in% valid_dates,
    ..pred_cols
  ]
  
  data.table::setorderv(
    pred,
    c(date_col, id_col)
  )
  
  
  pred_result <- lapply(valid_dates,function(current_date){
    coef_filter <- fm_coef[get(date_col) == current_date, c('term', 'estimate'),with = F]
    out_filter <- out_selected[get(date_col) == current_date]
    
    out_filter[
      ,
      pred := coef_filter[term == "(Intercept)", estimate] +
        coef_filter[term == "bm", estimate] * bm +
        coef_filter[term == "mom12m_raw", estimate] * mom12m_raw +
        coef_filter[term == "size", estimate] * size
    ]
    
    out_filter
    
  }) |> 
    rbindlist(use.names = F) |> 
    select(any_of(id_col), any_of(date_col), any_of(outcome_col), any_of(weight_col), pred) |> 
    rename(actual = value)
  
  list(prediction = pred_result, 
       formula = fm_formula, 
       id_col = id_col,
       date_col = date_col,
       value_col = 'actual',
       pred_col = 'pred',
       weight_col = "weight")
}