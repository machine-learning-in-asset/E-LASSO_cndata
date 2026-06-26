
# Input Data --------------------------------------------------------------

read.one.stock.trd.file <- function(path,
                                    id_col = "Stkcd",
                                    date_col = "Trdmnt",
                                    return_col = "Mretwd",
                                    weight_source_col = "Msmvosd") {
  require.packages(c("readxl", "lubridate", "data.table"))
  
  df <- as.data.frame(readxl::read_excel(path))
  if (!id_col %in% names(df)) {
    df[[id_col]] <- tools::file_path_sans_ext(basename(path))
  }
  
  df[[id_col]] <- normalize.stock.code(df[[id_col]])
  df[[date_col]] <- parse.trading.month(df[[date_col]])
  df <- coerce.model.numeric.columns(df, exclude_cols = c(id_col, date_col))
  df <- df[order(df[[id_col]], df[[date_col]]), , drop = FALSE]
  
  # For the project's z_t -> r_{t+1} panel, each row keeps characteristics
  # observed at month t. The realized next-month return is joined from
  # data_hub/stocky below, following the return-data source in
  # code/step2_models/Fama_MacBeth.R.
  #
  # The market-cap weight is the market capitalization known at month t.
  # It is named `Msmvosd_lag1` to match the existing Fama-MacBeth weighting
  # convention and downstream E-LASSO function defaults.
  # df$Msmvosd_lag1 <- df[[weight_source_col]]
  
  setDT(df)
  
  lag_cols <- setdiff(names(df), c(id_col, date_col))
  
  # 按股票代码和日期排序
  setorderv(df, c(id_col, date_col))
  
  # 分组滞后，并生成带有 _lag1 后缀的新变量
  df[, paste0(lag_cols, "_lag1") :=
       shift(.SD, n = 1L, type = "lag"),
     by = id_col,
     .SDcols = lag_cols]
  
  df <- df[, c(
    id_col,
    date_col,
    paste0(lag_cols, "_lag1")
  ), with = FALSE]
  
  df
}

require.packages <- function(packages) {
  missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop(
      "Please install required packages before running E-LASSO: ",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

dir.here <- function(...) {
  if (!requireNamespace("here", quietly = TRUE)) {
    stop("Package `here` is required for project paths. Please install it first.", call. = FALSE)
  }
  here::here(...)
}

read.one.stocky.return.file <- function(path,
                                        id_col = "Stkcd",
                                        date_col = "Trdmnt",
                                        return_col = "Mretwd") {
  require.packages(c("readxl", "lubridate", 'data.table'))
  
  df <- as.data.frame(readxl::read_excel(path))
  if (!id_col %in% names(df)) {
    df[[id_col]] <- tools::file_path_sans_ext(basename(path))
  }
  
  df[[id_col]] <- normalize.stock.code(df[[id_col]])
  df[[date_col]] <- parse.trading.month(df[[date_col]])
  data.table::setnames(df, old = return_col, new = "value")
  df <- df[order(df[[id_col]], df[[date_col]]), c(id_col, date_col, 'value'), drop = FALSE]
  df[c(id_col, date_col, "value")]
}

normalize.stock.code <- function(x) {
  x_chr <- as.character(x)
  x_chr <- trimws(x_chr)
  numeric_like <- grepl("^[0-9]+$", x_chr)
  x_chr[numeric_like] <- sprintf("%06d", as.integer(x_chr[numeric_like]))
  x_chr
}

parse.trading.month <- function(x) {
  if (inherits(x, "Date")) {
    return(as.Date(format(x, "%Y-%m-01")))
  }
  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    return(as.Date(format(as.Date(x), "%Y-%m-01")))
  }
  if (is.numeric(x)) {
    parsed <- as.Date(x, origin = "1899-12-30")
    return(as.Date(format(parsed, "%Y-%m-01")))
  }
  
  x_chr <- trimws(as.character(x))
  parsed <- suppressWarnings(lubridate::ym(x_chr))
  need_ymd <- is.na(parsed)
  if (any(need_ymd)) {
    parsed[need_ymd] <- suppressWarnings(lubridate::ymd(x_chr[need_ymd]))
  }
  as.Date(format(parsed, "%Y-%m-01"))
}

coerce.model.numeric.columns <- function(data, exclude_cols) {
  out <- data
  candidate_cols <- setdiff(names(out), exclude_cols)
  for (col_i in candidate_cols) {
    if (inherits(out[[col_i]], c("Date", "POSIXct", "POSIXlt"))) {
      next
    }
    if (is.logical(out[[col_i]])) {
      out[[col_i]] <- as.numeric(out[[col_i]])
      next
    }
    if (!is.numeric(out[[col_i]])) {
      converted <- suppressWarnings(as.numeric(out[[col_i]]))
      if (sum(is.finite(converted), na.rm = TRUE) > 0) {
        out[[col_i]] <- converted
      }
    }
  }
  out
}

load.stocky.next.returns <- function(stocky_dir = dir.here("data_hub", "stocky"),
                                     id_col = "Stkcd",
                                     date_col = "Trdmnt",
                                     return_col = "Mretwd",
                                     max_files = Inf) {
  require.packages(c("readxl", "data.table", "lubridate", "here"))
  
  stocky_files <- list.files(stocky_dir, pattern = "\\.xlsx$", full.names = TRUE)
  if (is.finite(max_files)) {
    stocky_files <- utils::head(stocky_files, max_files)
  }
  
  returns <- lapply(
    stocky_files,
    read.one.stocky.return.file,
    id_col = id_col,
    date_col = date_col,
    return_col = return_col
  )
  returns <- data.table::rbindlist(returns, use.names = TRUE, fill = TRUE)
  as.data.frame(returns)
}


preprocess.monthly.characteristics <- function(data,
                                               date_col = "Trdmnt",
                                               id_col = 'Stkcd',
                                               outcome_col = 'value',
                                               weight_col = 'weight',
                                               indicator_cols = character(),
                                               winsor_probs = c(0.01, 0.99)) {
  # Paper implementation notes:
  # - Winsorize each characteristic each month at the 1st and 99th
  #   cross-sectional percentiles.
  # - Fill missing values monthly using the cross-sectional mean; for
  #   indicator variables, use the monthly median.
  # - Standardize each characteristic each month by cross-sectional mean
  #   and standard deviation.
  # Source: PDF page 13, Section 4.1 Data.
  require.packages(packages = c('data.table'))
  out <- data
  setDT(out)
  feature_cols <- setdiff(colnames(out),c(date_col,id_col,outcome_col,weight_col,indicator_cols))
  log_rows <- list()
  months <- unique(out[[date_col]])
  
  for (month_i in months) {
    row_id <- which(out[[date_col]] == month_i)
    
    for (feature_i in feature_cols) {
      x <- out[[feature_i]][row_id]
      finite <- is.finite(x)
      non_missing_before <- sum(finite)
      
      if (non_missing_before == 0) {
        out[[feature_i]][row_id] <- NA_real_
        log_rows[[length(log_rows) + 1]] <- data.frame(
          Trdmnt = month_i,
          feature_name = feature_i,
          n_obs = length(x),
          n_non_missing_before = non_missing_before,
          impute_value = NA_real_,
          mean_after_impute = NA_real_,
          sd_after_impute = NA_real_,
          status = "all_missing",
          stringsAsFactors = FALSE
        )
        next
      }
      
      limits <- as.numeric(stats::quantile(
        x[finite],
        probs = winsor_probs,
        na.rm = TRUE,
        names = FALSE,
        type = 7
      ))
      x[finite] <- pmin(pmax(x[finite], limits[1]), limits[2])
      
      impute_value <- if (feature_i %in% indicator_cols) {
        stats::median(x[is.finite(x)], na.rm = TRUE)
      } else {
        mean(x[is.finite(x)], na.rm = TRUE)
      }
      
      x[!is.finite(x)] <- impute_value
      mean_x <- mean(x)
      sd_x <- stats::sd(x)
      
      if (!is.finite(sd_x) || sd_x == 0) {
        x[] <- 0
        status <- "zero_variance_after_impute"
      } else {
        x <- (x - mean_x) / sd_x
        status <- "ok"
      }
      
      out[[feature_i]][row_id] <- x
      log_rows[[length(log_rows) + 1]] <- data.frame(
        Trdmnt = month_i,
        feature_name = feature_i,
        n_obs = length(x),
        n_non_missing_before = non_missing_before,
        impute_value = impute_value,
        mean_after_impute = mean(out[[feature_i]][row_id], na.rm = TRUE),
        sd_after_impute = stats::sd(out[[feature_i]][row_id], na.rm = TRUE),
        status = status,
        stringsAsFactors = FALSE
      )
    }
  }
  
  attr(out, "preprocess_log") <- do.call(rbind, log_rows)
  out
}


load.actual.panel <- function(stock_trd_dir = dir.here("data_hub", "stock_TRD"),
                              stocky_dir = dir.here("data_hub", "stocky"),
                              id_col = "Stkcd",
                              date_col = "Trdmnt",
                              return_col = "Mretwd",
                              weight_source_col = "Msmvosd_lag1",
                              max_files = 100,
                              extra_exclude_cols = character(),
                              na_deal = 'zero' # c('zero','med')
                              ) {
  require.packages(c("readxl", "data.table", "lubridate", "here"))
  
  stock_files <- list.files(stock_trd_dir, pattern = "\\.xlsx$", full.names = TRUE)
  if (is.finite(max_files)) {
    stock_files <- utils::head(stock_files, max_files)
  }
  
  message("Reading ", length(stock_files), " stock_TRD files from: ", stock_trd_dir, '\n', 'And perform a one-order lag operation on features.')
  
  panel <- lapply(
    stock_files,
    read.one.stock.trd.file,
    id_col = id_col,
    date_col = date_col,
    return_col = return_col,
    weight_source_col = weight_source_col
  )
  panel <- data.table::rbindlist(panel, use.names = TRUE, fill = TRUE)
  panel <- as.data.frame(panel)
  
  message("Reading returns from: ", stocky_dir)
  returns <- load.stocky.next.returns(
    stocky_dir = stocky_dir,
    id_col = id_col,
    date_col = date_col,
    return_col = return_col,
    max_files = max_files
  )
  
  panel <- merge(
    returns,
    panel,
    by = c(id_col, date_col),
    all.x = FALSE,
    all.y = FALSE,
    sort = FALSE
  )
  
  panel <- panel[!is.na(panel[[date_col]]), , drop = FALSE]
  panel <- panel[is.finite(panel$value), , drop = FALSE]
  panel <- panel[is.finite(panel$Msmvosd_lag1) & panel$Msmvosd_lag1 > 0, , drop = FALSE]
  
  excluded_cols <- unique(c(
    id_col,
    date_col,
    "value",
    "Msmvosd_lag1",
    extra_exclude_cols
  ))
  feature_cols <- names(panel)[vapply(panel, is.numeric, logical(1))]
  feature_cols <- setdiff(feature_cols, excluded_cols)
  feature_cols <- feature_cols[vapply(panel[feature_cols], function(x) {
    any(is.finite(x))
  }, logical(1))]
  
  setDT(panel)
  date_cols <- names(panel)[
    vapply(
      panel,
      function(x) inherits(x, c("Date", "POSIXct", "POSIXlt", "IDate")),
      logical(1)
    )
  ]
  panel[, (setdiff(date_cols, date_col)) := NULL]
  panel[, weight := get(weight_source_col)]
  data.table::setorderv(panel,c(date_col, id_col))
  panel[
    ,
    weight := weight / sum(weight, na.rm = TRUE) * .N,
    by = c(date_col)
  ]
  # panel <- preprocess.monthly.characteristics(data = panel)
  setDT(panel)
  if(na_deal == 'med'){
    panel[
      ,
      (feature_cols) := lapply(
        .SD,
        function(x) {
          med <- median(x, na.rm = TRUE)
  
          if (!is.finite(med)) {
            med <- 0
          }
  
          fifelse(is.na(x), med, x)
        }
      ),
      by = c(date_col),
      .SDcols = feature_cols
    ]
  }
  if(na_deal == 'zero'){
    data.table::setnafill(
      panel,
      type = "const",
      fill = 0,
      cols = feature_cols
    )
  }
  list(
    panel = panel,
    feature_cols = feature_cols,
    source_files = stock_files,
    id_col = id_col,
    date_col = date_col,
    outcome_col = "value",
    weight_col = "weight"
  )
}

