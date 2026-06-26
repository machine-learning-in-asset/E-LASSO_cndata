
# Settings ----------------------------------------------------------------

## 文件路径 ----------------------------------------------------------------
TRD_data_path <- list.files(data_path["stock_TRD"], pattern = "\\.xlsx", full.names = T)
TRD_Co_data_path <- list.files(data_path['TRD_Co'], pattern = '\\.xlsx', full.names = T)

## 变量保存 ----------------------------------------------------------------
keep_TRD_vars <- c(
  'Stkcd', # [证券代码] - 以上交所、深交所公布的证券代码为准
  'Trdmnt', # [交易月份] - 以YYYY-MM表示
  'Mclsprc', # [月收盘价] - 月最后一个交易日的收盘价。
  'Mretwd', # [考虑现金红利再投资的月个股回报率] - 字段说明见说明书“周、月、年个股回报率的计算方法”。
  'Mretnd', # [不考虑现金红利再投资的月个股回报率] - 字段说明见说明书“周、月、年个股回报率的计算方法”。
  'Msmvosd', # [月个股流通市值] - 个股的流通股数与月收盘价的乘积。计算公式为：个股的流通股数与月收盘价的乘积。 A股以人民币元计，上海B以美元计，深圳B以港币计，注意单位是千
  'Msmvttl', # [月个股总市值] - 个股的发行总股数与月收盘价的乘积。计算公式为：个股的发行总股数与月收盘价的乘积，A股以人民币元计，上海B股以美元计，深圳B股以港币计，注意单位是千
  'Ndaytrd' # [月交易天数] - 计算公式为：月内实际交易的天数之和。
)
keep_TRD_Co_vars <- c(
  'Stkcd','Nnindcd'
)

# Load Data ---------------------------------------------------------------

## 行情数据 -------------------------------------------------------------------
TRD_data <- lapply(
  TRD_data_path,
  function(path1) {
    read_excel(path1) %>%
      slice(-c(1:2)) %>%
      mutate(
        across(
          !any_of(c("Stkcd", "Trdmnt", "Opndt", "Clsdt", "Capchgdt")),
          as.numeric
        )
      ) %>%
      filter(Markettype %in% c(1, 4) & Ndaytrd >= 10) %>%
      # Markettype, # [市场类型] - 1=上证A股市场 (不包含科创板），2=上证B股市场，4=深证A股市场（不包含创业板），8=深证B股市场，16=创业板， 32=科创板，64=北证A股市场。
      select(any_of(keep_TRD_vars))
  }
) %>%
  rbindlist(use.names = F) %>% 
  group_by(Stkcd) %>% 
  mutate(Trdmnt = ym(Trdmnt)) %>% 
  # filter(max(year(Trdmnt)) - min(year(Trdmnt)) >= 10) %>% 
  # filter(max(year(Trdmnt)) >= 2024) %>% 
  ungroup()

## 行业分类 --------------------------------------------------------------------
TRD_Co_data <- lapply(
  TRD_Co_data_path,
  function(path1) {
    read_excel(path1) %>%
      slice(-c(1:2)) %>%
      select(any_of(keep_TRD_Co_vars))
  }
) %>%
  rbindlist(use.names = F) |> 
  filter(!str_starts(Nnindcd, "J"))


## 股票代码 --------------------------------------------------------------------
stock_code <- intersect(
  unique(TRD_Co_data$Stkcd),
  unique(TRD_data$Stkcd)
)


## 清空文件夹 -------------------------------------------------------------------
unlink(list.files(dirname(files["data_hub_stocky"]), full.names = TRUE), recursive = TRUE)
unlink(list.files(dirname(files["data_hub_stock_TRD"]), full.names = TRUE), recursive = TRUE)

# Output Files ------------------------------------------------------------
library(future.apply)
library(furrr)
plan(multisession, workers = availableCores() - 1)
set.seed(0)


## Output stocky -----------------------------------------------------------

invisible(
  future_lapply(
    stock_code,
    function(code1) {
      df1 <- TRD_data %>%
        filter(Stkcd == code1) %>% 
        select(Stkcd, Trdmnt, Mretwd)
      file_path1 <- file.path(dirname(files["data_hub_stocky"]), paste0(code1, ".xlsx"))
      write_xlsx(df1, file_path1)
    }
  )
)

## Output stockx ------------------------------------------------------------
invisible(
  future_lapply(
    stock_code,
    function(code1) {
      df1 <- TRD_data %>%
        filter(Stkcd == code1)
      file_path1 <- file.path(dirname(files["data_hub_stock_TRD"]), paste0(code1, ".xlsx"))
      write_xlsx(df1, file_path1)
    }
  )
)


# Clean Memory ------------------------------------------------------------

# file.edit(unname(files['clean_memory']))
# source(unname(files['clean_memory']))
