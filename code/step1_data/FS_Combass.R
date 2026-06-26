
# Settings ----------------------------------------------------------------

## Paramters ---------------------------------------------------------------
quarter_lags <- 4

## 文件路径 ----------------------------------------------------------------
FS_Annodt_path <- list.files(data_path['FS_Annodt'], pattern = '\\.xlsx', full.names = T)
FS_BS_path <- list.files(data_path['FS_BS'], pattern = "\\.xlsx", full.names = T)
FS_IS_path <- list.files(data_path['FS_IS'], pattern = '\\.xlsx', full.names = T)
FS_CF_path <- list.files(data_path['FS_CF'], pattern = '\\.xlsx', full.names = T)

## 变量保存 ----------------------------------------------------------------
keep_TRD_vars <- c(
  'Stkcd','Trdmnt','Mclsprc','Mretwd','Mretnd','Msmvosd', 'Msmvttl','Ndaytrd'
  )
keep_Annodt_vars <- c(
  "Stkcd", # [证券代码] - 以沪、深、北证券交易所公布的证券代码为准。
  "Accper", # [统计截止日期] - YYYY-MM-DD，前四位表示会计报表公布年度
  "Annodt" # [报告公布日期] - 以YYYY-MM-DD列示，部分缺少在相应位置上以00表示，如1993年12月某日表示为1993-12-00
)
keep_BS_vars <- c(
  # identifiers
  "Stkcd", # [证券代码] - 以沪、深、北证券交易所公布的证券代码为准。
  "Accper", # [统计截止日期] - YYYY-MM-DD，前四位表示会计报表公布年度
  # "Typrep", # [报表类型] - 指上市公司的财务报表中反映的是合并报表或者母公司报表。“A＝合并报表”、“B＝母公司报表”。
  # "IfCorrect", # [是否发生差错更正] - 会计期间内是否发生差错更正。0：否；1：是
  # "DeclareDate", # [差错更正披露日期] - 差错更正公告的披露日期。若发生多次差错更正，并列展示，用逗号隔开。
  
  # assets
  "A001000000",  # 资产总计
  "A001100000",  # 流动资产合计
  "A001200000",  # 非流动资产合计
  "A001101000",  # 货币资金
  "A001110000",  # 应收票据净额
  "A001111000",  # 应收账款净额
  "A001112000",  # 预付款项净额
  "A001121000",  # 其他应收款净额
  "A001123000",  # 存货净额
  "A001125000",  # 其他流动资产
  "A001205000",  # 长期股权投资净额
  "A001211000",  # 投资性房地产净额
  "A001212000",  # 固定资产净额
  "A001213000",  # 在建工程净额
  "A001218000",  # 无形资产净额
  "A001219000",  # 开发支出
  "A001220000",  # 商誉净额
  "A001221000",  # 长期待摊费用
  "A001222000",  # 递延所得税资产
  "A001223000",  # 其他非流动资产
  
  # liabilities
  "A002000000",  # 负债合计
  "A002100000",  # 流动负债合计
  "A002200000",  # 非流动负债合计
  "A002101000",  # 短期借款
  "A002125000",  # 一年内到期的非流动负债
  "A002201000",  # 长期借款
  "A002203000",  # 应付债券
  "A002107000",  # 应付票据
  "A002108000",  # 应付账款
  "A002109000",  # 预收款项
  "A002112000",  # 应付职工薪酬
  "A002113000",  # 应交税费
  "A002120000",  # 其他应付款
  
  # equity
  "A003000000",  # 所有者权益合计
  "A003100000",  # 归属于母公司所有者权益合计
  "A003101000",  # 实收资本或股本
  "A003102000",  # 资本公积
  "A003103000",  # 盈余公积
  "A003105000",  # 未分配利润
  "A003111000",  # 其他综合收益
  "A003200000"   # 少数股东权益
)
keep_IS_vars <- c(
  # identifiers
  "Stkcd", # [证券代码] - 以沪、深、北证券交易所公布的证券代码为准。
  "Accper", # [统计截止日期] - YYYY-MM-DD，前四位表示会计报表公布年度
  # "Typrep", # [报表类型] - 指上市公司的财务报表中反映的是合并报表或者母公司报表。“A＝合并报表”、“B＝母公司报表”。
  # "IfCorrect", # [是否发生差错更正] - 会计期间内是否发生差错更正。0：否；1：是
  # "DeclareDate", # [差错更正披露日期] - 差错更正公告的披露日期。若发生多次差错更正，并列展示，用逗号隔开。
  
  # revenue and cost
  "B001100000",  # 营业总收入
  "B001101000",  # 营业收入
  "B001200000",  # 营业总成本
  "B001201000",  # 营业成本
  
  # taxes and expenses
  "B001207000",  # 税金及附加
  "B001209000",  # 销售费用
  "B001210000",  # 管理费用
  "B001211000",  # 财务费用
  "B001216000",  # 研发费用，2018年起使用，长期样本慎用
  
  # investment / fair-value / impairment items
  "B001302000",  # 投资收益
  "B001301000",  # 公允价值变动收益
  "B001212000",  # 资产减值损失
  "B001307000",  # 信用减值损失，2019年起使用，长期样本慎用
  "B001308000",  # 资产处置收益，2017年起使用，长期样本慎用
  "B001305000",  # 其他收益，2017年起使用，长期样本慎用
  
  # non-operating items
  "B001400000",  # 营业外收入
  "B001500000",  # 营业外支出
  
  # profits
  "B001300000",  # 营业利润
  "B001000000",  # 利润总额
  "B002100000",  # 所得税费用
  "B002000000",  # 净利润
  "B002000101",  # 归属于母公司所有者的净利润
  "B002000201",  # 少数股东损益
  
  # EPS and comprehensive income
  "B003000000",  # 基本每股收益
  "B004000000",  # 稀释每股收益
  "B005000000",  # 其他综合收益
  "B006000000",  # 综合收益总额
  "B006000101"   # 归属于母公司所有者的综合收益
)
period_IS_vars <- setdiff(
  keep_IS_vars,
  c("Stkcd", "Accper")
)
keep_CF_vars <- c(
  # identifiers
  "Stkcd", # [证券代码] - 以沪、深、北证券交易所公布的证券代码为准。
  "Accper", # [统计截止日期] - YYYY-MM-DD，前四位表示会计报表公布年度
  # "Typrep", # [报表类型] - 指上市公司的财务报表中反映的是合并报表或者母公司报表。“A＝合并报表”、“B＝母公司报表”。
  # "IfCorrect", # [是否发生差错更正] - 会计期间内是否发生差错更正。0：否；1：是
  # "DeclareDate", # [差错更正披露日期] - 差错更正公告的披露日期。若发生多次差错更正，并列展示，用逗号隔开。
  
  # operating cash flow
  "C001001000",  # 销售商品、提供劳务收到的现金
  "C001012000",  # 收到的税费返还
  "C001013000",  # 收到的其他与经营活动有关的现金
  "C001100000",  # 经营活动现金流入小计
  
  "C001014000",  # 购买商品、接受劳务支付的现金
  "C001020000",  # 支付给职工以及为职工支付的现金
  "C001021000",  # 支付的各项税费
  "C001022000",  # 支付其他与经营活动有关的现金
  "C001200000",  # 经营活动现金流出小计
  
  "C001000000",  # 经营活动产生的现金流量净额
  
  # investing cash flow
  "C002001000",  # 收回投资收到的现金
  "C002002000",  # 取得投资收益收到的现金
  "C002003000",  # 处置固定资产、无形资产和其他长期资产收回的现金净额
  "C002004000",  # 处置子公司及其他营业单位收到的现金净额
  "C002100000",  # 投资活动现金流入小计
  
  "C002006000",  # 购建固定资产、无形资产和其他长期资产支付的现金
  "C002007000",  # 投资支付的现金
  "C002009000",  # 取得子公司及其他营业单位支付的现金净额
  "C002200000",  # 投资活动现金流出小计
  
  "C002000000",  # 投资活动产生的现金流量净额
  
  # financing cash flow
  "C003008000",  # 吸收投资收到的现金
  "C003003000",  # 发行债券收到的现金
  "C003002000",  # 取得借款收到的现金
  "C003100000",  # 筹资活动现金流入小计
  
  "C003005000",  # 偿还债务支付的现金
  "C003006000",  # 分配股利、利润或偿付利息支付的现金
  "C003200000",  # 筹资活动现金流出小计
  
  "C003000000",  # 筹资活动产生的现金流量净额
  
  # cash balance changes
  "C004000000",  # 汇率变动对现金及现金等价物的影响
  "C005000000",  # 现金及现金等价物净增加额
  "C005001000",  # 期初现金及现金等价物余额
  "C006000000"   # 期末现金及现金等价物余额
)
period_CF_vars <- setdiff(
  keep_CF_vars,
  c("Stkcd", "Accper", "C005001000", "C006000000")
)

## 股票代码 ----------------------------------------------------------------
stock_code <- tools::file_path_sans_ext(
  list.files(dirname(files["data_hub_stock_TRD"]), 
             pattern = "\\.xlsx", 
             full.names = F)
  )

# Load Data ---------------------------------------------------------------

## 年报中报季报公布时间 --------------------------------------------------------------
FS_Annodt_data <- lapply(
  FS_Annodt_path,
  function(path1) {
    read_excel(path1) %>%
      slice(-c(1:2))
  }
) %>%
  rbindlist(use.names = F) %>%
  select(any_of(keep_Annodt_vars)) %>%
  mutate(Accper = ymd(Accper), Annodt = ymd(Annodt))

## Parallel ---------------------------------------------------------------
library(future.apply)
library(furrr)
plan(multisession, workers = availableCores() - 1)
set.seed(0)

## 资产负债表 -------------------------------------------------------------------
FS_BS_data <- future_lapply(
  FS_BS_path,
  function(path1) {
    read_excel(path1) %>% 
      slice(-c(1:2)) %>% 
      filter(Typrep == "A") %>% 
      select(any_of(keep_BS_vars)) 
  }
) %>%
  rbindlist(use.names = F) %>% 
  mutate(
    across(
      !any_of(c("Stkcd", "Accper")),
      as.numeric
    )
  ) %>% 
  mutate(Accper = ymd(Accper)) 
  # filter(lubridate::month(Accper) %in% c(3, 6, 9, 12))

## 利润表 ---------------------------------------------------------------------
FS_IS_data <- future_lapply(
  FS_IS_path,
  function(path1) {
    read_excel(path1) %>% 
      slice(-c(1:2)) %>% 
      filter(Typrep == "A") %>% 
      select(any_of(keep_IS_vars)) 
  }
) %>%
  rbindlist(use.names = F) %>% 
  mutate(
    across(
      !any_of(c("Stkcd", "Accper")),
      as.numeric
    )
  ) %>% 
  mutate(Accper = ymd(Accper)) 
  # filter(lubridate::month(Accper) %in% c(3, 6, 9, 12))

## 现金流量表 -------------------------------------------------------------------
FS_CF_data <- future_lapply(
  FS_CF_path,
  function(path1) {
    read_excel(path1) %>% 
      slice(-c(1:2)) %>% 
      filter(Typrep == "A") %>% 
      select(any_of(keep_CF_vars)) 
  }
) %>%
  rbindlist(use.names = F) %>% 
  mutate(
    across(
      !any_of(c("Stkcd", "Accper")),
      as.numeric
    )
  ) %>% 
  mutate(Accper = ymd(Accper)) 
  # filter(lubridate::month(Accper) %in% c(3, 6, 9, 12))


# Output Files ------------------------------------------------------------

invisible(
  future_lapply(
    stock_code,
    function(code1) {

## 资产负债表 -------------------------------------------------------------------
      bs_data <- FS_BS_data %>% 
        left_join(FS_Annodt_data, by = c('Stkcd', 'Accper')) %>% 
        mutate(Trdmnt = if_else(
          !is.na(Annodt), 
          floor_date(Annodt),
          floor_date(Accper %m+% months(quarter_lags), "month")
          ),
          Trdmnt = floor_date(as.Date(Trdmnt), "month")
          ) %>% 
        filter(Stkcd == code1) %>%
        arrange(Stkcd, Trdmnt, Annodt) %>%   # 先按 Annodt 从早到晚排
        distinct(Stkcd, Trdmnt, .keep_all = TRUE, .fromLast = TRUE) %>% 
        select(-c(Annodt, Accper, .fromLast)) %>% 
        select(Stkcd, Trdmnt, everything())

# 利润表 ---------------------------------------------------------------------
      is_data <- FS_IS_data %>% 
        left_join(FS_Annodt_data, by = c("Stkcd", "Accper")) %>% 
        mutate(
          Trdmnt = if_else(
            !is.na(Annodt), 
            floor_date(Annodt, "month"),
            floor_date(Accper %m+% months(quarter_lags), "month")
          ),
          Trdmnt = floor_date(as.Date(Trdmnt), "month")
        ) %>% 
        
        # 同一个 Stkcd + Trdmnt 如果重复，保留最新报表期
        filter(Stkcd == code1) %>% 
        mutate(
          fiscal_year = year(Accper),
          fiscal_quarter = quarter(Accper),
          q_index = fiscal_year * 4 + fiscal_quarter
        ) %>% 
        arrange(Accper) %>% 
        mutate(
          lag_q_index = lag(q_index),
          has_prev_quarter = q_index - lag_q_index == 1
        ) %>% 
        mutate(
          across(
            all_of(period_IS_vars),
            ~ case_when(
              fiscal_quarter == 1 ~ .x,
              has_prev_quarter ~ .x - lag(.x),
              TRUE ~ NA_real_
            ),
            .names = "{.col}_q"
          )
        ) %>% 
        mutate(
          has_4_consecutive_quarters =
            q_index - lag(q_index, 1) == 1 &
            q_index - lag(q_index, 2) == 2 &
            q_index - lag(q_index, 3) == 3
        ) %>% 
        mutate(
          across(
            ends_with("_q"),
            ~ if_else(
              has_4_consecutive_quarters &
                !is.na(.x) &
                !is.na(lag(.x, 1)) &
                !is.na(lag(.x, 2)) &
                !is.na(lag(.x, 3)),
              .x + lag(.x, 1) + lag(.x, 2) + lag(.x, 3),
              NA_real_
            ),
            .names = "{.col}_ttm"
          )
        ) %>% 
        arrange(Stkcd, Trdmnt, desc(Accper), desc(Annodt)) %>% 
        distinct(Stkcd, Trdmnt, .keep_all = TRUE, .fromLast = TRUE) %>% 
        select(Stkcd, Trdmnt, ends_with('ttm'))

## 现金流量表 -------------------------------------------------------------------
      
      cf_data <- FS_CF_data %>% 
        select(any_of(keep_CF_vars)) %>% 
        left_join(FS_Annodt_data, by = c("Stkcd", "Accper")) %>% 
        # filter(Stkcd == code1) %>% 
        mutate(
          Trdmnt = if_else(
            !is.na(Annodt), 
            floor_date(Annodt, "month"),
            floor_date(Accper %m+% months(quarter_lags), "month")
          ),
          Trdmnt = floor_date(as.Date(Trdmnt), "month")
        ) %>% 
        
        # 同一个 Stkcd + Trdmnt 如果重复，保留最新报表期
        filter(Stkcd == code1) %>% 
        mutate(
          fiscal_year = year(Accper),
          fiscal_quarter = quarter(Accper),
          quarter_id = fiscal_year * 4 + fiscal_quarter
        ) %>% 
        arrange(Accper) %>% 
        mutate(
          lag_quarter_id = lag(quarter_id),
          has_prev_quarter = quarter_id - lag_quarter_id == 1
        ) %>% 
        mutate(
          across(
            all_of(period_CF_vars),
            ~ case_when(
              fiscal_quarter == 1 ~ .x,
              has_prev_quarter ~ .x - lag(.x),
              TRUE ~ NA_real_
            ),
            .names = "{.col}_q"
          )
        ) %>% 
        mutate(
          has_4_consecutive_quarters =
            quarter_id - lag(quarter_id, 1) == 1 &
            quarter_id - lag(quarter_id, 2) == 2 &
            quarter_id - lag(quarter_id, 3) == 3
        ) %>% 
        mutate(
          across(
            ends_with("_q"),
            ~ if_else(
              has_4_consecutive_quarters &
                !is.na(.x) &
                !is.na(lag(.x, 1)) &
                !is.na(lag(.x, 2)) &
                !is.na(lag(.x, 3)),
              .x + lag(.x, 1) + lag(.x, 2) + lag(.x, 3),
              NA_real_
            ),
            .names = "{.col}_ttm"
          )
        ) %>% 
        arrange(Stkcd, Trdmnt, desc(Accper), desc(Annodt)) %>% 
        distinct(Stkcd, Trdmnt, .keep_all = TRUE) %>% 
        select(Stkcd, Trdmnt, setdiff(keep_CF_vars, period_CF_vars), ends_with('ttm'))

# 合并数据 --------------------------------------------------------------------

      df1 <- read_excel(file.path(dirname(files['data_hub_stock_TRD']), paste0(code1, '.xlsx'))) %>% 
        select(any_of(keep_TRD_vars)) %>% 
        # mutate(Trdmnt = ym(Trdmnt)) %>% 
        arrange(Trdmnt) %>% 
        left_join(bs_data, by = c('Stkcd', 'Trdmnt')) %>% 
        left_join(is_data, by = c('Stkcd', 'Trdmnt')) %>% 
        left_join(cf_data, by = c('Stkcd', 'Trdmnt')) %>% 
        rename_with(~ str_remove(.x, "_q_ttm$")) %>% 
        mutate(
          across(
            where(is.numeric),
            ~ if_else(.x == 0, NA_real_, .x)
          )
        ) %>% 
        mutate(
          across(
            where(is.numeric),
            ~ {
              original_na <- is.na(.x)
              grp <- cumsum(!original_na)
              gap <- ave(original_na, grp, FUN = cumsum)
              
              out <- data.table::nafill(.x, type = "locf")
              out[original_na & gap >= 6] <- NA_real_
              
              out
            }
          )
        )
      
      file_path1 <- file.path(dirname(files["data_hub_stock_TRD"]), paste0(code1, ".xlsx"))
      write_xlsx(df1, file_path1)
      
    }
  )
)


# Clean Memory ------------------------------------------------------------

# file.edit(unname(files['clean_memory']))
# source(unname(files['clean_memory']))

