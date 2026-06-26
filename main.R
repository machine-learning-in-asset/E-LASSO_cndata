rm(list = ls())

files <- c(
  packages = file.path("code", "step0_packages", "packages.R"),
  clean_memory = file.path("code", "step1_data", "clean_memory.R"), 
  stock_TRD = file.path("code", "step1_data", "stock_TRD.R"),
  data_hub_stock_TRD = file.path('data_hub','stock_TRD', 'NA'), # 月度股票回报率
  data_hub_stocky = file.path("data_hub", "stocky", 'NA'),
  FS_Combass = file.path("code", "step1_data", "FS_Combass.R"), # 三表
  Fama_MacBeth_fun = file.path("code", "step2_models", "Fama_MacBeth_fun.R"),
  E_Lasso_fun = file.path("code", "step2_models", "E_Lasso_fun.R"),
  E_Lasso_Portfolio_fun = file.path('code','step3_portfolio','E_Lasso_Portfolio_fun.R'),
  ManagerShareSalary = file.path("code", "step1_data", "ManagerShareSalary.R"),
  Load_Data = file.path('code','step1_data','Load_Data_fun.R')
)

data_path <- c(
  stock_TRD = file.path("data/csmar/月个股回报率文件163304330(仅供江西财经大学使用)"),
  
  FS_Annodt = file.path('data/csmar/年、中、季报基本情况文件162159569(仅供江西财经大学使用)'), 
  FS_BS = file.path('data/csmar/资产负债表104925553(仅供江西财经大学使用)'),
  FS_IS = file.path('data/csmar/利润表104042653(仅供江西财经大学使用)'), 
  FS_CF = file.path("data/csmar/现金流量表(直接法)150957382(仅供江西财经大学使用)"), 
  TRD_Co = file.path("data/csmar/公司文件100738595(仅供江西财经大学使用)"), 
  data_hub_stock_TRD = file.path('data_hub','stock_TRD'), # 月度股票回报率
  data_hub_stocky = file.path("data_hub", "stocky"),
  ManagerShareSalary = file.path("data/csmar/高管人数、持股及薪酬情况表144146746(仅供江西财经大学使用)")
)
  
create_file <- function(path) {
  if (basename(path) == "NA") {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  } else {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    
    if (!file.exists(path)) {
      file.create(path)
    }
  }
}

invisible(lapply(files, create_file))

# Load Packages -----------------------------------------------------------

source(unname(files['packages']))

# Clean Data --------------------------------------------------------------

# file.edit(unname(files['stock_TRD']))
# file.edit(unname(files['FS_Combass']))
# 
# file.edit(unname(files['ManagerShareSalary']))


# Load Data ---------------------------------------------------------------

# file.edit(unname(files['Load_Data']))
source(unname(files['Load_Data']))
dataset <- load.actual.panel(max_files = 300)

# Models ------------------------------------------------------------------

# file.edit(unname(files['Fama_MacBeth']))
file.edit(unname(files['Fama_MacBeth_fun']))
source(unname(files['Fama_MacBeth_fun']))
Fama_MacBeth_pred <- smoothed.WLS.pred(data = dataset$panel)

file.edit(unname(files['E_Lasso_fun']))
source(unname(files['E_Lasso_fun']))
s_lasso_pred <- smoothing.lasso.pred(dataset$panel)
c_lasso_pred <- combination.lasso.pred(dataset$panel)
e_lasso_pred <- encompassing.lasso.pred(
  s_lasso_pred_df = s_lasso_pred$prediction,
  c_lasso_pred_df = c_lasso_pred$prediction
  )

# file.edit(unname(files['E_Lasso_Portfolio']))
file.edit(unname(files['E_Lasso_Portfolio_fun']))
source(unname(files['E_Lasso_Portfolio_fun']))
eval.pred.fun(data_pred = Fama_MacBeth_pred$prediction)
