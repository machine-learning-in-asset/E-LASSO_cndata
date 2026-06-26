ManagerShareSalary_path <- list.files(data_path['ManagerShareSalary'], pattern = "\\.xlsx", full.names = T)

ManagerShareSalary_data <- lapply(
  ManagerShareSalary_path,
  function(path1) {
    read_excel(path1) %>% slice(-c(1:2))
  }
) %>%
  rbindlist(use.names = F)
