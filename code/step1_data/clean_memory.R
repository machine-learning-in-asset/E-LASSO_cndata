clean_big_objects <- function(threshold_mb = 500) {
  
  obj_size <- sapply(ls(envir = .GlobalEnv), function(x) {
    object.size(get(x, envir = .GlobalEnv))
  })
  
  big_objs <- names(obj_size[obj_size > threshold_mb * 1024^2])
  
  if (length(big_objs) == 0) {
    message("No objects larger than ", threshold_mb, " MB.")
  } else {
    message("Removing objects: ", paste(big_objs, collapse = ", "))
    rm(list = big_objs, envir = .GlobalEnv)
    gc()
  }
}

clean_big_objects(20)