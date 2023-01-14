#' Read and parse a `verse.lock` file.
#' 
#' @param ... Unused arguments, for future development.
#'  
#' @return Invisible, the function is called for it's side effects.
._read_lock = function(...) {
  
  # check verse is active
  if (!exists(".verse")) stop("Verse does not appear to be active.")
  
  # check .libPaths are correct
  if (!identical(.libPaths()[1], .verse$lib_path)) .libPaths(c(.verse$lib_path, .libPaths()))
  
  lock =  readLines("verse.lock", warn = FALSE)
  lock = lock[lock != ""]
  
  iter = 0
  pac = vector("list", sum(grepl("[[package]]", lock)))
  
  for (line in lock) {
    if (line == "[[package]]") {
      iter = iter + 1
    } else {
      x = strsplit(line, ":")
      pac[[iter]][x[[1]][1]] = x[[1]][2]
    }
  }
  
  return(pac)
}
