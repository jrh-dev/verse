restore_lock = function(lock_state, ...) {
  
  if (!exists("verse")) stop("Please activate verse")
  
  if (!identical(.libPaths()[1], verse$lib_path)) .libPaths(c(verse$lib_path, .libPaths()))
  
  for (x in seq_len(length(lock_state))) {
    remotes::install_version(
      package = lock_state[[x]][["package"]],
      version = lock_state[[x]][["version"]],
      dependencies = unname(unlist(strsplit((lock_state[[1]][["dependencies"]]), " "))),
      upgrade = lock_state[[x]][["upgrade"]]
    )
  }
  return(invisible())
}