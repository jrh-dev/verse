

verse_install_dep = function(lib_path, ...) {
  
  # check verse is active
  if (!exists("verse")) stop("Please activate verse")
  
  # check .libPaths are correct
  if (!identical(.libPaths()[1], verse$lib_path)) .libPaths(c(verse$lib_path, .libPaths()))
  
  # identify packages installed in the project library and dependencies
  pac_in_lib = as.data.frame(installed.packages(lib.loc = verse$project_lib))
  deps = unique(unname(unlist(tools::package_dependencies(pac_in_lib$Package))))
  
  # identify any dependencies not installed
  req_deps = setdiff(deps, pac_in_lib$Package)
  
  # remove base packages from consideration
  req_deps = req_deps[!req_deps %in% rownames(installed.packages(priority="base"))]
  
  # install
  if (length(req_deps) > 0) {
    for (pac in req_deps) {
      verse_install(
        pac,
        version = NULL,
        dependencies = NA,
        upgrade = "always",
        lock_version = FALSE,
        verse_dep = FALSE
      )
    }
  } else {
    write_lock()
  }
  
  return(invisible())
}


