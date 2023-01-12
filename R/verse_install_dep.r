

verse_install_dep = function(lib_path, ...) {
  
  pac_in_lib = as.data.frame(installed.packages(lib.loc = lib_path))
  
  deps = unique(unname(unlist(tools::package_dependencies(pac_in_lib$Package))))
  
  req_deps = setdiff(deps, pac_in_lib$Package)
  
  # remove base packages
  req_deps = req_deps[!req_deps %in% rownames(installed.packages(priority="base"))]
  
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
  
  return(invisible())
}


