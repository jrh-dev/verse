
verse_install = function(
    package,
    version = NULL,
    dependencies = NA,
    upgrade = "always",
    lock_version = FALSE,
    verse_dep = TRUE,
    ...
    ) {
  
  stopifnot("Active verse project not found" = exists("verse"))
  
  # confirm path set ok
  if (!identical(.libPaths()[1], verse$project_lib)) {
    actual_lib_1 = .libPaths()[1]
    expected_lib_1 = verse$project_lib
    
    msg = glue::glue(
      "Primary library not detected as project library.\n\n",
      "Primary library;\n",
      "{actual_lib_1}\n\n",
      "Expected library;\n",
      "{expected_lib_1}\n\n",
      "Try runnning verse::activate_verse"
    )
    
    stop(msg)
  }
  
  remotes::install_version(
    package = package,
    version = version,
    dependencies = dependencies,
    upgrade = upgrade
  )
  
  # add trycatch to give informative error if version not found, maybe check
  # that version is as requested too (if requested)
  inst_ver = paste0(unlist(packageVersion(package)),collapse = ".")
  
  if (lock_version) {
    lockver = inst_ver
  } else {
    lockver = glue::glue(">= ", inst_ver)
  }
  
  new_lock_entry = setNames(
    c(package, lockver, dependencies, upgrade),
    c("package", "version", "dependencies", "upgrade")
  )
  
  lock_state = read_lock()
  
  in_lock = which(sapply(lock_state, function(x) x[["package"]]) == "odns")
  
  if (length(in_lock) > 0) {
    lock_state[[in_lock]] = NULL
  }
  
  lock_state[[length(lock_state) + 1]] = new_lock_entry
  
  write_lock(to_lock = lock_state)
  
  # verse_install_dep prevents recursion, will only go to depth 1.
  # within verse_install_dep, verse_dep == FALSE
  if (verse_dep) verse_install_dep(verse$project_lib)
  
  
  return(invisible())
}









