
verse_install = function(
    package,
    version = NULL,
    dependencies = NA,
    upgrade = "always",
    lock_version = FALSE,
    verse_dep = TRUE,
    ...
    ) {
  
  # check verse is active
  if (!exists("verse")) stop("Please activate verse")
  
  # check .libPaths are correct
  if (!identical(.libPaths()[1], verse$lib_path)) .libPaths(c(verse$lib_path, .libPaths()))
  
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
  
  write_lock()
  
  # When calling verse_install_dep, recursion is prevented, confirmed dependency
  # installation will only go to depth 1. Additional dependencies are installed
  # but not added to the lockfile.
  if (verse_dep) verse_install_dep(verse$project_lib) else write_lock()
  
  return(invisible())
}









