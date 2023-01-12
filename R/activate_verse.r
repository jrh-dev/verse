#' Activate verse
#' 
#' @description Used to activate an existing verse setup. Activation requires
#'  the current working directory to have been used to initialize verse 
#'  previously and should therefore contain a custom `.Rprofile` and 
#'  `verse.lock` file.
#'  
#' @details Packages within the private library are checked for compliance with 
#'  the lock file upon activation. If the private library is not found then it
#'  will be created and rebuilt with package installation taking place based on
#'  the lock file.
#'  
#'  Base packages are not recorded in the lock file and are accessed according 
#'  to the users additional library paths in the way they would if verse was not
#'  being used. 
#'  
#'  @param ... Unused arguments, for future development.
#'  
#'  @return Invisible, the function is called for it's side effects.
#'  @export
activate_verse = function(...) {
  
  wd = getwd()
  
  # check verse.lock exists
  if (!file.exists(file.path(wd, "verse.lock"))) {
    # if it doesn't
    stop(._no_verse_lock(wd))
  }
  
  # set project lib and create if missing
  lib_path = file.path(wd, "lib")
  
  if (!dir.exists(lib_path)) {
    dir.create(lib_path)
    restore = TRUE
  }
  
  # add custom libpath in front of existing
  .libPaths(c(lib_path, .libPaths()))
  
  # confirm path set ok
  stopifnot("Unable to set project lib" = identical(.libPaths()[1], lib_path))
  
  # read lock file
  lock_state = read_lock()
  
  if (restore) {
    restore_lock(lock_state)
  } else {
    # check verse.lock and ensure all packages in project lib
    ins_pac = as.data.frame(installed.packages(lib.loc = lib_path))$Package
    for (pac in lock_state) {
      tar_pac = pac["package"]
      tar_ver = strsplit(pac["version"], " ")
      tar_met_crit_1 = tar_pac %in% ins_pac)
      
      if (length(tar_ver$version) > 1) {
        tar_met_crit_2 = switch(
          tar_ver$version[1],
          "<" = (packageVersion(tar_pac, lib.loc = lib_path) < tar_ver$version[2]),
          "<=" = (packageVersion(tar_pac, lib.loc = lib_path) <= tar_ver$version[2]),
          ">=" = (packageVersion(tar_pac, lib.loc = lib_path) >= tar_ver$version[2]),
          ">" = (packageVersion(tar_pac, lib.loc = lib_path) > tar_ver$version[2]),
          FALSE
        )
      } else {
        tar_met_crit_2 = (packageVersion(tar_pac, lib.loc = lib_path) == tar_ver$version[1])
      }
      
      if (!(tar_met_crit_1 && tar_met_crit_2)) {
        verse_install(
          package = pac["package"],
          version = pac["version"],
          dependencies = pac["dependencies"],
          upgrade = pac["upgrade"],
          lock_version = FALSE)
      }
    }
  }
  
  write("verse::activate()", ".Rprofile")
  verse = new.env(parent = parent.frame())
  assign("project_lib", lib_path, envir = verse)
  
  return(invisible())
}

._no_verse_lock = function(arg1) {
  return(glue::glue(
  "\'verse.lock\' file not found in working directory.\n\n",
  "Current working directory;\n\n",
  "{arg1}\n\n",
  "To initialise a verse project use verse::init()"
  ))
}
