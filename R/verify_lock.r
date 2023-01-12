#' Verify the project library
#' 
#' @description Used to verify that the contents of the project library adhere
#'  to the conditions set out in the lock file.
#'  
#' @details Packages within the private library are checked for compliance with 
#'  the lock file. Missing packages are installed and packages whose
#'  installations fail to meet the lock file specification are reinstalled.
#'  
#'  The function does NOT remove packages that are installed in the project
#'  library and not contained within the lock file.
#'  
#' @param lock R list object containing the parsed contents of the lock file.
#'  The `read_lock()` function must be used to generate the required object.
#' @param ... Unused arguments, for future development.
#'  
#' @return Invisible, the function is called for it's side effects.
#' @export
verify_lock = function(lock, ...) {
  
  # check verse is active
  if (!exists("verse")) stop("Please activate verse")
  
  # check .libPaths are correct
  if (!identical(.libPaths()[1], verse$lib_path)) .libPaths(c(verse$lib_path, .libPaths()))
  
  # check verse.lock and ensure all packages in project lib
  ins_pac = as.data.frame(installed.packages(lib.loc = verse$verse_lib))$Package
  
  # check presence and version of all packages in lock file
  for (pac in lock) {
    tar_pac = pac["package"]
    tar_ver = strsplit(pac["version"], " ")
    
    tar_met_crit_1 = tar_pac %in% ins_pac
    
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
      tar_met_crit_2 = (packageVersion(tar_pac, lib.loc = verse$lib_path) == tar_ver$version[1])
    }
    
    # if lock file conditions aren't fulfilled; install
    if (!(tar_met_crit_1 && tar_met_crit_2)) {
      verse_install(
        package = pac["package"],
        version = pac["version"],
        dependencies = pac["dependencies"],
        upgrade = pac["upgrade"],
        lock_version = FALSE)
    }
  }
  return(invisible())
}
