
read_lock = function(...) {
  
  if (!exists("verse")) stop("Please activate verse")
  
  if (!identical(.libPaths()[1], verse$lib_path)) .libPaths(c(verse$lib_path, .libPaths()))
  
  rw =  readLines("verse.lock", warn = FALSE)
  rw = rw[rw != ""]
  pc = 0
  p = vector("list", sum(grepl("[[package]]", rw)))
  
  for (line in rw) {
    if (line == "[[package]]") {
      pc = pc + 1
    } else {
      x = strsplit(line, ":")
      p[[pc]][x[[1]][1]] = x[[1]][2]
    }
  }
  return(p)
}