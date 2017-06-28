.onLoad <- function(libname, pkgname) {
  package.path = system.file(package = "reinforcelearn")
  path2pythonfile = paste0(package.path, "/gym_http_server.py")
  command <- "python"
  system2(command, args = path2pythonfile, stdout = NULL, wait = FALSE)
} # what if python is not installed?? -> error
