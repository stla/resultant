dllunload <- function(){
  dyn.unload(
    system.file("libs", "x64", "resultant.dll", package = "resultant")
  )
}

myinstall <- function() {
  try(pkgload::unload("resultant"))
  Rcpp::compileAttributes()
  if(rstudioapi::isAvailable()) {
    rstudioapi::restartSession(
      "devtools::install(quick = TRUE, keep_source = TRUE)"
    )
  } else {
    #try(dllunload())
    devtools::install(quick = TRUE, keep_source = TRUE)
  }
}

mydocument <- function() {
  if(rstudioapi::isAvailable()) {
    rstudioapi::restartSession(
      "roxygen2::roxygenise(load_code = roxygen2::load_installed)"
    )
  } else {
    roxygen2::roxygenise(load_code = roxygen2::load_installed)
  }
}
