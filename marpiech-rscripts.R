load_package <- function(package.name) {
  if (!require(package.name, character.only = TRUE)) {
    install.packages(package.name, dep=TRUE)
    if(!require(package.name, character.only = TRUE)) stop("Package not found")
  }
}

#source("https://raw.githubusercontent.com/marpiech/rscripts/master/enrichR.R")
