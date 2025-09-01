#setup r-environment - I had difficulties with installing packages on the server

# Always set a repo (Rscript is non-interactive)
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Ensure user lib exists & is first on the path
usr <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(usr)) dir.create(usr, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(usr, .libPaths()))

# Helper: install missing packages to the user lib
install_if_missing <- function(pkgs) {
  need <- setdiff(pkgs, rownames(installed.packages()))
  if (length(need)) {
    install.packages(need, lib = usr, dependencies = TRUE, Ncpus = max(1, parallel::detectCores()-1))
  }
}

# List ONLY what you actually use
install_if_missing(c(
  "dplyr","tibble","jsonlite","stringr",
  "httr","xml2","curl","openssl",
  "readr","tidyr","purrr","lubridate",
  "data.table","png","jpeg","knitr","rmarkdown"
))
