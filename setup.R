# Run once in the Posit Cloud RStudio console: source("setup.R")
packages <- c("shiny", "bslib", "tidyverse", "DT", "httr", "jsonlite")
missing <- setdiff(packages, rownames(installed.packages()))
if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}
unavailable <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(unavailable)) {
  stop("Packages unavailable: ", paste(unavailable, collapse = ", "), call. = FALSE)
}
message("Dependencies ready. Run shiny::runApp() from this folder.")
