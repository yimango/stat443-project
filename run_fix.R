# Run the fix on real results
# This assumes result object exists in the environment

library(tidyverse)

# Try to load result if it was saved
if (!exists("result")) {
  if (file.exists("result.RData")) {
    load("result.RData")
    message("Loaded result from result.RData")
  } else {
    stop("result object not found. Please run result <- main() first, or save it with save(result, file='result.RData')")
  }
}

# Source the fix script
source("fix_real_results.R")

