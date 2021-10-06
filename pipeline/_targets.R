library(targets)
library(future)
library(future.callr)
source("R/functions.R")
options(tidyverse.quiet = TRUE)
options(genthat.source_paths = "/mnt/ocfs_vol_00/pdonatbo/conditionals/packages/")
#options(genthat.debug = TRUE)

plan(callr)

tar_option_set(
  packages = c("genthat", "readr"),
  imports = c("genthat")
)

list(
  tar_target(
    packages_file,
    "data/packages.txt",
    format = "file"
  ),
  tar_target(
    packages_to_run,
    read_lines(packages_file)
  ),
  tar_target(result, gen_tests(packages_to_run, "tmp2"), pattern = map(packages_to_run)),
  tar_target(result_synthetic, gen_tests_synthetic(packages_to_run, "tmp2_syn"), pattern = map(packages_to_run)),
  tar_target(cov_packages, merge_cov_results(result)),
  tar_target(cov_packages_syn, merge_cov_results(result_synthetic))
)