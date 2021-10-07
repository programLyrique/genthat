library(targets)
library(future)
library(future.callr)
source("R/functions.R")
options(tidyverse.quiet = TRUE)
options(genthat.source_paths = "/mnt/ocfs_vol_00/pdonatbo/conditionals/packages/")
#options(genthat.debug = TRUE)

plan(callr)

tar_option_set(
  packages = c("genthat", "readr", "covr", "magrittr"),
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
  tar_target(basic_cov, tests_coverage(packages_to_run), pattern = map(packages_to_run)),
  tar_target(basic_cov_num, coverage_number(basic_cov), pattern = map(basic_cov)),
  
  tar_target(result, gen_tests(packages_to_run, "tmp2"), pattern = map(packages_to_run)),
  tar_target(result_num, coverage_number_genthat(basic_cov, result), pattern = map(basic_cov, result)),
  
  tar_target(result_synthetic, gen_tests_synthetic(packages_to_run, "tmp2_syn"), pattern = map(packages_to_run)),
  tar_target(result_synthetic_num, coverage_number_genthat(basic_cov, result_synthetic), pattern = map(basic_cov, result_synthetic)),
  
  tar_target(basic_cov_packages, merge_cov_results(basic_cov_num)),
  tar_target(cov_packages, merge_cov_results(result_num)),
  tar_target(cov_packages_syn, merge_cov_results(result_synthetic_num)),
  
  tar_target(compared_coverages, compare_coverages(basic_cov_packages, cov_packages, cov_packages_syn))
)