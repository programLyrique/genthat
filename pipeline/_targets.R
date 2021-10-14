library(targets)
library(tarchetypes)
library(future)
library(future.callr)
source("R/functions.R")
options(tidyverse.quiet = TRUE)
source_path <- "/mnt/ocfs_vol_00/pdonatbo/conditionals/packages/"
lib_path <- "/mnt/ocfs_vol_00/pdonatbo/conditionals/libs/"
options(genthat.source_paths = source_path)
options(future.wait.timeout = 60 * 60) # do not allow more than 1h for each task
#options(genthat.debug = TRUE)

plan(callr)

tar_target_resilient <- function(name, command, pattern, ...) {
  tname <- deparse(substitute(name))
  tname_err <- paste0(tname, "_err")
  sym_err <- parse(text = tname_err)[[1]]
  wrapped_command <- substitute(tryCatch(COMMAND, error = function(e) NULL), list(COMMAND = substitute(command)))
  # Or Filter if we do not want to use purrr
  compact_command <- substitute(purrr::compact(TNAME_ERR), list(TNAME_ERR = sym_err))
  tpattern <- substitute(map(TNAME_ERR), list(TNAME_ERR = sym_err))
  list(
    tar_target_raw(tname_err, wrapped_command, pattern = substitute(pattern), iteration = "list", ...),
    tar_target_raw(tname, command = compact_command, pattern = tpattern)
  )
}

tar_option_set(
  packages = c("genthat", "readr", "covr", "magrittr"),
  imports = c("genthat"),
  #error = "continue" # always continue by default
)

list(
  tar_target(
    packages_file,
    "data/packages.txt",
    format = "file"
  ),
  tar_target(
    packages_to_install,
    read_lines(packages_file)
  ),
  tar_target(
    packages_to_run,
    install_cran_packages(packages_to_install, lib_path, source_path),
    deployment = "main"
  ),
  
  tar_target_resilient(basic_cov, tests_coverage(packages_to_run), pattern = map(packages_to_run)),
  tar_target_resilient(basic_cov_num, coverage_number(basic_cov), pattern = map(basic_cov)),
  
  tar_target_resilient(result, gen_tests(packages_to_run, "tmp2", lib_path), pattern = map(packages_to_run)),
  tar_target_resilient(result_num, coverage_number_genthat(basic_cov, result), pattern = map(basic_cov, result)),
  
  tar_target_resilient(result_synthetic, gen_tests_synthetic(packages_to_run, "tmp2_syn", lib_path), pattern = map(packages_to_run)),
  tar_target_resilient(result_synthetic_num, coverage_number_genthat(basic_cov, result_synthetic), pattern = map(basic_cov, result_synthetic)),
  
  tar_target(basic_cov_packages, merge_cov_results(basic_cov_num)),
  tar_target(cov_packages, merge_cov_results(result_num)),
  tar_target(cov_packages_syn, merge_cov_results(result_synthetic_num)),
  
  tar_target(compared_coverages, compare_coverages(basic_cov_packages, cov_packages, cov_packages_syn)),
  
  tar_target(metrics, metrics_coverage(compared_coverages))
)