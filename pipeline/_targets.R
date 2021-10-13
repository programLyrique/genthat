library(targets)
library(tarchetypes)
library(future)
library(future.callr)
source("R/functions.R")
options(tidyverse.quiet = TRUE)
options(genthat.source_paths = "/mnt/ocfs_vol_00/pdonatbo/conditionals/packages/")
options(future.wait.timeout = 60 * 60) # do not allow more than 1h for each task
#options(genthat.debug = TRUE)

plan(callr)

# We use a custom environment instead of NULL in case one of the targets would actually
# return NULL as a normal value
error_marker <- new.env(parent = emptyenv())

tar_target_resilient <- function(name, command, pattern, ...) {
  tname <- deparse(substitute(name))
  tname_err <- paste0(tname, "_err")
  sym_err <- parse(text = tname_err)[[1]]
  wrapped_command <- substitute(tryCatch(COMMAND, error = function(e) error_marker), list(COMMAND = substitute(command)))
  # Or Filter if we do not want to use purrr
  compact_command <- substitute(purrr::discard(TNAME_ERR, function(e) identical(e, error_marker)), list(TNAME_ERR = sym_err))
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
  tar_target_resilient(
    packages_to_run,
    add_package(packages_to_install),
    pattern = map(packages_to_install),
    deployment = "main"
    #error = "continue" # it is ok if we cannot install a package, we just do not compute anything on it
  ),
  
  # Use error = "continue" for all the coverage instrumentation targets?
  tar_target_resilient(basic_cov, tests_coverage(packages_to_run), pattern = map(packages_to_run)),
  tar_target(basic_cov_num, coverage_number(basic_cov), pattern = map(basic_cov)),
  
  tar_target_resilient(result, gen_tests(packages_to_run, "tmp2"), pattern = map(packages_to_run)),
  tar_target(result_num, coverage_number_genthat(basic_cov, result), pattern = map(basic_cov, result)),
  
  tar_target_resilient(result_synthetic, gen_tests_synthetic(packages_to_run, "tmp2_syn"), pattern = map(packages_to_run)),
  tar_target(result_synthetic_num, coverage_number_genthat(basic_cov, result_synthetic), pattern = map(basic_cov, result_synthetic)),
  
  tar_target(basic_cov_packages, merge_cov_results(basic_cov_num)),
  tar_target(cov_packages, merge_cov_results(result_num)),
  tar_target(cov_packages_syn, merge_cov_results(result_synthetic_num)),
  
  tar_target(compared_coverages, compare_coverages(basic_cov_packages, cov_packages, cov_packages_syn))
)