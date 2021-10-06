# because future does not capture options
set_common_options <- function() {
  options(tidyverse.quiet = TRUE)
  options(genthat.source_paths = "/mnt/ocfs_vol_00/pdonatbo/conditionals/packages/")
}

gen_tests <- function(pkg, output) {
  set_common_options()
  tibble::add_column(gen_from_package(
    pkg,
    types="all", 
    action="generate", 
    prune_tests=TRUE, 
    output_dir=output
  ), package = pkg)
}

gen_tests_synthetic <- function(pkg, output) {
  set_common_options()
  options(genthat.synthetic = TRUE)
  on.exit(options(genthat.synthetic = FALSE))
  gen_tests(pkg, output)
}

merge_cov_results <- function(res) {
  dplyr::bind_rows(res)
}