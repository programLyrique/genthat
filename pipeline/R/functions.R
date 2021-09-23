gen_tests <- function(pkg, output) {
  gen_from_package(
    pkg,
    types="all", 
    action="generate", 
    prune_tests=TRUE, 
    output_dir=output
  ) 
}

gen_tests_synthetic <- function(pkg, output) {
  options(genthat.synthetic = TRUE)
  gen_tests(pkg, output)
  options(genthat.synthetic = FALSE)
}

merge_cov_results <- function(res) {
  # TODO: add the package name
  dplyr::bind_rows(res)
}