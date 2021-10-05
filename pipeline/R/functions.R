gen_tests <- function(pkg, output) {
  tibble::add_column(gen_from_package(
    pkg,
    types="all", 
    action="generate", 
    prune_tests=TRUE, 
    output_dir=output
  ), package = pkg)
}

gen_tests_synthetic <- function(pkg, output) {
  options(genthat.synthetic = TRUE)
  on.exit(options(genthat.synthetic = FALSE))
  gen_tests(pkg, output)
}

merge_cov_results <- function(res) {
  dplyr::bind_rows(res)
}