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

tests_coverage <- function(pkg) {
  base_path <- "/mnt/ocfs_vol_00/pdonatbo/conditionals/packages/"
  #as.data.frame does not keep the package column, it seems
  tibble::add_column(as.data.frame(package_coverage(file.path(base_path, pkg), type="tests")),
                                       package = pkg)
}

coverage_number <- function(tests_coverage) {
  pkg <- tests_coverage[1, "package"]
  tibble::tibble(package = pkg, pkg_coverage = compute_coverage(tally_coverage(tests_coverage)))
}

coverage_number_genthat <- function(tests_coverage, res_genthat) {
  pkg <- tests_coverage[1, "package"]
  tibble::tibble(package = pkg, pkg_coverage = compute_coverage(tally_coverage(tests_coverage), attr(res_genthat, "raw_coverage")))
}

merge_cov_results <- function(res) {
  # Compute here coverage for the whole package before binding rows
  # Also, maybe find from that which functions have no coverage at all
  dplyr::bind_rows(res)
}

compare_coverages <- function(basic, with_genthat, with_synthetic) {
  dplyr::left_join(dplyr::rename(basic, basic_coverage = pkg_coverage), dplyr::rename(with_genthat, genthat_coverage = pkg_coverage)) %>%
    dplyr::left_join(dplyr::rename(with_synthetic, synthetic_coverage = pkg_coverage))
  
}