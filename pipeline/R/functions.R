# because future does not capture options
set_common_options <- function() {
  options(tidyverse.quiet = TRUE)
  options(genthat.source_paths = "/mnt/ocfs_vol_00/pdonatbo/conditionals/packages/")
}

gen_tests <- function(pkg, output, lib_paths = .libPaths()[1]) {
  set_common_options()
  print(pkg) # or tar_message_run ?
  tibble::add_column(gen_from_package(
    pkg,
    types="all", 
    action="generate", 
    prune_tests=TRUE, 
    output_dir=output,
    lib_paths = lib_paths
  ), package = pkg)
}

gen_tests_synthetic <- function(pkg, output, lib_paths = .libPaths()[1]) {
  set_common_options()
  options(genthat.synthetic = TRUE)
  on.exit(options(genthat.synthetic = FALSE))
  gen_tests(pkg, output, lib_paths)
}

tests_coverage <- function(pkg,lib_paths = .libPaths()[1]) {
  base_path <- "/mnt/ocfs_vol_00/pdonatbo/conditionals/packages/"
  #as.data.frame does not keep the package column, it seems
  tibble::add_column(as.data.frame(package_coverage(file.path(base_path, pkg), type="tests")),
                                       package = pkg)
}

coverage_number <- function(tests_coverage) {
  pkg <- tests_coverage[1, "package"]
  tibble::tibble(package = pkg, pkg_coverage = compute_coverage(tally_coverage(tests_coverage)))
}


install_cran_packages <- function(packages_to_install,
                                  lib=NULL,
                                  destdir=NULL,
                                  mirror = "https://cloud.r-project.org/") {
  options(repos=mirror)
  
  requested <- packages_to_install
  
  installed <- installed.packages(lib.loc=lib)[,1]
  missing <- setdiff(requested, installed)
  
  message("Installing ", length(missing), " packages from ", mirror, " into ", lib)
  
  if (length(missing) > 0) {
    if (!is.null(destdir) && !dir.exists(destdir)) dir.create(destdir, recursive=TRUE)
    if (!is.null(lib) && !dir.exists(lib)) dir.create(lib, recursive=TRUE)
  }
  
  # set package installation timeout
  Sys.setenv(
    `_R_INSTALL_PACKAGES_ELAPSED_TIMEOUT_`=Sys.getenv("_R_INSTALL_PACKAGES_ELAPSED_TIMEOUT_", "5000")
  )
  
  install.packages(
    missing,
    lib=lib,
    destdir=destdir,
    dependencies=TRUE,
    INSTALL_opts=c("--example", "--install-tests", "--with-keep.source", "--no-multiarch"),
    Ncpus=floor(.9*parallel::detectCores())
  )
  
  installed <- installed.packages(lib.loc=lib)[,1]
  successful_installed <- intersect(installed, requested)
  
  # Extract the sources from the missing ones that successfully installed
  installed_missing <- intersect(successful_installed, missing)

  
  archives <- list.files(destdir, pattern = "\\.tar\\.gz$", full.names = TRUE)
  for(package in installed_missing) {
    destfiles <- grep(package, archives, value = TRUE, fixed = TRUE)
    if(length(destfiles) == 0) {
      next;
    }
    destfile <- destfiles[[1]] # there should be only one anyway
    pkgdir <- file.path(destdir, package)
    if(dir.exists(pkgdir)) {
      warning("Destination directory for extracting exists ", pkgdir, "\n")
    }
    message("Extracting", destfile ," to ", pkgdir, "\n")
    utils::untar(destfile, exdir=destdir)
    file.remove(destfile) # remove the archive so that it does not interfere with newer versions
  }
  
  successful_installed
}

# Make sure that packages are installed and their sources are accessible
add_package <- function(pkg) {
  set_common_options()
  options(warn = 2) #Fail when there is a warning already
  on.exit(options(warn = 0))
  #is it installed
  if(nchar(system.file(package = pkg)) == 0) {
    install.packages(pkg, dependencies = TRUE)
  }
  #Is the source code accessible
  pkg_path <- file.path(getOption("genthat.source_paths"), pkg)
  if(!dir.exists(pkg_path)) {
    download_package(pkg, destdir = getOption("genthat.source_paths"))
  }
  pkg
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

metrics_coverage <- function(coverages) {
  dplyr::mutate(coverages, diff = synthetic_coverage - genthat_coverage)
}