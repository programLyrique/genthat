#' @export
#'
gen_call <- function(trace, call_hash, external_file = "", format_code = TRUE) {
  tryCatch({
    externals <- new.env(parent=emptyenv())
    serializer <- new(Serializer)
    call <- generate_call(trace, serializer)
    globals <- generate_globals(trace$globals, serializer)
    
    #if(!is.null(trace$seed)) {
    #    header <- ".Random.seed <<- .ext.seed\n\n"
    #}
    
    # Gather the externals in an environment
    serializer$externals(externals)
    header <- ""
    if (length(externals) > 0) {
      # We add the hash to differentiate different invokations of the same function
      fname_ext <- paste0(tools::file_path_sans_ext(external_file), "-", escape_name(trace$fun), "-", call_hash, ".ext")
      
      saveRDS(externals, fname_ext)
      header <- paste0("env <- readRDS(\"", fname_ext, "\")\nparent.env(env) <- environment()\nlocal(")
    }
    
    # TODO: capture the errors in order to show for which function parameters things fail!
    
    code <- paste0(
      header,
      if (nchar(header) > 0) '\n' else '',
      "try({\n", # try otherwise any error will stop further processing of the synthetic call file
      globals,
      if (nchar(globals) > 0) '\n' else '',
      "`%>%` <- magrittr::`%>%`\n", # hack 
      call, 
      "\n})",
      if (nchar(header) > 0) ', envir = env)' else '',
      "\n\n"
    )
    
    if(format_code) {
      code <- reformat_code(code)
    }
    
    code
  }, error = function(e) {
    stop(simpleError(paste("Generate error:", trimws(e$message, which="both")), e$call))
  })
}

generate_synthetic_files <- function(tracer_type, session_file, output_dir, run_i) {
  stopifnot(tracer_type == "set")
  
  tracer <- genthat::set_tracer(
    genthat::create_tracer(
      "set",
      session_file=session_file
    )
  )
  
  # Find out the prospective calls
  synth_calls <- purrr::keep(as.list(tracer$known_traces), ~!is.logical(.))
    
  if(length(synth_calls) == 0) {
    return(NULL)
  }
  
  stopifnot(all(vapply(synth_calls, function(x) attr(x, "synthetic"), TRUE)))
  
  # Check here if all package directories already exist
  pkgs <- unique(Map(function(trace) trace$pkg, synth_calls))
  paths <- Map(function(pkg) file.path(output_dir, pkg), pkgs)
  for(path in paths) {
    stopifnot(dir.exists(path) || dir.create(path, recursive=TRUE))
  }
  
  log_debug("Generate synthetic files ", run_i, " with ", length(synth_calls), " new calls, for packages ", paste0(pkgs, collapse = " "))
  
  # Generate the files
  pkg_paths <- new.env(parent = emptyenv())
  for(pkg in pkgs) {
    log_debug("Creating file for pkg ", pkg)
    pkg_info <- list()
    script_name <- file.path(output_dir, pkg, paste0("synthetic_", run_i, ".R"))
    file_script <- file(script_name, open = "w+")# truncate any existing file
    pkg_info$script_name <- script_name
    pkg_info$file_script <- file_script
    on.exit(close(file_script))
    assign(pkg, pkg_info, pkg_paths)
  }
  

  
  
  # HACK
  # inject magrittr %>%
  #write("`%>%` <- magrittr::`%>%`\n\n", file = file_script, append = TRUE)
  
  # Write the prospective synthetic calls in the file
  
  for(call_hash in names(synth_calls)) {
    pkg_info <- get0(synth_calls[[call_hash]]$pkg, pkg_paths)
    if(is.null(pkg_info)) {
     stop("No package name for ", call_hash, ". Cannot continue.")
    }
    code <- gen_call(synth_calls[[call_hash]], call_hash, pkg_info$script_name)
    write(code, file = pkg_info$file_script, append = TRUE)
  }
  
  return(Map(function(pkg) pkg$script_name, as.list(pkg_paths)))
}

#' @export
#'
perform_synthetic_traces <- function(tracer_type, session_file, output_dir, run_file, max_runs = 5) {
  i <- 1
  runs <- data.frame(output=character(),
                     error=character())
  repeat {
    synth_files <- generate_synthetic_files(tracer_type, session_file, output_dir, i)
    if(is.null(synth_files) || i >= max_runs) {
      if(i == 1) {
        log_debug("No synthetic runs have been performed.")
      }
      else {
        log_debug("Total number of synthetic runs: ", if(i == max_runs) i else i - 1)
        if(!is_debug_enabled() && !is.null(synth_files)) {
          for(pkg in synth_files) {# One per package actually
            file.remove(pkg)
          }
        }
      }
      break
    }
    run <- lapply(synth_files, run_file)
    names(run) <- NULL # We remove the package name because there should actually be only one package here
    runs <- dplyr::bind_rows(runs, run) # more robust when there are different columns
    if(!is_debug_enabled() && !is.null(synth_files)) {
      # that won't remove the last one...
      for(pkg in synth_files) {# One per package actually
        file.remove(pkg)
      }
    }
    i <- i + 1
  }
  #TODO: return some metadata, such as the number of runs before stopping
  runs
}

