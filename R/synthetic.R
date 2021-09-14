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
    
    code <- paste0(
      header,
      if (nchar(header) > 0) '\n' else '',
      "{\n",
      globals,
      if (nchar(globals) > 0) '\n' else '',
      call, 
      "\n}",
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

generate_synthetic_file <- function(tracer_type, session_file, output_dir, run_i) {
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
  
  
  log_debug("Run synthetic file ", run_i, " with ", length(synth_calls), " new calls.")
  
  # Generate the file
  script <- file.path(output_dir, paste0("synthetic_", run_i, ".R"))
  
  file_script <- file(script, open = "w+")# truncate any existing file
  
  # HACK
  # inject magrittr %>%
  #write("`%>%` <- magrittr::`%>%`\n\n", file = file_script, append = TRUE)
  
  # Write the prospective synthetic calls in the file
  
  for(call_hash in names(synth_calls)) {
    code <- gen_call(synth_calls[[call_hash]], call_hash, script)
    write(code, file = file_script, append = TRUE)
  }
  
  return(script)
}

#' @export
#'
perform_synthetic_traces <- function(tracer_type, session_file, output_dir, run_file, max_runs = 10) {
  i <- 1
  runs <- data.frame(output=character(),
                     error=character())
  repeat {
    synth_file <- generate_synthetic_file(tracer_type, session_file, output_dir, i)
    if(is.null(synth_file) || i >= max_runs) {
      if(i == 1) {
        log_debug("No synthetic runs have been performed.")
      }
      else {
        log_debug("Total number of synthetic runs: ", if(i == max_runs) i else i - 1)
      }
      break
    }
    run <- run_file(synth_file)
    runs <- rbind(runs, run)
    if(!is_debug_enabled() && !is.null(synth_file)) {
      file.remove(synth_file)
    }
    i <- i + 1
  }
  #TODO: return some metadata, such as the number of runs before stopping
  runs
}