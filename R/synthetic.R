#' @export
#'
gen_call <- function(trace, format_code = TRUE) {
  tryCatch({
    serializer <- new(Serializer)
    call <- generate_call(trace, serializer)
    globals <- generate_globals(trace$globals, serializer)
    
    #if(!is.null(trace$seed)) {
    #    header <- ".Random.seed <<- .ext.seed\n\n"
    #}
    
    code <- paste0(
      "{\n",
      globals,
      if (nchar(globals) > 0) '\n' else '',
      call, '\n',
      "}\n\n"
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
  
  # Write the prospective synthetic calls in the file
  # We need also to set up the seed and the globals as needed 
  
  for(call in synth_calls) {
    write(gen_call(call), file = file_script, append = TRUE)
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
        log_debug("Total number of synthetic runs: ", i - 1)
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