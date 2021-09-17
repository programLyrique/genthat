#' @export
#'
create_set_tracer <- function(session_file=NULL) {
    stopifnot(is.null(session_file) || is_chr_scalar(session_file))

    known_traces <-
        if (!is.null(session_file) && file.exists(session_file)) {
            if (is_debug_enabled()) {
                log_debug("Loading existing trace hashes from ", session_file)
            }

            readRDS(session_file)
        } else {
            new.env(parent=emptyenv(), hash=TRUE)
        }

    structure(
        list(
            traces=new.env(parent=emptyenv(), hash=TRUE),
            known_traces=known_traces,
            traced_functions=new.env(parent=emptyenv(), hash = TRUE),
            session_file=session_file
        ),
        class="set_tracer"
    )
}

#' @export
#'
store_trace.set_tracer <- function(tracer, trace) {
    # we need to compute the digest without the seed 
    # and without the return value
    trace_without_seed <- trace
    trace_without_seed$seed <- NULL
    trace_without_seed$retv <- NULL
    attr(trace_without_seed, "synthetic") <- NULL

    ser <- serialize(trace_without_seed, connection=NULL, ascii=FALSE)

    if (length(ser) > getOption("genthat.max_trace_size", .Machine$integer.max)) {
        trace <- create_trace(trace$fun, trace$pkg, skipped=length(ser))
        ser <- serialize(trace, connection=NULL, ascii=FALSE)
    }

    key <- digest::digest(ser, algo="sha1", serialize=FALSE)

    if (!attr(trace, "synthetic") && (is.null(tracer$known_traces[[key]]) || !is.logical(tracer$known_traces[[key]]))) {
        if(!is.null(tracer$known_traces[[key]]) && !is.logical(tracer$known_traces[[key]])) {
            log_debug("A prospective synthetic call was hit in the client code.")
        }
        tracer$known_traces[[key]] <- TRUE
        tracer$traces[[key]] <- trace
        tracer$traced_functions[[trace$fun]] <- get0(trace$fun, tracer$traced_functions, ifnotfound = 0) + 1
    }
    else if(attr(trace, "synthetic") && is.null(tracer$known_traces[[key]])) {
        log_debug("Adding prospective call for ", trace$fun)
        tracer$known_traces[[key]] <- trace
    }
    
    # That should not happen with synthetic traces (by construction)
    # try to see if it is a failed or errored call
    if (class(trace) %in% c("genthat_trace_error", "genthat_trace_failure")) {
        log_debug("Intercepted error or failure for ", trace$fun)
        
        # look if there is a corresponding prospective call
        trace_without_seed$error <- NULL
        trace_without_seed$failure <- NULL
        class(trace_without_seed) <- "genthat_trace"
        ser <- serialize(trace_without_seed, connection=NULL, ascii=FALSE)
        if (length(ser) > getOption("genthat.max_trace_size", .Machine$integer.max)) {
            trace <- create_trace(trace$fun, trace$pkg, skipped=length(ser))
            ser <- serialize(trace, connection=NULL, ascii=FALSE)
        }
        
        key <- digest::digest(ser, algo="sha1", serialize=FALSE)
        
        # if there is a prospective call with that, "remove" it
        trace_witness <- tracer$known_traces[[key]]
        if(!is.null(trace_witness) && !is.logical(trace_witness)) {
            log_debug("Just removed a failing prospective call.")
            # Still a logical so that it is not added back
            tracer$known_traces[[key]] <- FALSE 
        }
    }

    invisible(trace)
}

#' @export
#'
function_count.set_tracer <- function(tracer, fun) {
    return(get0(fun, tracer$traced_functions, ifnotfound=0))
}

#' @export
#'
has_trace.set_tracer <- function(tracer, fun, pkg=NULL, args=list(), globals=list()) {
    simple_trace <- list(fun=fun, pkg=pkg, args=as.list(args), globals=as.list(globals))
    # no seed, to retv, no synthetic
    class(simple_trace) <- "genthat_trace"
    
    
    ser <- serialize(simple_trace, connection=NULL, ascii=FALSE)
    
    if (length(ser) > getOption("genthat.max_trace_size", .Machine$integer.max)) {
        trace <- create_trace(trace$fun, trace$pkg, skipped=length(ser))
        ser <- serialize(trace, connection=NULL, ascii=FALSE)
    }
    
    key <- digest::digest(ser, algo="sha1", serialize=FALSE)
    
    value <- tracer$known_traces[[key]]
    
    return(!is.null(value) && is.logical(value))
}

#' @export
#'
reset_traces.set_tracer <- function(tracer) {
    rm(list=ls(envir=tracer$known_traces, sort=FALSE, all.names=TRUE), envir=tracer$known_traces)
    rm(list=ls(envir=tracer$traces, sort=FALSE, all.names=TRUE), envir=tracer$traces)
    rm(list=ls(envir=tracer$traced_functions, sort=FALSE, all.names=TRUE), envir=tracer$traced_functions)
}

#' @export
#'
copy_traces.set_tracer <- function(tracer) {
    if (!is.null(tracer$session_file)) {
        saveRDS(tracer$known_traces, tracer$session_file)
    }

    traces <- as.list(tracer$traces)
    names(traces) <- NULL
    traces
}
