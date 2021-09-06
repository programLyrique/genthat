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
    }
    else {
        tracer$known_traces[[key]] <- trace
    }

    invisible(trace)
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
    
    return(!is.null(tracer$known_traces[[key]]))
}

#' @export
#'
reset_traces.set_tracer <- function(tracer) {
    rm(list=ls(envir=tracer$known_traces, sort=FALSE, all.names=TRUE), envir=tracer$known_traces)
    rm(list=ls(envir=tracer$traces, sort=FALSE, all.names=TRUE), envir=tracer$traces)
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
