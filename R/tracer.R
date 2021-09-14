#' @export
#'
create_tracer <- function(type="set", ...) {
    type <- match.arg(arg=type, choices=c("sequence", "set"), several.ok=FALSE)
    fun <- switch(type,
        sequence=create_sequence_tracer,
        set=create_set_tracer
    )

    fun(...)
}

#' @name Store tracer
#' @title Stores given trace to the tracer
#'
#' @export
#'
store_trace <- function(tracer, trace) {
    UseMethod("store_trace")
}

#' @name Check existence of a trace
#' @title Check if the trace would exist after the call
#'
#' @export
#'
has_trace <- function(tracer, fun, pkg=NULL, args=list(), globals=list()) {
    UseMethod("has_trace")
}

#' @name Cound of the function name
#' @title How many times we have already seen the function (with any arguments)
#'
#' @export
#'
function_count <- function(tracer, fun) {
    UseMethod("function_count")
}

#' @name Reset traces
#' @title Clears the captured traces
#'
#' @export
#'
reset_traces <- function(tracer) {
    UseMethod("reset_traces")
}

#' @name Copy call traces
#' @title Creates a copy of traces captured so far and returns them as R list.
#'
#' @export
#'
copy_traces <- function(tracer) {
    UseMethod("copy_traces")
}

#' @export
#'
copy_traces.default <- function(tracer) {
    tracer <- get_tracer()
    stopifnot(!is.null(tracer))
    copy_traces(tracer)
}

#' @export
#'
reset_traces.default <- function(tracer) {
    tracer <- get_tracer()
    stopifnot(!is.null(tracer))
    reset_traces(tracer)
}

#' @export
#'
set_tracer <- function(tracer) {
    stopifnot(!is.null(tracer))

    options(genthat.tracer=tracer)

    invisible(tracer)
}

#' @export
#'
get_tracer <- function() {
    tracer <- getOption("genthat.tracer")

    if (is.null(tracer)) {
        tracer <- create_tracer()
        set_tracer(tracer)
    }

    tracer
}
