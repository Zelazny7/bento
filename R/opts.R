## class that handles discretization by information value

validate <- function(opts) UseMethod("validate")

validate.iv_opts <- function(opts) {

  defaults <- list(
    iv.inc.min = 0.001,
    iv.dec.max = 0.0005,
    min.cnt    = 25,
    min.res    = 10,
    mono       = 0,
    max.bin    = 10,
    epsilon    = 0.01,
    expansion  = 1.1
  )

  ## create a list of defaults and modify it with the passed in options
  structure(modifyList(defaults, opts), class="iv_opts")

}

setOldClass(Classes = "iv_opts")

#' @export
discretizer_options <- function(type=c("iv","var"), ...) {
  type <- match.arg(type)
  
  opts <- list(...)
  class(opts) <- paste0(type, "_opts")

  validate(opts)
}

