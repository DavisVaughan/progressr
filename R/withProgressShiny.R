#' Plug-in backward compatibility replacement for shiny::withProgress()
#'
#' @param expr,\ldots,env,quoted Arguments passed to [shiny::withProgress] as is.
#'
#' @param handlers Zero or more progression handlers used to report on progress.
#'
#' @return The value of `[shiny::withProgress]`.
#'
#' @example incl/withProgressShiny.R
#'
#' @export
withProgressShiny <- function(expr, ..., env = parent.frame(), quoted = FALSE, handlers = c(shiny = shiny_handler, progressr::handlers(default = NULL))) {
  if (!quoted) expr <- substitute(expr)
  expr <- bquote(progressr::with_progress({.(expr)}, handlers = .(handlers)))
  res <- withVisible(shiny::withProgress(expr, ..., env = env, quoted = TRUE))
  if (res$visible) res$value else invisible(res$value)
}