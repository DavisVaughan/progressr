## From Luke Tierney on 2019-09-19 per private email

setGlobalConditionHandler <- function(fun) {
    if (getRversion() < "4.0.0")
        stop("Global calling handlers requires R-devel (2019-09-14 r77188) or newer")
    .Internal(.addGlobHands("condition", list(fun), .GlobalEnv, NULL, TRUE))
}

globalCallingHandlers <-
    local({
        gh <- NULL
        function(...) {
            handlers <- list(...)
            if (length(handlers) == 0)
                gh
            else {
                classes <- names(handlers)
                if (length(classes) != length(handlers)) 
                    stop("bad handler specification")
                if (identical(classes, "condition") && is.null(handlers[[1]]))
                    gv <<- NULL
                else
                    gh <<- c(handlers, gh)
            }
        }
    })


initGlobalConditionHandler <- function() {
    setGlobalConditionHandler(function(cond) {
        handlers <- globalCallingHandlers()
        if (! is.null(handlers)) {
            classes <- names(handlers)
            .Internal(.addCondHands(classes, handlers, .GlobalEnv, NULL, TRUE))
            signalCondition(cond)
        }
    })
}

#' @export
report_on_progress <- function() {
  if (getRversion() >= "4.0.0") initGlobalConditionHandler()
  globalCallingHandlers(prog
}
