# Check that an object is a ShinySession object, and give an informative error.
# The default label is the caller function's name.
validate_session_object <- function(session, label = as.character(sys.call(sys.parent())[[1]])) {
  if (missing(session) ||
      !inherits(session, c("ShinySession", "MockShinySession", "session_proxy")))
  {
    stop(call. = FALSE,
         sprintf(
           "`session` must be a 'ShinySession' object. Did you forget to pass `session` to `%s()`?",
           label
         )
    )
  }
}

# dropNulls
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

updateActionBttn <- function (inputId, 
                              label = NULL,
                              session = getDefaultReactiveDomain()){
  validate_session_object(session)
  message <- dropNulls(list(label = label))
  session$sendInputMessage(inputId, message)
}