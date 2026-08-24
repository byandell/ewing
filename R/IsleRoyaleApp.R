#' Isle Royale Wolf-Moose Simulation App (Wrapper Shell)
#'
#' Backward-compatible wrapper for launching the Isle Royale wolf-moose spatial predator-prey
#' simulation application using the generalized `ecosystemApp` platform.
#'
#' @param title Application title string (default: `"Isle Royale Wolf-Moose Simulation Platform"`).
#' @param id Module ID string.
#' @export
#' @rdname IsleRoyaleApp
#' @importFrom bslib page_sidebar sidebar
IsleRoyaleApp <- function(title = "Isle Royale Wolf-Moose Simulation Platform") {
  ecosystemApp(ecosystem = "isle_royale", title = title)
}

#' Isle Royale Input Controls Module Wrapper
#' @param id Module ID string
#' @export
#' @rdname IsleRoyaleApp
IsleRoyaleInput <- function(id) {
  ecosystemInput(id, ecosystem = "isle_royale")
}

#' Isle Royale Output Display Module Wrapper
#' @param id Module ID string
#' @export
#' @rdname IsleRoyaleApp
IsleRoyaleOutput <- function(id) {
  ecosystemOutput(id, ecosystem = "isle_royale")
}

#' Isle Royale Server Logic Module Wrapper
#' @param id Module ID string
#' @export
#' @rdname IsleRoyaleApp
IsleRoyaleServer <- function(id) {
  ecosystemServer(id, ecosystem = "isle_royale")
}

#' @export
#' @rdname IsleRoyaleApp
isleRoyaleApp <- IsleRoyaleApp

#' @export
#' @rdname IsleRoyaleApp
isleRoyaleInput <- IsleRoyaleInput

#' @export
#' @rdname IsleRoyaleApp
isleRoyaleOutput <- IsleRoyaleOutput

#' @export
#' @rdname IsleRoyaleApp
isleRoyaleServer <- IsleRoyaleServer
