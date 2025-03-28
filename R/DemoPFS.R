#' Interactive PFS Demonstration
#'
#' @return Opens a shiny app session.
#'
#' @export
DemoPFS <- function () {
  app_dir <- system.file('shiny', package = 'pfs')
  if (app_dir == '') {
    stop("Could not find example directory. Try re-installing 'pfs'.",
         call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = 'normal')
}
