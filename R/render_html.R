#' This is a fairly pointless function, included only to match the syntax of other render_ functions
#' @param infile String giving the name of the file in the Rmd/ directory,
#'   not including the extension.
#' @param herePath String telling here::here where to look. Defaults to
#'   \code{herePath = "Rmd/"}.
#' @param output_file Passed to \code{rmarkdown::render}.
#' @export
render_html <- function(infile, herePath = "Rmd/", output_file = NULL){
  infile <- gsub(".Rmd", "", infile)
  inf <- paste0(here::here(herePath), infile, ".Rmd")
  render(inf, output_format = html_document(anchor_sections = FALSE),
         output_file = output_file)
}
