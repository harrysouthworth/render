#' Auto-edit Rmd to make it more suitable for LaTeX, and create PDF
#' @param infile String giving the name of the input file, without the path
#'   and (optionally) without the file extension.
#' @param herePath String to pass to \code{here::here} to get the correct path
#'   to the directory containing the Rmd file.
#' @param keep Whether to keep the temporary Rmd file produced. Defaults to
#'   \code{keep = FALSE}.
#' @param render Whether to render the PDF. Defaults to \code{render = TRUE}.
#' @param output_file String, giving the name of the output file. If not given,
#'   the default behaviour of \code{render} should occur.
#' @param keep_tex Passed to \code{pdf_document}.
#' @param custom_removal Optional string to be replaced with an empty string.
#' @param include Defaults to \code{include = includes(in_header = "parahdr.tex"6)}
#'   and file "parahdr.tex" is written by the function itself. If parahdr.tex
#'   is not in \code{include}, figures might not be properly placed.
#' @param toc,toc_depth,number_sections Passed to \code{rmarkdown::pdf_document}.
#' @param cleanup_ask Whether to ask before removing temporary files. Defaults
#'   to \code{cleanup_ask = TRUE}, but this is intended to be temporary.
#' @return Invisibly, a character vector containing hte modified Rmd.
#' @details The function assumes your deepest heading is 5 or lower,
#'   and that the level 1 heading is abused by being used to create a top level
#'   tabset in the HTML output. The code is simple and will likely break at
#'   times.
#' @note The function replaces all instances of "# " with " ", the idea being
#'   that for section headings, or those that are written by cat statements,
#'   one hash is removed. This also means that any comments will need to be
#'   started with more than one hash or they will be uncommented. In principle
#'   we should be able to use \code{rmarkdown::render(..., output_format = "all")}.
#'   However, accurately picking up the output format seems to not always work
#'   or be picked up by some of the other functions, such as \code{output_table}.
#' @aliases render_word
#' @example theFormat <- "html"; render("Rmd/doc.Rmd")
#'          theFormat <- "pdf"; render("Rmd/doc.Rmd")
#'          theFormat <- "word"; render("Rmd/doc.Rmd)
#' @export
render_pdf <- function(infile, herePath = "Rmd/", keep = FALSE, render = TRUE,
                   output_file = NULL, keep_tex = FALSE,
                   custom_removal = NULL,
                   include = includes(in_header = "parahdr.tex"),
                   toc = TRUE, toc_depth = 3, number_sections = TRUE,
                   cleanup_ask = FALSE){

  fnms <- list.files(here(herePath))
  if ("parahdr.tex" %in% fnms){
    message("overwriting parahdr.tex")
  }
  parahdr(here(herePath))
  on.exit(file.remove(here::here(herePath, "parahdr.tex")))

  outf <- reduceSubsectioning(infile, herePath)
  #outf <- gsub("%", "\\\\%", outf)

  if (!keep && !keep_tex){
    on.exit(cleanup(outf, ask = cleanup_ask), add = TRUE)
  }

  render(outf, output_format = pdf_document(toc = toc, toc_depth = toc_depth,
                                            number_sections = number_sections,
                                            includes = include, keep_tex = keep_tex,
                                            extra_dependencies = c("float", "fancyhdr")),
         output_file = output_file)

  pdffile <- paste0(substring(outf, 1, nchar(outf) - 14), ".pdf")
  invisible(try(file.rename(gsub("\\.Rmd", "\\.pdf", outf), pdffile), silent = TRUE))

  invisible(outf)
}
