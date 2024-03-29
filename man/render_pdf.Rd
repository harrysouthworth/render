% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/render_pdf.R
\name{render_pdf}
\alias{render_pdf}
\alias{render_word}
\title{Auto-edit Rmd to make it more suitable for LaTeX, and create PDF}
\usage{
render_pdf(
  infile,
  herePath = "Rmd/",
  keep = FALSE,
  render = TRUE,
  output_file = NULL,
  keep_tex = FALSE,
  custom_removal = NULL,
  include = includes(in_header = "parahdr.tex"),
  toc = TRUE,
  toc_depth = 3,
  number_sections = TRUE,
  cleanup_ask = FALSE
)
}
\arguments{
\item{infile}{String giving the name of the input file, without the path
and (optionally) without the file extension.}

\item{herePath}{String to pass to \code{here::here} to get the correct path
to the directory containing the Rmd file.}

\item{keep}{Whether to keep the temporary Rmd file produced. Defaults to
\code{keep = FALSE}.}

\item{render}{Whether to render the PDF. Defaults to \code{render = TRUE}.}

\item{output_file}{String, giving the name of the output file. If not given,
the default behaviour of \code{render} should occur.}

\item{keep_tex}{Passed to \code{pdf_document}.}

\item{custom_removal}{Optional string to be replaced with an empty string.}

\item{include}{Defaults to \code{include = includes(in_header = "parahdr.tex"6)}
and file "parahdr.tex" is written by the function itself. If parahdr.tex
is not in \code{include}, figures might not be properly placed.}

\item{toc, toc_depth, number_sections}{Passed to \code{rmarkdown::pdf_document}.}

\item{cleanup_ask}{Whether to ask before removing temporary files. Defaults
to \code{cleanup_ask = TRUE}, but this is intended to be temporary.}
}
\value{
Invisibly, a character vector containing hte modified Rmd.
}
\description{
Auto-edit Rmd to make it more suitable for LaTeX, and create PDF
}
\details{
The function assumes your deepest heading is 5 or lower,
  and that the level 1 heading is abused by being used to create a top level
  tabset in the HTML output. The code is simple and will likely break at
  times.
}
\note{
The function replaces all instances of "# " with " ", the idea being
  that for section headings, or those that are written by cat statements,
  one hash is removed. This also means that any comments will need to be
  started with more than one hash or they will be uncommented. In principle
  we should be able to use \code{rmarkdown::render(..., output_format = "all")}.
  However, accurately picking up the output format seems to not always work
  or be picked up by some of the other functions, such as \code{output_table}.
}
\examples{
theFormat <- "html"; render("doc")
         theFormat <- "pdf"; render("doc")
         theFormat <- "word"; render("doc")
}
