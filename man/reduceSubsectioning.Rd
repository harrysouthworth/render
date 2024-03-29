% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/render_helpers.R
\name{reduceSubsectioning}
\alias{reduceSubsectioning}
\title{Reduce subsections by 1 and remove some other guff}
\usage{
reduceSubsectioning(infile, herePath = "Rmd/", custom_removal = NULL)
}
\arguments{
\item{infile}{String giving the name of the input file, without the path
and (optionally) without the file extension.}

\item{herePath}{String to pass to \code{here::here} to get the correct path
to the directory containing the Rmd file.}

\item{custom_removal}{Character vector giving strings to be replaced with
empty strings. Defaults to \code{custom_removal = NULL}.}
}
\description{
Reduce subsections by 1 and remove some other guff
}
