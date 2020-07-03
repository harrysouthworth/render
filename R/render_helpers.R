#' Print table to HTML, PDF or Word from within a for loop
#' @param x The output of \code{output_table}.
#' @param format String: either "html", "pdf" or "word".
#' @details If you use for loops to produce output in an Rmd file, you need to
#'   explicitly print the outputs, and how that is done can depend on the
#'   format.
#' @export
print.output_table <- function(x, format = theFormat){
  if (format == "word"){
    docx_value(x)
    cat("<w:p/>")
  } else {
    knitr:::print.knitr_kable(x)
  }
  invisible()
}

parahdr <- function(path){
  path <- file.path(path, "parahdr.tex")
  txt <- "\\usepackage{titlesec}
  \\titleformat{\\paragraph}
  {\\normalfont\\bfseries}
  {}
  {0pt}
  {}\n"
  writeLines(txt, path)
}

#' Attempt to remove temporary files
#' @param outfile String to outfile, including the full path.
#' @param ask Logical defaulting to \code{ask = FALSE}. If \code{ask = TRUE},
#'   the function promtps the user with the filenames it has found.
cleanup <- function(outfile, ask = FALSE){
  s <- strsplit(outfile, "/")[[1]]
  file <- s[length(s)]
  path <- s[-length(s)]
  path <- paste(path, collapse = "/")

  identifier <- strsplit(file, "_")[[1]]
  identifier <- identifier[length(identifier)]
  identifier <- substring(identifier, 1, nchar(identifier) - 4)

  fns <- list.files(path)
  rfns <- fns[grepl(identifier, fns)]

  if (length(rfns) > 0){
    if (ask){
      cat("Remove these files?:\n")
      cat(paste("  ", rfns, collapse = "\n"), "\n")
      ans <- readline(prompt = "> ")
    } else {
      ans <- "y"
    }

    if (tolower(ans) %in% c("y", "yes")){
      out <- file.remove(file.path(path, rfns))
    } else {
      out <- message("exiting")
    }
  } else {
    invisible()
  }
}

#' Create a table, depending on the type of output format
#' @param x A data frame. If it is a matrix, it gets turned into a data frame.
#' @param format Either "pdf", "html" or "word".
#' @param digits,row.names,escape,align,font_size,full_width Passed through (or not)
#'   to kable or flextable.
#' @details It kind of works for html and pdf, not really tested for Word. It
#'   is very limited in terms of the options available.
#' @export
output_table <- function(x, format = theFormat, digits = 3,
                         row.names = TRUE, escape = FALSE,
                         align = c("l", rep("r", ncol(x))),
                         ..., font_size = NULL, full_width = NULL){
  x <- as.data.frame(x)

  if (format == "html"){
    res <- kable(x, digits = digits, row.names = row.names, ...) %>% kable_styling(font_size = font_size)
  } else if (format == "pdf") {
    x <- as.data.frame(x, stringsAsFactors = FALSE)

    res <- kable(x, format = "latex", align = align, row.names = row.names,
                 escape = escape, digits = digits, ...) %>%
      kable_styling(font_size = font_size, full_width = full_width)
  } else if (format == "word"){
    if (row.names){
      x <- cbind(rownames(x), x)
      names(x)[1] <- " "
    }
    res <- fitFlextableToPage(flextable::flextable(as.data.frame(x), ...))
  } else {
    stop("output_format should be 'html', 'pdf' or 'word'")
  }
  class(res) <- c("output_table", class(res))
  res
}

# copied from stackoverflow
fitFlextableToPage <- function(ft, pgwidth = 6){
  ft_out <- ft %>% flextable::autofit()

  ft_out <- flextable::width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable::flextable_dim(ft_out)$widths))
  return(ft_out)
}

#' Get the Git repository commit version
#' @details Get the short HEAD from the Git commit.
#' @export
gitcv <- function (){
  system("git rev-parse --short HEAD", intern = TRUE)
}

#' Print "DRAFT" and, perhaps, the Git commit version so that the font is white
#' @param format String, either "html" or "pdf". This is a required argument
#'   because picking the information up from the render run tends to be
#'   unreliable.
#' @details If the output format is 'word', anyone can edit it and there's not
#'   much point putting the commit number in. Also, I can't figure out how to
#'   make the font white.
#' @param format A string, either "html", "pdf", or "word".
#' @export
printcv <- function(format = theFormat){
  wh <- gitcv()
  if (format == "html"){
    dr <- paste0("\n\n<div class='watermark'>DRAFT <span style='color:white; font-size:xx-small;'>",
                 gitcv(),
                 "</span></div>\n\n")
    cat(dr)
  } else if (format == "pdf") {
    dr <- paste0("\n\nDRAFT \\textcolor{white}{", wh, "}\n\n")
    cat(dr)
  } else if (format == "word"){
    cat("\n\nDRAFT\n\n")
  }

  invisible()
}

#' Reduce subsections by 1 and remove some other guff
#' @param infile String giving the name of the input file, without the path
#'   and (optionally) without the file extension.
#' @param herePath String to pass to \code{here::here} to get the correct path
#'   to the directory containing the Rmd file.
#' @param custom_removal Character vector giving strings to be replaced with
#'   empty strings. Defaults to \code{custom_removal = NULL}.
#' @export
reduceSubsectioning <- function(infile, herePath = "Rmd/", custom_removal = NULL){
  rnd <- paste(sample(c(0:9, letters, LETTERS), 9, replace = TRUE), collapse = "")

  infile <- gsub(".Rmd", "", infile)
  outfile <- paste0(infile, "_", rnd)

  inf <- here(herePath, paste0(infile, ".Rmd"))
  outf <- here(herePath, paste0(outfile, ".Rmd"))

  rl <- readr::read_lines(inf)

  rl <- gsub("\\{.tabset\\}", "", rl)
  rl <- gsub("# ", " ", rl)

  rl <- ifelse(substring(rl, 1, 4) == "<div", "", rl)

  if (!is.null(custom_removal)){
    for (i in 1:length(custom_removal)){
      rl <- gsub(custom_removal[i], "", rl)
    }
  }

  readr::write_lines(rl, outf)
  outf
}
