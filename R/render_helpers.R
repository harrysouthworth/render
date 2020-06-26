#' Print table to HTML, PDF or Word from within a for loop
#' @param x The output of \code{output_table}.
#' @param format String: either "html", "pdf" or "word".
#' @details If you use for loops to produce output in an Rmd file, you need to
#'   explicitly print the outputs, and how that is done can depend on the
#'   format.
print.output_table <- function(x, format = theFormat){
  if (format == "word"){
    docx_value(x)
    cat("<w:p/>")
  } else {
    print(x)
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

cleanup <- function(identifier, path, ask = FALSE){
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
    invisible(out)
  }
}

#' Create a table, depending on the type of output format
#' @param x A data frame. If it is a matrix, it gets turned into a data frame.
#' @param format Either "pdf", "html" or "word".
#' @param digits Have a guess.
#' @details It kind of works for html and pdf, not really tested for Word. It
#'   is very limited in terms of the options available.
#' @export
output_table <- function(x, format = theFormat, digits = 3,
                         row.names = TRUE,
                         align = c("l", rep("r", ncol(x))),
                         ..., font_size = NULL, full_width = NULL){
  x <- as.data.frame(x)

  if (format == "html"){
    res <- kable(x, digits = digits, row.names = row.names, ...) %>% kable_styling(font_size = font_size)
  } else if (format == "pdf") {
    x <- as.data.frame(x, stringsAsFactors = FALSE)

    res <- kable(x, format = "latex", align = align, row.names = row.names,
          digits = digits, ...) %>%
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
  ft_out <- ft %>% autofit()

  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
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
