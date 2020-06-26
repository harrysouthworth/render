render_word <- function(infile, herePath = "Rmd/", keep = FALSE, render = TRUE,
                       output_file = NULL, keep_tex = FALSE,
                       custom_removal = NULL,
                       toc = TRUE, toc_depth = 3,
                       cleanup_ask = FALSE){
  here <- here::here
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

  if (!keep && !keep_tex){
    on.exit(cleanup(rnd, path = here::here(herePath), ask = cleanup_ask), add = TRUE)
  }

  if (render){
    render(outf, output_format = word_document(toc = toc, toc_depth = toc_depth),
           output_file = output_file)
    docfile <- gsub("\\.Rmd", "\\.docx", inf)
    invisible(try(file.rename(gsub("\\.Rmd", "\\.docx", outf), docfile), silent = TRUE))
  }

  invisible(rl)
}
