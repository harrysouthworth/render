render_word <- function(infile, herePath = "Rmd/", keep = FALSE, render = TRUE,
                       output_file = NULL, keep_tex = FALSE,
                       custom_removal = NULL,
                       toc = TRUE, toc_depth = 3,
                       cleanup_ask = FALSE){

  outf <- reduceSubsectioning(infile, herePath)

  if (!keep && !keep_tex){
    on.exit(cleanup(outf, path = here::here(herePath), ask = cleanup_ask), add = TRUE)
  }

  render(outf, output_format = word_document(toc = toc, toc_depth = toc_depth),
         output_file = output_file)
  docfile <- gsub("\\.Rmd", "\\.docx", outf)
  invisible(try(file.rename(gsub("\\.Rmd", "\\.docx", outf), docfile), silent = TRUE))

  invisible(outf)
}
