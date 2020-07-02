render_word <- function(infile, herePath = "Rmd/", keep = FALSE, render = TRUE,
                       output_file = NULL, keep_tex = FALSE,
                       custom_removal = NULL,
                       toc = TRUE, toc_depth = 3,
                       cleanup_ask = FALSE){

  outf <- reduceSubsectioning(infile, herePath)

  if (!keep && !keep_tex){
    on.exit(cleanup(outf, ask = cleanup_ask), add = TRUE)
  }

  render(outf, output_format = word_document(toc = toc, toc_depth = toc_depth),
         output_file = output_file)

  docfile <- paste0(substring(outf, 1, nchar(outf) - 14), ".docx")
  invisible(try(file.rename(gsub("\\.Rmd", "\\.pdf", outf), docfile), silent = TRUE))

  invisible(outf)
}
