# Text-metric calls (grobHeight/grobWidth/convertWidth on text and gridtext
# grobs) need an open graphics device. Without one, R auto-opens the base
# pdf() device, which only knows the 14 standard PostScript faces and warns
# loudly for any other system font (e.g. "Arial"). Open a device that knows
# system fonts instead, matching what render_poster() actually uses.
#
# **The device has to be chosen at run time.** cairo_pdf() is the one we want,
# but a macOS build of R is often compiled without cairo -- the GitHub Actions
# macOS runner has neither cairo nor a usable quartz, so the tests fall back to
# pdf() there and emitted "font family 'Arial' not found in PostScript font
# database" for every text grob, which R CMD check turns into a failure
# (2026-08-31).
open_metric_device <- function() {
  if (isTRUE(capabilities("cairo"))) {
    grDevices::cairo_pdf(nullfile())
    return(invisible("cairo_pdf"))
  }
  ok <- isTRUE(capabilities("aqua")) &&
    !inherits(try(grDevices::quartz(file = nullfile(), type = "pdf"), silent = TRUE),
              "try-error")
  if (ok) return(invisible("quartz"))
  grDevices::pdf(nullfile())
  invisible("pdf")
}

device <- open_metric_device()

# 基本の pdf デバイスに落ちたときは，実在のフォント名を PostScript の
# 標準書体 (Helvetica) の metrics に割り当てておく．**寸法は近似になるが，
# テストが見ているのは「箱の高さが中身に従うか」といった相対的な関係**なので
# それで足りる．割り当てないと text grob ごとに警告が出て，check が落ちる．
if (identical(device, "pdf")) {
  helvetica <- grDevices::pdfFonts()$Helvetica
  families <- unique(c(poster_font(), poster_font(cjk = TRUE), "Arial"))
  for (f in families[nzchar(families)]) {
    args <- list(helvetica)
    names(args) <- f
    try(do.call(grDevices::pdfFonts, args), silent = TRUE)
  }
}

withr::defer(grDevices::dev.off(), teardown_env())
