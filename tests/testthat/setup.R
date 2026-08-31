# Text-metric calls (grobHeight/grobWidth/convertWidth on text and gridtext
# grobs) need an open graphics device. Without one, R auto-opens the base
# pdf() device, which only knows the 14 standard PostScript faces and warns
# loudly for any other system font (e.g. "Arial"). Open a device that knows
# system fonts instead, matching what render_poster() actually uses.
#
# **The device has to be chosen at run time.** cairo_pdf() is the one we want,
# but a macOS build of R is often compiled without cairo, so fall back to
# quartz and then to pdf().
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

# **実在のフォント名を PostScript の標準書体 (Helvetica) の metrics に割り当てる**．
# セッションのどこかで基本の pdf デバイスが開かれると，text grob ごとに
# "font family 'Arial' not found in PostScript font database" が出て，
# `R CMD check` はそれを失敗として扱う (2026-08-31，GitHub Actions の macOS で発生)．
# 割り当てておけば，どのデバイスが開かれても警告は出ない．
# **寸法は近似になるが，テストが見ているのは「箱の高さが中身に従うか」といった
# 相対的な関係**なのでそれで足りる．
local({
  helvetica <- grDevices::pdfFonts()$Helvetica
  families <- unique(c(poster_font(), poster_font(cjk = TRUE), "Arial", "Helvetica"))
  for (f in families[nzchar(families)]) {
    args <- list(helvetica)
    names(args) <- f
    try(do.call(grDevices::pdfFonts, args), silent = TRUE)
  }
})

# どのデバイスで測ったかは，CI のログを読むときに効く (Windows は cairo_pdf，
# macOS のランナーは cairo が無いことがある)．
message("ggposter tests: metric device = ", device,
        " / cairo = ", capabilities("cairo"),
        " / aqua = ", capabilities("aqua"))

withr::defer(grDevices::dev.off(), teardown_env())
