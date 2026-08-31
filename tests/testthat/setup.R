# Text-metric calls (grobHeight/grobWidth/convertWidth on text and gridtext
# grobs) need an open graphics device. Without one, R auto-opens the base
# pdf() device, which only knows a handful of standard PostScript font names
# and warns loudly for any other system font (e.g. "Arial"). Open a device
# that knows system fonts instead, matching what render_poster() actually
# uses in real output.
#
# **The device has to be chosen at run time.** cairo_pdf() is the one we want,
# but a macOS build of R is often compiled without cairo -- on the GitHub
# Actions macOS runner it is, and the tests then fell back to pdf() and
# emitted "font family 'Arial' not found in PostScript font database" for
# every text grob, which R CMD check turns into a failure (2026-08-31).
# So: cairo first, then quartz (macOS), and pdf() only as a last resort.
open_metric_device <- function() {
  if (isTRUE(capabilities("cairo"))) {
    grDevices::cairo_pdf(nullfile())
    return(invisible("cairo_pdf"))
  }
  if (isTRUE(capabilities("aqua"))) {
    grDevices::quartz(file = nullfile(), type = "pdf")
    return(invisible("quartz"))
  }
  grDevices::pdf(nullfile())
  invisible("pdf")
}

open_metric_device()

withr::defer(grDevices::dev.off(), teardown_env())
