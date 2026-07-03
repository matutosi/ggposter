# Text-metric calls (grobHeight/grobWidth/convertWidth on text and gridtext
# grobs) need an open graphics device. Without one, R auto-opens the base
# pdf() device, which only knows a handful of standard PostScript font names
# and warns loudly for any other system font (e.g. "Arial"). Open a cairo_pdf
# device for the whole test session instead, matching what render_poster()
# actually uses in real output.
grDevices::cairo_pdf(nullfile())

withr::defer(grDevices::dev.off(), teardown_env())
