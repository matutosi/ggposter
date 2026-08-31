# Text-metric calls (grobHeight/grobWidth/convertWidth on text and gridtext
# grobs) need an open graphics device. Without one, R auto-opens the base
# pdf() device, which only knows the 14 standard PostScript faces and warns
# loudly for any other system font (e.g. "Arial"). Open a device that knows
# system fonts instead, matching what render_poster() actually uses.
#
# **`capabilities("cairo")` を信じてはいけない**．GitHub Actions の macOS
# ランナーは TRUE を返すのに `cairo_pdf()` が実行時に
# "failed to load cairo DLL" で落ち，基本の pdf デバイスに落ちる
# (2026-08-31 に CI のログで確認)．そこで**実際に開いて確かめる**．
cairo_works <- function() {
  if (!isTRUE(capabilities("cairo"))) return(FALSE)
  ok <- tryCatch({
    withCallingHandlers(
      { grDevices::cairo_pdf(nullfile()); identical(names(grDevices::dev.cur()), "cairo_pdf") },
      warning = function(w) invokeRestart("muffleWarning"))
  }, error = function(e) FALSE)
  if (!isTRUE(ok) && grDevices::dev.cur() != 1L) try(grDevices::dev.off(), silent = TRUE)
  isTRUE(ok)
}

CAIRO_OK <- cairo_works()   # 開いたままなら，そのデバイスを寸法の測定に使う

# cairo が駄目なら **ragg** で測る．ragg はこのパッケージが PNG 出力に使っており
# (`render_poster()`)，システムフォントも Unicode も扱えるので，
# 基本の pdf デバイスのような警告 (フォント名・`mbcsToSbcs` の記号の置換) が出ない．
if (!CAIRO_OK) {
  ragg_ok <- requireNamespace("ragg", quietly = TRUE) &&
    !inherits(try(ragg::agg_png(nullfile(), width = 200, height = 200), silent = TRUE),
              "try-error")
  if (!ragg_ok) {
    quartz_ok <- isTRUE(capabilities("aqua")) &&
      !inherits(try(grDevices::quartz(file = nullfile(), type = "pdf"), silent = TRUE),
                "try-error")
    if (!quartz_ok) grDevices::pdf(nullfile())
  }
}

# **実在のフォント名を PostScript の標準書体 (Helvetica) の metrics に割り当てる**．
# 基本の pdf デバイスに落ちたとき，text grob ごとに
# "font family 'Arial' not found in PostScript font database" が出て，
# `R CMD check` はそれを失敗として扱うため．
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

# **PDF の書き出しは cairo が要る** (`render_poster()` は `cairo_pdf()` を使う)．
# 使えない環境ではその機能自体が動かないので，該当のテストは飛ばす
# (黙って別の見た目の PDF を書くより，飛ばして理由を残すほうがよい)．
skip_if_no_cairo <- function() {
  testthat::skip_if_not(CAIRO_OK, "cairo が使えない (render_poster() の PDF 出力は cairo_pdf に依る)")
}

message("ggposter tests: cairo = ", CAIRO_OK,
        " / device = ", names(grDevices::dev.cur()),
        " / aqua = ", capabilities("aqua"))

withr::defer(grDevices::dev.off(), teardown_env())
