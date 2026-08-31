#' Read a poster spec from a YAML file
#'
#' Parses a YAML file into the list structure expected by [poster()]. Only
#' does light validation (presence of `sections` and one of `layout`,
#' `grid` or `columns`); the content builders raise their own errors for
#' malformed section bodies.
#'
#' @param path Path to a YAML file.
#' @return A list following the [poster()] spec schema.
#' @export
#' @examples
#' path <- system.file("extdata", "poster_sample.yml", package = "ggposter")
#' spec <- read_poster_yaml(path)
# `grid:` の `boxes` に書いた `y` を取り戻す．
#
# **R の yaml パッケージは YAML 1.1 なので，引用符の無い `y` を真偽値とみなし，
# キーが `"TRUE"` になる** (2026-08-31 に実機で確認．`x` は無事なので気づきにくい)．
# 姉妹ツール (acposter・qtposter) は YAML 1.2 の経路なので `y` のまま通り，
# **同じヘッダーがここでだけ落ちていた**．`'y'` と引用符で書けば3つとも通るが，
# 引用符の無い書き方も受けられるよう，読み込んだ直後にキー名を戻す．
restore_y_key <- function(spec) {
  boxes <- spec$grid$boxes
  if (is.null(boxes)) return(spec)
  spec$grid$boxes <- lapply(boxes, function(b) {
    if (is.list(b) && "TRUE" %in% names(b) && is.null(b[["y"]])) {
      names(b)[names(b) == "TRUE"] <- "y"
    }
    b
  })
  spec
}

read_poster_yaml <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("YAML file not found: {.path {path}}")
  }
  spec <- yaml::read_yaml(path)
  spec <- restore_y_key(spec)
  if (is.null(spec$sections)) {
    cli::cli_abort("Poster YAML must have a top-level {.field sections} map.")
  }
  if (is.null(spec$layout) && is.null(spec$grid) &&
      is.null(spec$columns) && is.null(spec$cols)) {
    cli::cli_abort(c(
      "Poster YAML must say how the sections are laid out.",
      "i" = "Use {.field layout} (a map of column name to section names), {.field grid} (with {.field columns}/{.field boxes}), or a plain {.field columns} count."
    ))
  }
  spec
}
