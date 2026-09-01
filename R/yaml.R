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

# YAML 1.1 の真偽値は `y` だけではない．**引用符の無い `y`/`n`/`on`/`off`/
# `yes`/`no`/`true`/`false` は，キーに書くと `"TRUE"`/`"FALSE"` になる**．
# `grid$boxes` の `y` は restore_y_key() で元に戻せる (そこに来るキーが
# `y` しかないと分かっているため) が，一般には**どの綴りだったか復元できない**
# (`on` も `yes` も同じ `"TRUE"` になる)．黙って別のキーとして扱うより，
# **どこが壊れたかを示して止める**．
yaml11_bad_keys <- function(x, trail = "") {
  if (!is.list(x)) return(character(0))
  nms <- names(x)
  out <- character(0)
  for (i in seq_along(x)) {
    key  <- if (!is.null(nms) && nzchar(nms[[i]])) nms[[i]] else paste0("[[", i, "]]")
    here <- if (nzchar(trail)) paste(trail, key, sep = "$") else key
    if (!is.null(nms) && nms[[i]] %in% c("TRUE", "FALSE")) out <- c(out, here)
    out <- c(out, yaml11_bad_keys(x[[i]], here))
  }
  out
}

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
read_poster_yaml <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("YAML file not found: {.path {path}}")
  }
  spec <- yaml::read_yaml(path)
  spec <- restore_y_key(spec)
  bad <- yaml11_bad_keys(spec)
  if (length(bad)) {
    cli::cli_abort(c(
      "{cli::qty(length(bad))} Key{?s} in {.path {path}} came back as {.val TRUE}/{.val FALSE} instead of {cli::qty(length(bad))} {?a name/names}: {.val {bad}}.",
      "i" = "R's YAML parser follows YAML 1.1, where a bare {.code y}, {.code n}, {.code on}, {.code off}, {.code yes} or {.code no} is a boolean.",
      "i" = "Quote the key -- {.code 'on':} rather than {.code on:} -- so it stays a name."
    ))
  }
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
