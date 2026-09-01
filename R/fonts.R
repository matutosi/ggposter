#' Find an available font family for a poster
#'
#' Chooses a font family that is actually installed on the system, so that
#' output renders (and embeds into PDF) without falling back to a substitute.
#' Latin text prefers clean sans-serif faces; CJK text prefers a Japanese
#' capable family.
#'
#' @param cjk If `TRUE`, look for a CJK (Japanese) capable family.
#' @param preferred Character vector of family names to try, in order. If `NULL`,
#'   a built-in list of common families is used.
#' @param fallback Family name to return when none of `preferred` is installed.
#'   Defaults to the empty string, which lets the graphics device pick.
#'
#' @return A single font family name (character scalar).
#' @export
#' @examples
#' poster_font()
#' poster_font(cjk = TRUE)
poster_font <- function(cjk = FALSE, preferred = NULL, fallback = "") {
  if (is.null(preferred)) {
    preferred <- if (cjk) {
      c("Noto Sans JP", "Noto Sans CJK JP", "Yu Gothic", "Meiryo",
        "MS Gothic", "Hiragino Sans", "Source Han Sans")
    } else {
      c("Noto Sans", "Arial", "Helvetica", "DejaVu Sans", "Segoe UI",
        "Liberation Sans")
    }
  }
  installed <- poster_font_families()
  hit <- preferred[preferred %in% installed]
  if (length(hit)) hit[[1]] else fallback
}

#' Names of installed font families
#' @return Character vector of family names available on the system.
#' @keywords internal
#' @noRd
poster_font_families <- function() {
  fam <- tryCatch(unique(systemfonts::system_fonts()$family),
                  error = function(e) character(0))
  fam
}

#' Register a font so its exact file is used by graphics devices
#'
#' Thin convenience wrapper over [systemfonts::register_font()]; useful when a
#' specific TTF/OTF must be embedded (e.g. a lab-mandated typeface).
#'
#' @param family Name to register the font under.
#' @param plain,bold,italic,bolditalic Paths to font files for each face. Only
#'   `plain` is required.
#' @return Invisibly, the registered family name.
#' @export
poster_register_font <- function(family, plain, bold = plain,
                                 italic = plain, bolditalic = plain) {
  systemfonts::register_font(
    name = family,
    plain = plain, bold = bold, italic = italic, bolditalic = bolditalic
  )
  invisible(family)
}

#' Does a string contain CJK characters?
#'
#' Covers the ranges a poster realistically carries: CJK symbols and
#' punctuation, hiragana/katakana, the two main Han blocks, compatibility
#' ideographs, and the fullwidth/halfwidth forms.
#'
#' @param x A character vector (or anything coercible to one).
#' @return `TRUE` if any element contains a CJK character.
#' @keywords internal
#' @noRd
has_cjk <- function(x) {
  if (is.null(x) || !length(x)) return(FALSE)
  x <- as.character(x)
  x <- x[!is.na(x)]
  if (!length(x)) return(FALSE)
  # Written as ASCII \u escapes rather than the characters themselves:
  # R CMD check requires a portable package to keep its R *code* ASCII.
  pattern <- paste0("[\u3000-\u303F\u3040-\u309F\u30A0-\u30FF",
                    "\u3400-\u4DBF\u4E00-\u9FFF\uF900-\uFAFF\uFF00-\uFFEF]")
  any(grepl(pattern, x, perl = TRUE))
}

#' Pick the font family that can actually draw a string
#'
#' `theme$cjk_family` used to be stored, printed and accepted as an alias
#' but never reached a single `gpar()`: every text grob was drawn in
#' `base_family`, so Japanese text on a poster with a Latin `base_family`
#' fell back to whatever substitute the device chose. Each piece of text is
#' now drawn in `cjk_family` when it actually contains CJK characters, and
#' in `base_family` otherwise -- grid has no per-script fallback within a
#' single `gpar()`, so the choice has to be made per grob.
#'
#' @param x The text about to be drawn.
#' @param theme A [poster_theme()] object.
#' @return A font family name (character scalar).
#' @keywords internal
#' @noRd
text_family <- function(x, theme) {
  if (!has_cjk(x)) return(theme$base_family)
  cjk <- theme$cjk_family
  if (is.null(cjk) || !nzchar(cjk)) theme$base_family else cjk
}
