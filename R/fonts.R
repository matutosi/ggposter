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
