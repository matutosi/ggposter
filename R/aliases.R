#' Cross-tool aliases for spec keys
#'
#' ggposter, acposter (`build-poster-pdf`) and qtposter describe the same
#' poster metadata under different key names -- `authors` vs `author` vs
#' `poster-authors`, and so on. Rather than renaming ggposter's own
#' arguments (which would break every existing spec), the shared names are
#' accepted as aliases and rewritten to ggposter's own keys before a spec
#' is built. Moving a header between the three tools then needs no edits.
#'
#' Deliberately absent: a bare `size` alias for the theme's `base_size`.
#' `size` already means the *paper* size in `spec$poster`, while qtposter
#' uses it for the *font* size, so accepting it in both places would make
#' the ambiguity worse. Use `font-size` (or `base_size`) for type.
#'
#' @format A named list of character vectors, one per spec block. Names are
#'   the alias, values the ggposter key it maps to.
#' @keywords internal
#' @noRd
poster_spec_aliases <- list(
  title = c(
    author       = "authors",
    institute    = "affiliations",
    institutes   = "affiliations",
    affiliation  = "affiliations",
    note         = "funding",
    footer       = "funding"
  ),
  poster = c(
    paper = "size"
  ),
  theme = c(
    "font-size"   = "base_size",
    font_size     = "base_size",
    "font-family" = "base_family",
    font_family   = "base_family",
    "cjk-family"  = "cjk_family"
  )
)

#' Rewrite alias keys within one spec block
#'
#' @param x One block of a spec (`spec$title`, `spec$poster`, ...), or `NULL`.
#' @param aliases A named character vector: alias -> ggposter key.
#' @param block The block's name, used in messages.
#' @return `x` with every alias renamed to the key it maps to.
#' @keywords internal
#' @noRd
normalize_aliases <- function(x, aliases, block) {
  if (!is.list(x) || is.null(names(x))) return(x)
  for (from in intersect(names(x), names(aliases))) {
    to <- aliases[[from]]
    if (is.null(x[[to]])) {
      x[[to]] <- x[[from]]
    } else {
      cli::cli_warn(c(
        "{.field {block}} sets both {.field {to}} and its alias {.field {from}}.",
        "i" = "Keeping {.field {to}}; drop one of the two."
      ))
    }
    x[[from]] <- NULL
  }
  x
}

#' Accept the shared poster-tool key names in a spec
#'
#' @param spec A poster spec (already read from YAML if it was a path).
#' @return The spec with alias keys rewritten to ggposter's own keys.
#' @keywords internal
#' @noRd
normalize_spec <- function(spec) {
  for (block in names(poster_spec_aliases)) {
    spec[[block]] <- normalize_aliases(spec[[block]], poster_spec_aliases[[block]], block)
  }
  spec
}
