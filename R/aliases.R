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
    "poster-authors" = "authors",
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
    font          = "base_family",
    "font-family" = "base_family",
    font_family   = "base_family",
    "cjk-family"  = "cjk_family"
  )
)

#' Top-level keys that belong to a block
#'
#' acposter and qtposter headers are *flat*: `author`, `paper` and
#' `font-size` all sit at the top level of the YAML. ggposter groups the
#' same information into `title`, `poster` and `theme` blocks. Rather than
#' making that difference something to hand-edit on every move, a spec may
#' write these keys at the top level and they are folded into the block
#' they belong to. The nested form keeps working unchanged, and the two can
#' be mixed (the block wins, with a warning, if both set the same key).
#'
#' Deliberately absent again: a bare `size`. qtposter means type size by
#' it and ggposter means paper, so it is never promoted; write `font-size`
#' or `paper`.
#'
#' @format A named list of character vectors: block name -> top-level keys.
#' @keywords internal
#' @noRd
poster_flat_keys <- list(
  title = c("subtitle", "author", "authors", "poster-authors",
            "institute", "institutes", "affiliation", "affiliations",
            "note", "funding", "footer", "logo"),
  poster = c("paper", "orientation"),
  theme = c("font-size", "font_size", "font", "font-family", "font_family",
            "cjk-family", "cjk_family", "accent")
)

#' Fold a flat (acposter/qtposter-style) header into ggposter's blocks
#'
#' @param spec A poster spec.
#' @return The spec with top-level metadata keys moved into `title`,
#'   `poster` and `theme`.
#' @keywords internal
#' @noRd
promote_flat_keys <- function(spec) {
  # `title: "..."` (a plain string, as the other two tools write it) is the
  # poster's title; `title: list(...)` is ggposter's own title block.
  if (!is.null(spec$title) && !is.list(spec$title)) {
    spec$title <- list(title = spec$title)
  }
  for (block in names(poster_flat_keys)) {
    for (key in poster_flat_keys[[block]]) {
      if (is.null(spec[[key]])) next
      if (!is.list(spec[[block]])) spec[[block]] <- list()
      if (is.null(spec[[block]][[key]])) {
        spec[[block]][[key]] <- spec[[key]]
      } else {
        cli::cli_warn(c(
          "{.field {key}} is set both at the top level and inside {.field {block}}.",
          "i" = "Keeping the one inside {.field {block}}; drop one of the two."
        ))
      }
      spec[[key]] <- NULL
    }
  }
  spec
}

#' Turn a plain column count into an equal-share `layout`
#'
#' acposter and qtposter lay a poster out by column *count* alone
#' (`columns: 2`), flowing the cards down the first column and on into the
#' next. ggposter names its columns instead, so a top-level `columns` is
#' expanded into that same left-to-right flow: the sections, in the order
#' they are written, split into `columns` contiguous groups of as near to
#' equal length as they divide.
#'
#' @param section_names Section names, in spec order.
#' @param columns Number of columns.
#' @return A named list of character vectors, one per column.
#' @keywords internal
#' @noRd
flow_layout <- function(section_names, columns) {
  n <- suppressWarnings(as.integer(columns))
  if (length(n) != 1 || is.na(n) || n < 1) {
    cli::cli_abort("{.field columns} must be a single positive integer, not {.val {columns}}.")
  }
  # Left column first, as in a newspaper: the leftmost columns take the
  # remainder, so a column is never left empty while a later one is filled.
  sizes <- rep(length(section_names) %/% n, n)
  rem <- length(section_names) %% n
  if (rem > 0) sizes[seq_len(rem)] <- sizes[seq_len(rem)] + 1
  ends <- cumsum(sizes)
  out <- lapply(seq_len(n), function(i) {
    section_names[seq_len(sizes[i]) + ends[i] - sizes[i]]
  })
  stats::setNames(out, paste0("col", seq_len(n)))
}

#' Collapse a list of names into the one string ggposter draws
#'
#' acposter and qtposter take `author` and `institute` as lists; ggposter's
#' [poster_title()] draws one already-formatted line per row. A list is
#' joined with `", "` so the same header renders in all three rather than
#' failing on a vector where a single string was expected.
#'
#' @param x A spec value.
#' @return `x` as a single string if it arrived as a list/vector of them.
#' @keywords internal
#' @noRd
collapse_title_row <- function(x) {
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  if (is.character(x) && length(x) > 1) paste(x, collapse = ", ") else x
}

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
    if (identical(from, to)) next        # a key that is already its own name

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
  spec <- promote_flat_keys(spec)
  for (block in names(poster_spec_aliases)) {
    spec[[block]] <- normalize_aliases(spec[[block]], poster_spec_aliases[[block]], block)
  }
  for (key in c("title", "subtitle", "authors", "affiliations", "funding")) {
    if (!is.null(spec$title[[key]])) {
      spec$title[[key]] <- collapse_title_row(spec$title[[key]])
    }
  }
  if (!is.null(spec$columns) || !is.null(spec$cols)) {
    columns <- spec$columns %||% spec$cols
    spec$columns <- NULL
    spec$cols <- NULL
    if (!is.null(spec$layout) || !is.null(spec$grid)) {
      cli::cli_warn(c(
        "{.field columns} is set alongside {.field layout}/{.field grid}.",
        "i" = "{.field columns} only builds a layout when neither is given; ignoring it."
      ))
    } else {
      spec$layout <- flow_layout(names(spec$sections), columns)
    }
  }
  spec
}
