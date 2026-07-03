#' Build a poster from a declarative spec
#'
#' Assembles a title band and a multi-column body of section cards into a
#' single [patchwork] object sized for a real paper size (default A1
#' portrait). The spec can be an R list (built by hand, or by reading YAML
#' with [read_poster_yaml()]) or a path to a YAML file, in which case it is
#' read automatically.
#'
#' See the package vignette for the full spec schema. In short:
#' \describe{
#'   \item{`poster`}{`size` (e.g. `"A1"`) and `orientation`.}
#'   \item{`theme`}{Arguments passed to [poster_theme()].}
#'   \item{`title`}{Arguments passed to [poster_title()].}
#'   \item{`layout`}{`columns`, `left`, `right`: which section names go in
#'     which column, top to bottom.}
#'   \item{`sections`}{Named list; each has `header`, optional relative
#'     `height`, and a `body` of `type` `"text"`, `"table"`, `"figure"`, or
#'     `"image"` plus that type's arguments.}
#' }
#'
#' @param spec An R list following the schema above, or a path to a YAML file.
#' @param objects Named list of R objects (ggplots, data.frames, grobs)
#'   referenced from `sections[[.]]$body$object` in the spec.
#' @param theme A [poster_theme()] object. If `NULL` (default), it is built
#'   from `spec$theme`.
#' @param base_dir Directory used to resolve relative image paths in the spec.
#'   Defaults to the YAML file's directory, or the working directory for a
#'   list spec.
#'
#' @return An object of class `ggposter`, a thin wrapper around a
#'   [patchwork] object. Print it to preview, or pass it to
#'   [render_poster()] to save a PDF/PNG at true size.
#' @export
#' @examples
#' spec <- list(
#'   title = list(title = "Example poster"),
#'   layout = list(left = "intro", right = "outro"),
#'   sections = list(
#'     intro = list(header = "INTRO", body = list(type = "text", md = "- Hello")),
#'     outro = list(header = "OUTRO", body = list(type = "text", md = "- Bye"))
#'   )
#' )
#' p <- poster(spec)
poster <- function(spec, objects = list(), theme = NULL, base_dir = NULL) {
  if (is.character(spec)) {
    if (is.null(base_dir)) base_dir <- dirname(spec)
    spec <- read_poster_yaml(spec)
  }
  if (is.null(base_dir)) base_dir <- "."
  if (is.null(theme)) theme <- do.call(poster_theme, spec$theme %||% list())

  size_mm <- poster_size(spec$poster$size %||% "A1", spec$poster$orientation %||% "portrait")

  title_grob <- if (!is.null(spec$title)) {
    do.call(poster_title, c(spec$title, list(theme = theme, width = size_mm[["width"]] - 20)))
  } else {
    NULL
  }

  layout <- spec$layout %||% list(left = names(spec$sections), right = character(0))
  columns <- layout[setdiff(names(layout), "columns")]
  columns <- columns[lengths(columns) > 0]
  col_width_mm <- size_mm[["width"]] / max(length(columns), 1)

  col_grobs <- lapply(columns, function(section_names) {
    build_column(section_names, spec$sections, theme, objects, base_dir, col_width_mm)
  })

  body <- Reduce(function(a, b) a | b, col_grobs)

  full <- if (!is.null(title_grob)) {
    title_h <- grid::convertHeight(sum(title_grob$heights), "mm", valueOnly = TRUE)
    body_h  <- size_mm[["height"]] - title_h
    patchwork::wrap_elements(full = title_grob) /
      body +
      patchwork::plot_layout(heights = c(title_h, body_h))
  } else {
    body
  }

  structure(
    list(patchwork = full, theme = theme, size_mm = size_mm, spec = spec,
        objects = objects, base_dir = base_dir),
    class = "ggposter"
  )
}

#' Build one column of stacked section cards
#' @param section_names Character vector of section names, top to bottom.
#' @param sections Named list of section specs (`spec$sections`).
#' @param theme A [poster_theme()] object.
#' @param objects Named list of R objects referenced by section bodies.
#' @param base_dir Directory used to resolve relative image paths.
#' @param col_width_mm Column width in millimetres, used as the default text
#'   wrap width when a section body does not specify one.
#' @return A [patchwork] object stacking the column's cards.
#' @keywords internal
#' @noRd
build_column <- function(section_names, sections, theme, objects, base_dir, col_width_mm) {
  cards <- lapply(section_names, function(nm) {
    build_section(nm, sections[[nm]], theme, objects, base_dir, col_width_mm)
  })
  heights <- vapply(section_names, function(nm) sections[[nm]]$height %||% 1, numeric(1))
  wrapped <- lapply(cards, patchwork::wrap_elements)
  Reduce(function(a, b) a / b, wrapped) + patchwork::plot_layout(heights = heights)
}

#' Build a single section card from its spec
#' @param name Section name (used only for error messages).
#' @param section A single section spec: `list(header=, height=, body=list(type=, ...))`.
#' @inheritParams build_column
#' @return A [poster_card()] grob.
#' @keywords internal
#' @noRd
build_section <- function(name, section, theme, objects, base_dir, col_width_mm) {
  if (is.null(section)) {
    cli::cli_abort("Section {.val {name}} is referenced in {.field layout} but missing from {.field sections}.")
  }
  body <- build_body(section$body, theme, objects, base_dir, col_width_mm)
  poster_card(body, header = section$header, theme = theme)
}

#' Dispatch a section's `body` spec to the matching content builder
#' @param body A body spec: `list(type = "text"|"table"|"figure"|"image", ...)`.
#' @inheritParams build_column
#' @return A grob (or ggplot, for `"figure"`) ready for [poster_card()].
#' @keywords internal
#' @noRd
build_body <- function(body, theme, objects, base_dir, col_width_mm) {
  type <- body$type %||% "text"
  switch(type,
    # Text wraps relative to its card ("npc", the default in card_text())
    # unless the spec gives an explicit absolute width: gridtext's line
    # breaking is unreliable at the very small absolute point sizes that
    # come out of a heavily scaled-down preview (see render_poster(scale=)
    # / rescale_poster()), so tying the wrap width to a computed millimetre
    # value (which looked fine at true size) actually made small previews
    # worse, not better.
    text = card_text(body$md, theme, width = body$width),
    table = card_table(objects[[body$object]], theme,
                       title = body$title, caption = body$caption,
                       width = (body$width %||% col_width_mm) -
                         2 * grid::convertWidth(theme$pad, "mm", valueOnly = TRUE)),
    figure = card_figure(objects[[body$object]], theme,
                        width = body$width, height = body$height),
    image = card_image(file.path(base_dir, body$files), labels = body$labels,
                       theme = theme, height = body$height %||% 60),
    cli::cli_abort("Unknown body type {.val {type}}.")
  )
}

#' @export
print.ggposter <- function(x, ...) {
  # Font sizes and any explicit mm width/height in the spec are absolute
  # physical measurements sized for the poster's true paper size (e.g.
  # 594x841mm for A1). Printing/auto-printing (RStudio's plot pane, a knitr
  # chunk, etc.) draws onto whatever device is already open, which is
  # essentially never that exact physical size -- so without rescaling,
  # text and photos would overflow their cards. Fit to the active device's
  # physical size instead, the same way render_poster(scale=) fits to a
  # requested output size.
  dims <- tryCatch(grDevices::dev.size("mm"), error = function(e) NULL)
  if (!is.null(dims) && all(is.finite(dims)) && all(dims > 0)) {
    scale <- min(dims[[1]] / x$size_mm[["width"]], dims[[2]] / x$size_mm[["height"]])
    if (is.finite(scale) && scale > 0 && abs(scale - 1) > 1e-6) {
      x <- rescale_poster(x, scale)
    }
  }
  print(x$patchwork, ...)
  invisible(x)
}

#' Rebuild a poster at a different scale
#'
#' Font sizes and any explicit millimetre widths/heights in the spec (e.g. a
#' `card_image` `height`, or a `card_figure` `width`) are absolute physical
#' measurements, sized for the poster's true paper size. Simply asking
#' [render_poster()] for a smaller output canvas (`scale < 1`) without also
#' shrinking those measurements would make text and photos overflow their
#' cards. This rebuilds the whole poster with the paper size, theme font
#' size/spacing, and any explicit body `width`/`height` overrides all
#' multiplied by `scale`, so a scaled-down preview looks proportionally
#' identical to the true-size version.
#'
#' @param x A `ggposter` object, as returned by [poster()].
#' @param scale Multiplier applied to paper size, theme, and explicit body
#'   dimensions.
#' @return A new `ggposter` object at the scaled size.
#' @keywords internal
#' @noRd
rescale_poster <- function(x, scale) {
  theme <- x$theme
  mm <- function(u) grid::convertWidth(u, "mm", valueOnly = TRUE) * scale
  theme$base_size     <- theme$base_size * scale
  theme$border_width  <- theme$border_width * scale
  theme$pad           <- as_mm_unit(mm(theme$pad))
  theme$gap           <- as_mm_unit(mm(theme$gap))
  theme$corner_radius <- as_mm_unit(mm(theme$corner_radius))

  spec <- x$spec
  spec$poster$size        <- unname(x$size_mm) * scale
  spec$poster$orientation <- "portrait"  # size_mm already reflects the original orientation
  spec$sections <- lapply(spec$sections, function(section) {
    if (!is.null(section$body$width))  section$body$width  <- section$body$width  * scale
    if (!is.null(section$body$height)) section$body$height <- section$body$height * scale
    section
  })

  poster(spec, objects = x$objects, theme = theme, base_dir = x$base_dir)
}
