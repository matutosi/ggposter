#' Build a poster from a declarative spec
#'
#' Assembles a title band and a multi-column body of section cards into a
#' single [gtable::gtable] sized for a real paper size (default A1
#' portrait). The spec can be an R list (built by hand, or by reading YAML
#' with [read_poster_yaml()]) or a path to a YAML file, in which case it is
#' read automatically.
#'
#' See the package vignette for the full spec schema. In short:
#' \describe{
#'   \item{`poster`}{`size` (e.g. `"A1"`) and `orientation`.}
#'   \item{`theme`}{Arguments passed to [poster_theme()].}
#'   \item{`title`}{Arguments passed to [poster_title()].}
#'   \item{`layout`}{`columns`, `left`, `right` (or any other column names):
#'     which section names go in which column, top to bottom. Set
#'     `align_rows: true` to line up each row position across columns to
#'     the tallest `"auto"`-height card at that position, instead of each
#'     column stacking at its own height. Every section must appear
#'     exactly once across all columns.}
#'   \item{`grid`}{An alternative to `layout` for irregular (CSS-Grid-like)
#'     arrangements where a card spans multiple columns and/or rows:
#'     `columns` (integer) and `boxes` (a list of `list(name=, x=, y=, w=,
#'     h=)`, `x`/`y` 0-based, `w`/`h` defaulting to `1`). Row heights follow
#'     the same `height`/`"auto"` rule as `layout`, but resolved per row
#'     from whichever boxes in that row don't span multiple rows; a
#'     spanning box never stretches the rows/columns it spans, it is
#'     top-left anchored at its own natural size within them. Overlapping
#'     or out-of-bounds boxes, and any section not placed in exactly one
#'     box, raise an error; content taller than the page raises a warning.
#'     If both `layout` and `grid` are set, `grid` wins (with a warning).}
#'   \item{`sections`}{Named list; each has `header`, optional relative
#'     `height`, and a `body` of `type` `"text"`, `"table"`, `"figure"`, or
#'     `"image"` plus that type's arguments.}
#' }
#'
#' A spec may also be written the way the sibling tools acposter
#' (`build-poster-pdf`) and qtposter write a header -- **flat**, with the
#' metadata at the top level rather than inside `title`/`poster`/`theme`
#' -- so that one header serves all three unchanged:
#'
#' ```yaml
#' title: "A poster"
#' author: ["A. One", "B. Two"]     # authors, poster-authors
#' institute: "Somewhere Univ."     # institutes, affiliation(s)
#' note: "Funded by X"              # funding, footer
#' paper: A1                        # -> poster$size
#' orientation: portrait
#' columns: 2                       # -> an equal-share layout
#' font-size: 20                    # -> theme$base_size
#' font: "Noto Sans"                # -> theme$base_family
#' ```
#'
#' Each key is folded into the block it belongs to and renamed to
#' ggposter's own: `author`/`authors`/`poster-authors` -> `title$authors`,
#' `institute`/`institutes`/`affiliation(s)` -> `title$affiliations`,
#' `note`/`funding`/`footer` -> `title$funding`, `paper` -> `poster$size`,
#' `font-size`/`font`/`cjk-family` ->
#' `theme$base_size`/`base_family`/`cjk_family`. A list of authors or
#' affiliations (as the other two tools write them) is joined into the one
#' line [poster_title()] draws. Setting the same thing twice -- a key and
#' its alias, or the flat and the nested form -- keeps ggposter's own and
#' warns. Nested specs are untouched, and the two forms can be mixed.
#'
#' A bare `size` is *not* accepted: qtposter means type size by it and a
#' spec here means paper, so write `font-size` or `paper`.
#'
#' A top-level `columns` count (as `cols` too) stands in for `layout` when
#' neither `layout` nor `grid` is given: the sections flow down the
#' leftmost column and on into the next, in spec order, split as evenly as
#' they divide. Given alongside `layout`/`grid` it is ignored, with a
#' warning.
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
#'   [gtable::gtable]. Print it to preview, or pass it to
#'   [render_poster()] to save a PDF/PNG at true size. To inspect the layout
#'   with each card's content area outlined, pass `show_plot_area = TRUE` to
#'   [render_poster()] -- it's an output option, so the same poster object
#'   renders both the normal and the outlined version.
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
  build_poster(spec, objects = objects, theme = theme, base_dir = base_dir,
               show_plot_area = FALSE)
}

#' Build a ggposter object, optionally with plot-area outlines
#'
#' The implementation behind [poster()]. Kept separate so that
#' [render_poster()] can rebuild a poster with `show_plot_area = TRUE`
#' without exposing that inspection-only overlay on [poster()]'s own
#' (content/layout) API.
#'
#' @inheritParams poster
#' @param show_plot_area If `TRUE`, draw a dashed border around every
#'   section's body cell -- the area its figure/table/text/image content
#'   actually occupies -- on top of the content, without hiding it. See
#'   [poster_card()].
#' @return An object of class `ggposter`.
#' @keywords internal
#' @noRd
build_poster <- function(spec, objects = list(), theme = NULL, base_dir = NULL,
                         show_plot_area = FALSE) {
  # "auto"-height cards (build_column()) and the title band both need a
  # concrete numeric mm height at this point, resolved by measuring text
  # against whatever device is current right now. render_poster() doesn't
  # open its real output device until later, so without a device already
  # open, that measurement falls back to R's generic default (e.g. a bare
  # postscript font substitute) -- narrower on average than the real font,
  # which can wrap a paragraph into fewer lines than it actually needs and
  # underestimate the card's height enough for the next card to overlap
  # it. Measuring against a real ragg device (the same backend
  # render_poster() uses for PNG output) keeps this consistent regardless
  # of whether the caller already has a device open.
  old_dev <- grDevices::dev.cur()
  measuring_file <- tempfile(fileext = ".png")
  ragg::agg_png(measuring_file, width = 100, height = 100, units = "mm")
  on.exit({
    grDevices::dev.off()
    if (old_dev > 1) grDevices::dev.set(old_dev)
    unlink(measuring_file)
  }, add = TRUE)

  if (is.character(spec)) {
    if (is.null(base_dir)) base_dir <- dirname(spec)
    spec <- read_poster_yaml(spec)
  }
  if (is.null(base_dir)) base_dir <- "."
  spec <- normalize_spec(spec)
  if (is.null(theme)) theme <- do.call(poster_theme, spec$theme %||% list())

  size_mm <- poster_size(spec$poster$size %||% "A1", spec$poster$orientation %||% "portrait")

  title_grob <- if (!is.null(spec$title)) {
    do.call(poster_title, c(spec$title, list(theme = theme, width = size_mm[["width"]] - 20)))
  } else {
    NULL
  }

  title_h <- if (!is.null(title_grob)) {
    grid::convertHeight(sum(title_grob$heights), "mm", valueOnly = TRUE)
  } else {
    0
  }
  col_height_mm <- size_mm[["height"]] - title_h

  # Stitching the title and columns together via patchwork's `/` and `|`
  # operators (both of which route through `wrap_elements(full=)` for plain
  # grobs/gtables) causes some cards to be drawn a second time, as faint
  # duplicates further down the page, once there are 3+ stacked elements.
  # gtable's own row/column spanning does the same job -- mixed absolute-mm
  # rows are already used throughout this package -- without going through
  # whatever in patchwork's "full patch" machinery causes the duplication,
  # so build the whole poster as one plain gtable instead.
  if (!is.null(spec$grid)) {
    if (!is.null(spec$layout)) {
      cli::cli_warn(c(
        "Both {.field layout} and {.field grid} are set in the spec.",
        "i" = "{.field grid} takes precedence; {.field layout} is ignored."
      ))
    }
    n_cols <- validate_grid_columns(spec$grid)
    grid_col_width_mm <- size_mm[["width"]] / n_cols
    body_grob <- build_grid_body(spec$grid, spec$sections, theme, objects, base_dir,
                                 grid_col_width_mm, col_height_mm, show_plot_area = show_plot_area)
    full <- new_full_gtable(n_cols, grid_col_width_mm, title_grob, title_h, col_height_mm)
    body_row <- if (!is.null(title_grob)) 2 else 1
    full <- gtable::gtable_add_grob(full, body_grob, t = body_row, l = 1, r = n_cols, name = "grid_body")
  } else {
    layout <- spec$layout %||% list(left = names(spec$sections))
    align_rows <- isTRUE(layout$align_rows)
    columns <- layout[setdiff(names(layout), c("columns", "align_rows"))]
    # Empty columns are kept, not dropped. A top-level `columns: 4` with
    # only three sections expands (via flow_layout()) to three filled
    # columns and one empty one, and dropping the empty one silently
    # widened the other three to a quarter more than the spec asked for --
    # the same header laid out differently here than in acposter/qtposter,
    # which is exactly what the shared-key work was for. An explicitly
    # written empty column is likewise taken at its word.
    col_width_mm <- size_mm[["width"]] / max(length(columns), 1)

    col_grobs <- lapply(columns, function(section_names) {
      build_column(section_names, spec$sections, theme, objects, base_dir, col_width_mm, col_height_mm,
                   show_plot_area = show_plot_area)
    })

    # layout$align_rows = TRUE lines up each row position across columns to
    # the tallest "auto"-height card at that position, instead of letting
    # every column stack its cards at their own independent heights. The
    # first build_column() pass above only *measures* each card's natural
    # height; once the per-row target is known from that measurement, affected
    # columns are rebuilt so the shorter cards at that row stretch to match.
    if (align_rows && length(col_grobs) > 1) {
      n_rows <- max(vapply(col_grobs, function(cg) length(attr(cg, "is_auto")), integer(1)))
      target_row_mm <- vapply(seq_len(n_rows), function(i) {
        vals <- vapply(col_grobs, function(cg) {
          is_auto <- attr(cg, "is_auto")
          auto_mm <- attr(cg, "auto_mm")
          if (i <= length(is_auto) && isTRUE(is_auto[[i]])) auto_mm[[i]] else NA_real_
        }, numeric(1))
        vals <- vals[!is.na(vals)]
        if (length(vals)) max(vals) else NA_real_
      }, numeric(1))

      col_grobs <- lapply(columns, function(section_names) {
        build_column(section_names, spec$sections, theme, objects, base_dir, col_width_mm, col_height_mm,
                     show_plot_area = show_plot_area, target_row_mm = target_row_mm)
      })
    }

    n_cols <- max(length(columns), 1)
    full <- new_full_gtable(n_cols, col_width_mm, title_grob, title_h, col_height_mm)
    body_row <- if (!is.null(title_grob)) 2 else 1
    for (i in seq_along(col_grobs)) {
      full <- gtable::gtable_add_grob(full, col_grobs[[i]], t = body_row, l = i, name = names(columns)[[i]] %||% paste0("col", i))
    }
  }

  structure(
    list(patchwork = full, theme = theme, size_mm = size_mm, spec = spec,
        objects = objects, base_dir = base_dir),
    class = "ggposter"
  )
}

#' Start the poster's outer gtable (equal-width columns, optional title row)
#' @param n_cols Number of equal-width columns.
#' @param col_width_mm Width of one column, in millimetres.
#' @param title_grob The title band grob, or `NULL` for no title.
#' @param title_h Title band height in millimetres (ignored if `title_grob`
#'   is `NULL`).
#' @param col_height_mm Height available to the body row, in millimetres.
#' @return A [gtable::gtable()] with the title already placed (if any); the
#'   caller still needs to add the body content at the body row.
#' @keywords internal
#' @noRd
new_full_gtable <- function(n_cols, col_width_mm, title_grob, title_h, col_height_mm) {
  row_heights <- if (!is.null(title_grob)) {
    grid::unit(c(title_h, col_height_mm), "mm")
  } else {
    grid::unit(col_height_mm, "mm")
  }
  full <- gtable::gtable(widths = grid::unit(rep(col_width_mm, n_cols), "mm"),
                         heights = row_heights)
  if (!is.null(title_grob)) {
    full <- gtable::gtable_add_grob(full, title_grob, t = 1, l = 1, r = n_cols, name = "title")
  }
  full
}

#' Validate `spec$grid$columns` and return it
#' @param grid_spec `spec$grid`.
#' @return The number of columns, as a positive integer.
#' @keywords internal
#' @noRd
validate_grid_columns <- function(grid_spec) {
  n_cols <- grid_spec$columns
  if (is.null(n_cols) || !is.numeric(n_cols) || length(n_cols) != 1 ||
      n_cols < 1 || n_cols != round(n_cols)) {
    cli::cli_abort("{.field grid$columns} must be a single positive integer.")
  }
  as.integer(n_cols)
}

#' Coerce one `grid$boxes` coordinate/span to a whole number, or abort
#'
#' `as.integer()` alone was too permissive: it truncated a fractional `x`
#' silently (`x: 0.9` became column 0, not the column the spec named), and
#' let a zero or negative `w`/`h` through as an empty cell range. A
#' zero-span box occupies no cells at all, so it slipped past the overlap
#' check and then produced a gtable entry with `r < l`.
#'
#' @param value The value as written in the spec.
#' @param field Field name (`"x"`, `"y"`, `"w"`, `"h"`), for the message.
#' @param box_name The box's `name`, for the message.
#' @param min_value Smallest value accepted: `0` for a coordinate, `1` for a
#'   span.
#' @return `value` as a length-1 integer.
#' @keywords internal
#' @noRd
grid_box_int <- function(value, field, box_name, min_value) {
  if (!is.numeric(value) || length(value) != 1 || !is.finite(value) ||
      value != round(value) || value < min_value) {
    cli::cli_abort(c(
      "{.field grid$boxes} entry {.val {box_name}} has an invalid {.field {field}}: {.val {value}}.",
      "i" = "{.field {field}} must be a single whole number, {min_value} or greater."
    ))
  }
  as.integer(value)
}

#' Resolve a stack of tracks to absolute millimetre sizes
#'
#' The one sizing rule a poster body follows, in both of its layouts: a
#' track (a card row in [build_column()], a grid row in [build_grid_body()])
#' either measures its own content (`"auto"`) or takes a relative share of
#' whatever the measured tracks leave behind. Resolving everything to plain
#' absolute millimetres -- rather than mixing in a `"null"` unit -- is
#' deliberate: a gtable carrying a `"null"` row made patchwork's
#' `wrap_elements(full=)` draw the whole gtable a second time, as a faint
#' duplicate further down the page.
#'
#' The two callers had near-identical copies of this, which is how the
#' figure-height bug fixed on 2026-09-02 could be present in one sizing
#' path and not the other.
#'
#' @param is_auto Logical, one entry per track: `TRUE` if the track is
#'   sized by its own measured content.
#' @param auto_mm Numeric, one per track: the measured size of an `is_auto`
#'   track (`0` for the others).
#' @param weight Numeric, one per track: the relative share of a non-auto
#'   track (`0` for the others).
#' @param total_mm Space available to the whole stack, in millimetres.
#' @param overflow_message A cli message to warn with when the measured
#'   tracks alone are taller than `total_mm`, or `NULL` to stay silent. The
#'   string may use `{overflow_mm}`.
#' @return Numeric vector of track sizes in millimetres. When no track has
#'   a weight to absorb the leftover space, one extra trailing entry (a
#'   spacer) is appended, so the stack still totals `total_mm` and its
#'   content stays top-anchored instead of centred.
#' @keywords internal
#' @noRd
resolve_track_mm <- function(is_auto, auto_mm, weight, total_mm,
                             overflow_message = NULL) {
  auto_total  <- sum(auto_mm[is_auto])
  overflow_mm <- round(auto_total - total_mm, 1)
  if (!is.null(overflow_message) && auto_total - total_mm > 1e-6) {
    cli::cli_warn(overflow_message, .envir = environment())
  }
  leftover_mm  <- max(total_mm - auto_total, 0)
  weight_total <- sum(weight[!is_auto])
  out <- vapply(seq_along(is_auto), function(i) {
    if (is_auto[[i]]) {
      auto_mm[[i]]
    } else if (weight_total > 0) {
      leftover_mm * weight[[i]] / weight_total
    } else {
      0
    }
  }, numeric(1))
  if (weight_total == 0 && leftover_mm > 1e-6) out <- c(out, leftover_mm)
  out
}

#' Build the body of a `grid:`-laid-out poster: one gtable of boxes placed by
#' `x`/`y`/`w`/`h`, CSS-Grid style
#'
#' Row heights follow the same two rules as [build_column()]'s section
#' `height` (a number shares out leftover space; `"auto"` measures the
#' card's own content), applied per *row* instead of per column, from
#' whichever boxes in that row don't span multiple rows (`h == 1`). A
#' spanning box (`w > 1` or `h > 1`) never drives row/column sizing -- it is
#' always measured at its own natural size and pinned to the top-left of
#' the cell union it spans (see [anchor_top_left()]), so it can leave blank
#' space below it instead of stretching its neighbours. A row with no
#' non-spanning box in it (fully covered by spanning boxes) falls back to
#' an equal (weight `1`) share of the leftover space.
#'
#' @param grid_spec `spec$grid`: `list(columns =, boxes = list(list(name=,
#'   x=, y=, w=, h=), ...))`. `x`/`y` are 0-based; `w`/`h` default to `1`.
#' @param sections Named list of section specs (`spec$sections`).
#' @param theme,objects,base_dir,show_plot_area As in [build_column()].
#' @param col_width_mm Width of one grid column, in millimetres.
#' @param col_height_mm Total height available to the grid body, in
#'   millimetres.
#' @return A [gtable::gtable()] with every box placed at its `t`/`l`/`b`/`r`
#'   cell span.
#' @keywords internal
#' @noRd
#' Read and check a `grid:` spec's boxes
#'
#' Everything that can be settled before a single card is built: the fields
#' each box must carry and their types, that boxes and sections name each
#' other exactly once, that no box runs off the right edge, and that no two
#' boxes claim the same cell. Split out of [build_grid_body()] so that the
#' rules live in one readable place rather than as a preamble to the
#' measuring and assembly it has nothing to do with.
#'
#' @param grid_spec `spec$grid`.
#' @param sections Named list of section specs (`spec$sections`).
#' @param n_cols The grid's column count, from [validate_grid_columns()].
#' @return A list with `boxes` (each `list(name=, x=, y=, w=, h=)`, all
#'   integers, `x`/`y` 0-based) and `n_rows`, the number of rows the boxes
#'   between them occupy.
#' @keywords internal
#' @noRd
validate_grid_boxes <- function(grid_spec, sections, n_cols) {
  boxes <- lapply(grid_spec$boxes %||% list(), function(b) {
    if (is.null(b$name) || is.null(b$x) || is.null(b$y)) {
      cli::cli_abort("Every {.field grid$boxes} entry needs {.field name}, {.field x}, and {.field y}.")
    }
    if (!is.character(b$name) || length(b$name) != 1) {
      cli::cli_abort("Every {.field grid$boxes} {.field name} must be a single section name.")
    }
    list(name = b$name,
        x = grid_box_int(b$x, "x", b$name, 0),
        y = grid_box_int(b$y, "y", b$name, 0),
        w = grid_box_int(b$w %||% 1, "w", b$name, 1),
        h = grid_box_int(b$h %||% 1, "h", b$name, 1))
  })

  box_names <- vapply(boxes, `[[`, character(1), "name")
  undefined <- setdiff(box_names, names(sections))
  if (length(undefined)) {
    cli::cli_abort("{.field grid$boxes} references {cli::qty(length(undefined))} section{?s} not defined in {.field sections}: {.val {undefined}}.")
  }
  unplaced <- setdiff(names(sections), box_names)
  if (length(unplaced)) {
    cli::cli_abort("{cli::qty(length(unplaced))} Section{?s} defined in {.field sections} but not placed in {.field grid$boxes}: {.val {unplaced}}.")
  }

  overflow <- Filter(function(b) b$x + b$w > n_cols, boxes)
  if (length(overflow)) {
    overflow_names <- vapply(overflow, `[[`, character(1), "name")
    cli::cli_abort("{cli::qty(length(overflow))} Box{?es} overflow the grid's {n_cols} {cli::qty(n_cols)} column{?s}: {.val {overflow_names}}.")
  }

  n_rows <- if (length(boxes)) max(vapply(boxes, function(b) b$y + b$h, integer(1))) else 0L
  occupied <- matrix(NA_character_, nrow = n_rows, ncol = n_cols)
  for (b in boxes) {
    for (yy in b$y + seq_len(b$h)) {
      for (xx in b$x + seq_len(b$w)) {
        if (!is.na(occupied[yy, xx])) {
          cli::cli_abort("Boxes {.val {occupied[yy, xx]}} and {.val {b$name}} overlap in the grid.")
        }
        occupied[yy, xx] <- b$name
      }
    }
  }

  list(boxes = boxes, n_rows = n_rows)
}

build_grid_body <- function(grid_spec, sections, theme, objects, base_dir,
                            col_width_mm, col_height_mm, show_plot_area = FALSE) {
  n_cols   <- validate_grid_columns(grid_spec)
  placed   <- validate_grid_boxes(grid_spec, sections, n_cols)
  boxes    <- placed$boxes
  n_rows   <- placed$n_rows

  boxes <- lapply(boxes, function(b) {
    is_spanning <- b$w > 1 || b$h > 1
    height_field <- sections[[b$name]]$height %||% 1
    is_auto <- is_spanning || identical(height_field, "auto")
    card <- build_section(b$name, sections[[b$name]], theme, objects, base_dir,
                          col_width_mm * b$w, show_plot_area = show_plot_area,
                          fit_content_override = if (is_spanning) TRUE else NULL)
    c(b, list(card = card, is_auto = is_auto,
             auto_mm = if (is_auto) grid::convertHeight(measure_height(card), "mm", valueOnly = TRUE) else NA_real_,
             weight  = if (!is_auto) as.numeric(height_field) else NA_real_))
  })

  row_is_auto <- logical(n_rows)
  row_auto_mm <- numeric(n_rows)
  row_weight  <- numeric(n_rows)
  for (r in seq_len(n_rows)) {
    in_row <- Filter(function(b) b$h == 1 && b$y + 1 == r, boxes)
    auto_in_row <- Filter(function(b) b$is_auto, in_row)
    if (length(auto_in_row)) {
      row_is_auto[[r]] <- TRUE
      row_auto_mm[[r]] <- max(vapply(auto_in_row, `[[`, numeric(1), "auto_mm"))
    } else {
      weighted_in_row <- Filter(function(b) !b$is_auto, in_row)
      row_weight[[r]] <- if (length(weighted_in_row)) max(vapply(weighted_in_row, `[[`, numeric(1), "weight")) else 1
    }
  }

  row_mm <- resolve_track_mm(
    row_is_auto, row_auto_mm, row_weight, col_height_mm,
    overflow_message = "Poster grid content is {overflow_mm}mm taller than the page; some boxes may overlap.")

  grid_gtable <- gtable::gtable(widths = grid::unit(rep(col_width_mm, n_cols), "mm"),
                                heights = grid::unit(row_mm, "mm"))
  for (b in boxes) {
    # Only a *row*-spanning box's cell height can exceed its own content's
    # (row height is set purely by the h == 1 boxes in that row, so a
    # non-spanning box's cell, and a column-only-spanning box's cell, are
    # already sized to match its content exactly -- same as
    # build_column()'s cards, which don't need this either). Only h > 1
    # needs pinning to the top-left of its cell instead of stretching.
    #
    # anchor_top_left() itself isn't used here: it sizes its viewport from
    # measure_width()/measure_height(), but a poster_card's *width* is a
    # "null" unit (it normally fills whatever column it's given) -- outside
    # that context, gtable::gtable_width() resolves a lone "null" width to
    # some small, meaningless value, giving the card an near-zero-width
    # viewport that clips almost all of its content. The width this card
    # was actually built for is already known (col_width_mm * b$w), so use
    # that directly and only measure the height (which auto_mm already
    # holds, in the same units).
    card <- if (b$h > 1) {
      vp <- grid::viewport(x = grid::unit(0, "npc"), y = grid::unit(1, "npc"),
                           just = c("left", "top"), clip = "off",
                           width = grid::unit(col_width_mm * b$w, "mm"),
                           height = grid::unit(b$auto_mm, "mm"))
      grid::grobTree(b$card, vp = vp)
    } else {
      b$card
    }
    grid_gtable <- gtable::gtable_add_grob(grid_gtable, card, t = b$y + 1, l = b$x + 1,
                                           b = b$y + b$h, r = b$x + b$w, name = b$name)
  }
  grid_gtable
}

#' Build one column of stacked section cards
#'
#' A section's `height` is either a number (a relative share of the
#' leftover space, after `"auto"` sections take what their content needs)
#' or the string `"auto"`, meaning the card is built with [poster_card()]'s
#' `fit_content = TRUE` and its *actual measured height* is used instead.
#' Both kinds of section are resolved to absolute millimetre row heights on
#' one plain [gtable::gtable()], which the caller places in the poster's
#' root gtable.
#'
#' @param section_names Character vector of section names, top to bottom.
#' @param sections Named list of section specs (`spec$sections`).
#' @param theme A [poster_theme()] object.
#' @param objects Named list of R objects referenced by section bodies.
#' @param base_dir Directory used to resolve relative image paths.
#' @param col_width_mm Column width in millimetres, used as the default text
#'   wrap width when a section body does not specify one.
#' @param col_height_mm Total height available to the column, in
#'   millimetres. Sections with a numeric `height` share out whatever is
#'   left after the `"auto"` sections take the space their content needs.
#' @param show_plot_area Passed through to [poster_card()] for every card in
#'   the column.
#' @param target_row_mm Optional numeric vector, one value per section in
#'   `section_names`, from [build_poster()]'s cross-column `align_rows`
#'   pass. An `"auto"` section whose target exceeds its own measured height
#'   is rebuilt with `fit_content = FALSE` so it stretches to fill that
#'   height instead of leaving blank space beneath a shorter card.
#' @return A [gtable::gtable()] stacking the column's cards, carrying
#'   `is_auto` and `auto_mm` attributes (one entry per section) so
#'   [build_poster()] can compute row targets for `align_rows`.
#' @keywords internal
#' @noRd
build_column <- function(section_names, sections, theme, objects, base_dir,
                          col_width_mm, col_height_mm, show_plot_area = FALSE,
                          target_row_mm = NULL) {
  cards <- lapply(section_names, function(nm) {
    build_section(nm, sections[[nm]], theme, objects, base_dir, col_width_mm,
                  show_plot_area = show_plot_area)
  })
  raw_heights <- lapply(section_names, function(nm) sections[[nm]]$height %||% 1)
  is_auto <- vapply(raw_heights, identical, logical(1), "auto")

  auto_mm <- vapply(seq_along(cards), function(i) {
    if (is_auto[[i]]) grid::convertHeight(measure_height(cards[[i]]), "mm", valueOnly = TRUE) else 0
  }, numeric(1))

  if (!is.null(target_row_mm)) {
    for (i in seq_along(cards)) {
      if (is_auto[[i]] && !is.na(target_row_mm[[i]]) && target_row_mm[[i]] > auto_mm[[i]] + 1e-6) {
        cards[[i]] <- build_section(section_names[[i]], sections[[section_names[[i]]]], theme, objects,
                                    base_dir, col_width_mm, show_plot_area = show_plot_area,
                                    fit_content_override = FALSE)
        auto_mm[[i]] <- target_row_mm[[i]]
      }
    }
  }

  weights <- vapply(seq_along(raw_heights), function(i) {
    if (is_auto[[i]]) 0 else as.numeric(raw_heights[[i]])
  }, numeric(1))

  # Sized by the same rule as a grid: row -- see resolve_track_mm(), which
  # also appends the trailing spacer that keeps a column of nothing but
  # "auto" cards top-anchored at its full height. No overflow warning here:
  # a column that outgrows the page has always been left to overlap
  # silently, and starting to warn is a behaviour change, not a refactor.
  row_mm  <- resolve_track_mm(is_auto, auto_mm, weights, col_height_mm)
  heights <- grid::unit(row_mm, "mm")

  col <- gtable::gtable(widths = grid::unit(1, "null"), heights = heights)
  for (i in seq_along(cards)) {
    col <- gtable::gtable_add_grob(col, cards[[i]], t = i, l = 1, name = section_names[[i]])
  }
  attr(col, "is_auto") <- is_auto
  attr(col, "auto_mm") <- auto_mm
  col
}

#' Build a single section card from its spec
#' @param name Section name (used only for error messages).
#' @param section A single section spec: `list(header=, height=, body=list(type=, ...))`.
#' @param fit_content_override If not `NULL`, used instead of the usual
#'   `identical(section$height, "auto")` rule -- see `build_column()`'s
#'   `target_row_mm`.
#' @inheritParams build_column
#' @return A [poster_card()] grob.
#' @keywords internal
#' @noRd
build_section <- function(name, section, theme, objects, base_dir, col_width_mm,
                          show_plot_area = FALSE, fit_content_override = NULL) {
  if (is.null(section)) {
    cli::cli_abort("Section {.val {name}} is referenced in {.field layout} but missing from {.field sections}.")
  }
  # Resolved before the body is built, not after: a fit-to-content card
  # needs its figure to carry a real height (see build_body()), which the
  # body builder can only decide if it knows the card will be measured.
  fit_content <- if (!is.null(fit_content_override)) {
    fit_content_override
  } else {
    identical(section$height %||% 1, "auto")
  }
  body <- build_body(section$body, theme, objects, base_dir, col_width_mm,
                     show_plot_area = show_plot_area, fit_content = fit_content)
  poster_card(body, header = section$header, theme = theme, fit_content = fit_content,
             show_plot_area = show_plot_area)
}

#' Dispatch a section's `body` spec to the matching content builder
#'
#' `table` and `figure` bodies may also carry a `notes` field (a character
#' vector of markdown bullet lines): when present, the built content is
#' wrapped with [with_notes()] so the table/figure sits beside a bullet-list
#' description instead of alone. A `figure` body may instead carry a
#' `caption` field (a markdown string, possibly with embedded `"\n"` for
#' multiple bullet lines): when present, the built content is wrapped with
#' [with_caption_below()] so the bullets sit below the figure at full width.
#'
#' @param body A body spec: `list(type = "text"|"table"|"figure"|"image", ...)`.
#' @inheritParams build_column
#' @return A grob (or ggplot, for `"figure"`) ready for [poster_card()].
#' @keywords internal
#' @noRd
build_body <- function(body, theme, objects, base_dir, col_width_mm, show_plot_area = FALSE,
                       fit_content = FALSE) {
  type <- body$type %||% "text"
  has_notes   <- type %in% c("table", "figure") && !is.null(body$notes)
  has_caption <- type == "figure" && !has_notes && !is.null(body$caption)
  notes_width <- body$notes_width %||% 0.35
  pad_mm <- grid::convertWidth(theme$pad, "mm", valueOnly = TRUE)
  # `body$width`, if given, is the *combined* budget for main content + notes
  # (not the main content's own width): with_notes() splits it further below.
  total_width_mm <- (body$width %||% col_width_mm) - 2 * pad_mm
  main_width_mm  <- if (has_notes) total_width_mm * (1 - notes_width) - NOTES_GAP_MM else total_width_mm

  content <- switch(type,
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
                       width = main_width_mm),
    figure = {
      fig_width <- if (has_notes || has_caption) main_width_mm else body$width
      # A figure needs an explicit height, not just a width, whenever
      # something has to *measure* it:
      #   - paired with notes or a caption, so with_notes() /
      #     with_caption_below() can size the row it sits in;
      #   - in a `height: "auto"` card, so the card can size itself to the
      #     figure (see poster_fix_size()'s caching, which is per
      #     dimension).
      # Left unset in either case, a ggplotGrob measures to its fixed
      # axis/label rows alone -- its panel is a "null" unit -- so the card
      # collapsed to a fraction of the intended figure height. 4:3 is just
      # a reasonable default aspect ratio; a numeric `section$height` still
      # lets the figure fill its share of the column as before.
      needs_height <- has_notes || has_caption || isTRUE(fit_content)
      fig_height <- body$height
      if (is.null(fig_height) && needs_height) {
        basis <- fig_width %||% total_width_mm
        fig_height <- grid::convertWidth(as_mm_unit(basis), "mm", valueOnly = TRUE) * 0.75
      }
      card_figure(objects[[body$object]], theme, width = fig_width, height = fig_height)
    },
    image = card_image(file.path(base_dir, body$files), labels = body$labels,
                       theme = theme, height = body$height, width = body$width,
                       label_position = body$label_position %||% "below"),
    cli::cli_abort("Unknown body type {.val {type}}.")
  )
  if (has_notes) {
    content <- with_notes(content, body$notes, theme, total_width_mm, notes_width = notes_width,
                          show_plot_area = show_plot_area)
  } else if (has_caption) {
    caption_md <- strsplit(body$caption, "\n")[[1]]
    content <- with_caption_below(content, caption_md, theme, main_width_mm)
  }
  content
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
  # x$patchwork is a plain gtable (see build_column()/poster()): unlike a
  # patchwork/ggplot object, print()-ing a gtable just prints a text summary
  # of its layout and draws nothing, so draw it explicitly instead.
  grid::grid.newpage()
  grid::grid.draw(x$patchwork)
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
#' @param show_plot_area Passed to [build_poster()] when rebuilding, so
#'   [render_poster()] can produce a plot-area-outlined version from the
#'   stored spec/objects/theme.
#' @return A new `ggposter` object at the scaled size.
#' @keywords internal
#' @noRd
rescale_poster <- function(x, scale, show_plot_area = FALSE) {
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

  build_poster(spec, objects = x$objects, theme = theme, base_dir = x$base_dir,
               show_plot_area = show_plot_area)
}
