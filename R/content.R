#' Text card content: a bullet list rendered as markdown
#'
#' Wraps a character vector of markdown lines as a body grob using
#' [gridtext::textbox_grob()], so bold/italic/links work. Each line is laid
#' out as its own row, one per element of `md`; a `"- "`-prefixed line gets
#' a bullet in its own narrow column so a wrapped continuation line aligns
#' under the item's own text rather than under the bullet (a hanging
#' indent) -- gridtext has no CSS `text-indent`/`padding-left` support to do
#' this within a single call, so each item is measured and wrapped
#' separately instead.
#'
#' @param md Character vector. Each element becomes one paragraph/bullet.
#'   Markdown syntax (e.g. `**bold**`) is honoured.
#' @param theme A [poster_theme()] object.
#' @param width Wrap width, as a [grid::unit] or millimetres. `NULL` lets the
#'   grob size to its natural width (no wrapping).
#'
#' @return A grob suitable as the `body` argument of [poster_card()].
#' @export
#' @examples
#' g <- card_text(c("- point one", "- point two"), poster_theme())
card_text <- function(md, theme = poster_theme(), width = NULL) {
  gp <- grid::gpar(fontsize = theme$base_size, fontfamily = theme$base_family,
                   col = theme$body_text, lineheight = theme$lineheight)
  width <- if (is.null(width)) grid::unit(1, "npc") else as_mm_unit(width)
  bullet <- paste0(intToUtf8(0x2022), " ")
  bullet_w <- grid::grobWidth(grid::textGrob(bullet, gp = gp))

  box <- function(text, w) {
    text <- gsub("`", "", text)  # drop code spans (gridtext has no <code> support)
    gridtext::textbox_grob(text, x = 0, y = 1, hjust = 0, vjust = 1, halign = 0,
                           gp = gp, box_gp = grid::gpar(col = NA),
                           padding = grid::unit(c(0, 0, 0, 0), "mm"),
                           width = w)
  }

  rows <- lapply(md, function(line) {
    if (!grepl("^-\\s+", line)) return(box(line, width))
    text    <- sub("^-\\s+", "", line)
    text_w  <- width - bullet_w
    txt_box <- box(text, text_w)
    bl <- grid::textGrob(bullet, x = 0, y = 1, hjust = 0, vjust = 1, gp = gp)
    row <- gtable::gtable(widths = grid::unit.c(bullet_w, text_w),
                         heights = grid::grobHeight(txt_box))
    row <- gtable::gtable_add_grob(row, bl,      t = 1, l = 1, name = "bullet")
    row <- gtable::gtable_add_grob(row, txt_box, t = 1, l = 2, name = "text")
    row
  })

  heights <- do.call(grid::unit.c, lapply(rows, function(r) {
    if (inherits(r, "gtable")) gtable::gtable_height(r) else grid::grobHeight(r)
  }))
  out <- gtable::gtable(widths = width, heights = heights)
  for (i in seq_along(rows)) {
    out <- gtable::gtable_add_grob(out, rows[[i]], t = i, l = 1, name = paste0("line", i))
  }
  out
}

#' Place a bullet-list description beside a table or figure
#'
#' Splits a card's body horizontally into the given content on the left and
#' a bullet list (via [card_text()], left-aligned by default) on the right,
#' at a relative width split. Used by [poster()] when a `table`/`figure`
#' section body has a `notes` field.
#'
#' @param main The main content grob (e.g. from [card_table()] or
#'   [card_figure()]), already built to fit within `total_width_mm * (1 -
#'   notes_width)` (minus the gap) -- `with_notes()` only lays it out
#'   alongside the notes, it does not resize it. `card_table()` only
#'   shrinks a table that's too wide for that budget, it never stretches a
#'   narrower one to fill it, so `main`'s *actual* measured width (not the
#'   full budget) is used for the column, and the gap column sits right
#'   after it -- the notes column gets whatever space that frees up,
#'   instead of leaving a dead gap between the two.
#' @param notes_md Character vector of markdown bullet lines for
#'   [card_text()].
#' @param theme A [poster_theme()] object.
#' @param total_width_mm Total width available to `main` and the notes
#'   column together, in millimetres. Needed (rather than a relative split)
#'   because relative (`"null"`) gtable units can't be measured outside of a
#'   full layout render, and this whole grob's height must be measurable so
#'   a `height = "auto"` card can size itself to it (see [build_column()]).
#' @param notes_width Fraction (0-1) of `total_width_mm` given to `main` as
#'   an upper bound; the notes column gets the rest, expanding to take up
#'   whatever `main` doesn't actually use (see `main` above).
#' @param show_plot_area If `TRUE`, draw a dashed border around `main` and
#'   another around the notes column, separately, on top of the content --
#'   see [poster_card()]. When this draws its own borders, the caller must
#'   not also draw one around the whole combined body (see the
#'   `"plot_area_drawn"` attribute set on the return value).
#' @return A grob suitable as the `body` argument of [poster_card()].
#' @keywords internal
#' @noRd
with_notes <- function(main, notes_md, theme, total_width_mm, notes_width = 0.35,
                       show_plot_area = FALSE) {
  gap_mm             <- 5 / 3
  max_notes_width_mm <- total_width_mm * notes_width
  max_main_width_mm  <- total_width_mm - max_notes_width_mm - gap_mm
  main_width_mm  <- min(grid::convertWidth(measure_width(main), "mm", valueOnly = TRUE),
                        max_main_width_mm)
  notes_width_mm <- total_width_mm - main_width_mm - gap_mm
  notes_grob <- card_text(notes_md, theme, width = notes_width_mm)
  row_height <- max(measure_height(main), measure_height(notes_grob))
  row <- gtable::gtable(
    widths  = grid::unit(c(main_width_mm, gap_mm, notes_width_mm), "mm"),
    heights = row_height
  )
  row <- gtable::gtable_add_grob(row, main, t = 1, l = 1, name = "main")
  # Top-anchored explicitly, rather than relying on how a shorter grob
  # happens to be positioned within a taller cell: when notes is shorter
  # than main (the common case), it must sit level with the top of main,
  # not centred in the row.
  row <- gtable::gtable_add_grob(row, anchor_top_left(notes_grob), t = 1, l = 3, name = "notes")
  if (show_plot_area) {
    # Sized to each grob's own measured width/height, not the full column it
    # sits in: card_table() only shrinks a table that's too wide for its
    # column, it never stretches a narrower one to fill it, so a border
    # drawn at the column's full width would show a misleadingly large
    # "empty" margin that was never really part of the table's own area.
    plot_area_gp <- grid::gpar(fill = NA, col = "#FF00FF", lty = "dashed", lwd = 1.2)
    plot_area_for <- function(g) {
      grid::rectGrob(x = 0, y = 1, just = c("left", "top"),
                     width = measure_width(g), height = measure_height(g),
                     gp = plot_area_gp)
    }
    row <- gtable::gtable_add_grob(row, plot_area_for(main), t = 1, l = 1, z = Inf, name = "plot_area_main")
    row <- gtable::gtable_add_grob(row, plot_area_for(notes_grob), t = 1, l = 3, z = Inf, name = "plot_area_notes")
  }
  out <- anchor_top_left(row)
  # Tells poster_card() this body already has its own (two, separate)
  # plot-area borders, so it should not also draw one around the whole
  # combined body -- see poster_card()'s show_plot_area handling.
  if (show_plot_area) attr(out, "plot_area_drawn") <- TRUE
  out
}

#' Table card content: a booktab-style table grob
#'
#' Builds a table with a title/caption and simple top/bottom/header rules
#' (booktab style), based on [gridExtra::tableGrob()].
#'
#' @param x A data.frame or tibble.
#' @param theme A [poster_theme()] object.
#' @param title Optional title text drawn above the table.
#' @param caption Optional caption text drawn below the table.
#' @param rows Passed to [gridExtra::tableGrob()]; `NULL` (default) hides row
#'   names.
#' @param width Target width, as a [grid::unit] or millimetres. If the table
#'   is naturally wider, its font size is shrunk to fit (with a final clip as
#'   a safety net). `NULL` leaves the table at its natural size.
#'
#' @return A grob suitable as the `body` argument of [poster_card()].
#' @export
#' @examples
#' g <- card_table(head(iris), poster_theme(), title = "Iris")
card_table <- function(x, theme = poster_theme(), title = NULL, caption = NULL,
                       rows = NULL, width = NULL) {
  build <- function(size_mult) {
    tt <- gridExtra::ttheme_minimal(
      base_size = theme$base_size * 0.8 * size_mult,
      base_family = theme$base_family,
      core = list(fg_params = list(col = theme$body_text)),
      colhead = list(fg_params = list(col = theme$body_text, fontface = "bold"))
    )
    g <- gridExtra::tableGrob(x, rows = rows, theme = tt)
    g <- poster_add_booktab(g, theme)
    if (!is.null(title) || !is.null(caption)) {
      g <- poster_add_annotation(g, title = title, caption = caption, theme = theme,
                          size_mult = size_mult)
    }
    g
  }

  g <- build(1)
  if (!is.null(width)) {
    target <- grid::convertWidth(as_mm_unit(width), "mm", valueOnly = TRUE)
    # A single font-size guess from the width ratio is only approximate:
    # cell padding does not shrink linearly with font size, so large shrink
    # factors (e.g. a small preview via render_poster(scale=)) can still
    # leave the table too wide. Iterate a few times, guessing conservatively
    # short of the measured ratio each time, before falling back to the
    # clip in anchor_top_left() as a last-resort safety net.
    size_mult <- 1
    for (i in 1:5) {
      natural <- grid::convertWidth(measure_width(g), "mm", valueOnly = TRUE)
      if (natural <= target || natural <= 0) break
      size_mult <- size_mult * (target / natural) * 0.97
      g <- build(size_mult)
    }
  }
  anchor_top_left(g, clip = "on")
}

#' Add booktab-style horizontal rules to a table grob
#' @param g A [gridExtra::tableGrob()].
#' @param theme A [poster_theme()] object.
#' @return The table grob with rule segments added.
#' @keywords internal
#' @noRd
poster_add_booktab <- function(g, theme) {
  header_rows <- which(g$layout$name == "colhead-fg")
  top <- 1; bottom <- max(g$layout$t)
  header_bottom <- if (length(header_rows)) max(g$layout[header_rows, "t"]) else top

  gtable::gtable_add_grob(
    g,
    grobs = list(
      grid::segmentsGrob(y1 = 1, y0 = 1, gp = grid::gpar(lwd = 2.5, col = theme$body_text)),
      grid::segmentsGrob(y1 = 0, y0 = 0, gp = grid::gpar(lwd = 2.5, col = theme$body_text)),
      grid::segmentsGrob(y1 = 0, y0 = 0, gp = grid::gpar(lwd = 1.2, col = theme$body_text))
    ),
    t = c(top, bottom, header_bottom),
    b = c(top, bottom, header_bottom),
    l = 1, r = ncol(g),
    name = c("booktab-top", "booktab-bottom", "booktab-header")
  )
}

#' Add a title and/or caption above/below a table grob
#' @inheritParams poster_add_booktab
#' @param title Optional title string.
#' @param caption Optional caption string.
#' @param size_mult Multiplier applied to `theme$base_size` for the title and
#'   caption text, kept in sync with the table's own font scaling.
#' @return The table grob stacked with title/caption text grobs.
#' @keywords internal
#' @noRd
poster_add_annotation <- function(g, title = NULL, caption = NULL, theme, size_mult = 1) {
  grobs <- list(g)
  heights <- list(measure_height(g))
  if (!is.null(title)) {
    tg <- grid::textGrob(title, x = 0, hjust = 0,
      gp = grid::gpar(fontsize = theme$base_size * 0.95 * size_mult, fontface = "bold",
                      fontfamily = theme$base_family, col = theme$body_text))
    grobs <- c(list(tg), grobs)
    heights <- c(list(grid::grobHeight(tg) + grid::unit(3, "mm")), heights)
  }
  if (!is.null(caption)) {
    cg <- grid::textGrob(caption, x = 0, hjust = 0,
      gp = grid::gpar(fontsize = theme$base_size * 0.7 * size_mult, fontface = "italic",
                      fontfamily = theme$base_family, col = theme$body_text))
    grobs <- c(grobs, list(cg))
    heights <- c(heights, list(grid::grobHeight(cg) + grid::unit(3, "mm")))
  }
  tbl <- gtable::gtable(widths = measure_width(g),
                       heights = do.call(grid::unit.c, heights))
  for (i in seq_along(grobs)) {
    tbl <- gtable::gtable_add_grob(tbl, grobs[[i]], t = i, l = 1, name = paste0("row", i))
  }
  tbl
}

#' Figure card content: a ggplot styled for poster font size
#'
#' Applies the poster's base font size/family to a ggplot's theme, then
#' converts it to a grob.
#'
#' @param gg A ggplot object.
#' @param theme A [poster_theme()] object.
#' @param width,height Target size as a [grid::unit] or millimetres. `NULL`
#'   keeps the plot's natural size.
#'
#' @return A grob suitable as the `body` argument of [poster_card()].
#' @export
#' @examples
#' g <- card_figure(ggplot2::qplot(1, 1), poster_theme())
card_figure <- function(gg, theme = poster_theme(), width = NULL, height = NULL) {
  gg <- gg + ggplot2::theme(
    text = ggplot2::element_text(size = theme$base_size * 0.75,
                                 family = theme$base_family)
  )
  g <- ggplot2::ggplotGrob(gg)
  poster_fix_size(g, width = width, height = height)
}

#' Force a grob to a fixed width/height via a wrapping viewport
#' @param g A grob.
#' @param width,height Target size as [grid::unit] or millimetres. `NULL`
#'   leaves that dimension unconstrained (filling whatever space the
#'   surrounding cell gives it).
#' @return The grob with a size-fixing viewport attached. Each explicitly
#'   given dimension is cached (see [measure_width()]/[measure_height()])
#'   since a bare `grobTree()` wrapper has no measurable size of its own --
#'   without this a fit-to-content (`height = "auto"`) figure card that sets
#'   an explicit height but no width can't be measured, and the next card
#'   overlaps it. The unspecified dimension is stored as `NULL` (its
#'   viewport uses `"npc"`, unmeasurable outside a layout) so its
#'   `measure_*()` falls through to the generic.
#' @keywords internal
#' @noRd
poster_fix_size <- function(g, width = NULL, height = NULL) {
  if (is.null(width) && is.null(height)) return(g)
  w <- if (!is.null(width))  as_mm_unit(width)  else grid::unit(1, "npc")
  h <- if (!is.null(height)) as_mm_unit(height) else grid::unit(1, "npc")
  out <- grid::grobTree(g, vp = grid::viewport(width = w, height = h))
  attr(out, "measured_size") <- list(
    width  = if (!is.null(width))  w else NULL,
    height = if (!is.null(height)) h else NULL
  )
  out
}

#' Image card content: one or more photos arranged in a row, with optional labels
#'
#' Reads image files with [magick::image_read()], converts them to raster
#' grobs, and lays them out in a horizontal strip -- e.g. a "dominant species"
#' photo band, as in the sample poster.
#'
#' @param files Character vector of image file paths.
#' @param labels Optional character vector of captions, same length as
#'   `files`.
#' @param theme A [poster_theme()] object.
#' @param height Height of each photo, as a [grid::unit] or millimetres.
#'   `NULL` (default, if `width` is also `NULL`) uses 60mm.
#' @param width Alternative to `height`: a target *total* row width (all
#'   photos plus the gaps between them). Each photo's height is solved so
#'   that, at its own aspect ratio, the row sums to this width -- useful for
#'   making a photo band fill a specific fraction of a card's width
#'   regardless of how many photos it has. Takes precedence over `height`
#'   if both are given.
#' @param space Gap between photos, as a [grid::unit] or millimetres.
#' @param label_position `"below"` (default) draws each caption in its own
#'   row under the photo. `"inside"` overlays it near the bottom of the
#'   photo itself (white text on a semi-transparent bar), so the row's
#'   height is just the photo height.
#'
#' @return A grob suitable as the `body` argument of [poster_card()].
#' @export
#' @examples
#' f <- system.file("extdata", "small.JPG", package = "ggposter")
#' g <- card_image(f, labels = "sample", theme = poster_theme())
card_image <- function(files, labels = NULL, theme = poster_theme(),
                       height = NULL, width = NULL, space = 3,
                       label_position = c("below", "inside")) {
  label_position <- match.arg(label_position)
  space <- as_mm_unit(space)
  n <- length(files)
  if (!is.null(labels) && length(labels) != n) {
    cli::cli_abort("{.arg labels} must have the same length as {.arg files}.")
  }

  ars <- vapply(files, function(f) {
    info <- magick::image_info(magick::image_read(f))
    info$width / info$height
  }, numeric(1))

  if (!is.null(width)) {
    space_mm  <- grid::convertWidth(space, "mm", valueOnly = TRUE)
    target_mm <- grid::convertWidth(as_mm_unit(width), "mm", valueOnly = TRUE)
    height <- grid::unit(max((target_mm - space_mm * (n - 1)) / sum(ars), 1), "mm")
  } else {
    height <- as_mm_unit(height %||% 60)
  }
  photo_widths <- lapply(ars, function(ar) height * ar)

  # Wrapped to each photo's own width, rather than a single-line centred
  # textGrob: a caption longer than a narrow (e.g. portrait) photo used to
  # overflow evenly on both sides and get clipped, chopping text off both
  # its start and end.
  inside <- !is.null(labels) && label_position == "inside"
  make_label <- function(l, w) {
    if (inside) {
      gridtext::textbox_grob(l, x = grid::unit(0.5, "npc"), y = grid::unit(2, "mm"),
                             hjust = 0.5, vjust = 0, halign = 0.5, width = w,
                             gp = grid::gpar(fontsize = theme$base_size * 0.6,
                                             fontfamily = theme$base_family, col = "#FFFFFF"),
                             box_gp = grid::gpar(col = NA),
                             padding = grid::unit(c(0, 0, 0, 0), "mm"))
    } else {
      gridtext::textbox_grob(l, x = grid::unit(0.5, "npc"), y = grid::unit(1, "npc"),
                             hjust = 0.5, vjust = 1, halign = 0.5, width = w,
                             gp = grid::gpar(fontsize = theme$base_size * 0.6,
                                             fontfamily = theme$base_family, col = theme$body_text),
                             box_gp = grid::gpar(col = NA),
                             padding = grid::unit(c(0, 0, 0, 0), "mm"))
    }
  }
  labs <- if (is.null(labels)) NULL else Map(make_label, labels, photo_widths)
  label_gap <- grid::unit(1, "mm")
  row_height <- if (is.null(labs) || inside) {
    height
  } else {
    label_h <- do.call(grid::unit.c, lapply(labs, grid::grobHeight))
    height + label_gap + max(label_h)
  }

  built <- lapply(seq_len(n), function(i) {
    img <- magick::image_read(files[[i]])
    photo_width <- photo_widths[[i]]
    rg <- grid::rasterGrob(grDevices::as.raster(img), width = grid::unit(1, "npc"),
                           height = grid::unit(1, "npc"))
    photo_vp <- grid::viewport(width = photo_width, height = height)

    if (inside) {
      bar_h <- grid::grobHeight(labs[[i]]) + grid::unit(4, "mm")
      bar <- grid::rectGrob(y = 0, height = bar_h, just = "bottom",
                            gp = grid::gpar(fill = grDevices::rgb(0, 0, 0, 0.55), col = NA))
      photo <- grid::grobTree(rg, bar, labs[[i]], vp = photo_vp)
      return(list(width = photo_width, grob = photo))
    }

    photo <- grid::grobTree(rg, vp = photo_vp)
    if (is.null(labs)) return(list(width = photo_width, grob = photo))
    cell <- gtable::gtable(widths = photo_width,
                          heights = grid::unit.c(height, label_gap,
                                                 row_height - height - label_gap))
    cell <- gtable::gtable_add_grob(cell, photo,   t = 1, l = 1)
    cell <- gtable::gtable_add_grob(cell, labs[[i]], t = 3, l = 1)
    list(width = photo_width, grob = cell)
  })

  cells  <- lapply(built, `[[`, "grob")
  widths <- do.call(grid::unit.c, lapply(built, `[[`, "width"))
  row <- gtable::gtable(widths = widths, heights = row_height)
  for (i in seq_along(cells)) {
    row <- gtable::gtable_add_grob(row, cells[[i]], t = 1, l = i)
  }
  row <- gtable::gtable_add_col_space(row, space)
  anchor_top_left(row)
}
