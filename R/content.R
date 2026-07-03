#' Text card content: a bullet list rendered as markdown
#'
#' Wraps a character vector of markdown lines (or a single markdown string) as
#' a body grob using [gridtext::textbox_grob()], so bold/italic/links work.
#'
#' @param md Character vector. Each element becomes one paragraph/bullet; they
#'   are joined with newlines. Markdown syntax (e.g. `**bold**`) is honoured.
#' @param theme A [poster_theme()] object.
#' @param width Wrap width, as a [grid::unit] or millimetres. `NULL` lets the
#'   grob size to its natural width (no wrapping).
#'
#' @return A grob suitable as the `body` argument of [poster_card()].
#' @export
#' @examples
#' g <- card_text(c("- point one", "- point two"), poster_theme())
card_text <- function(md, theme = poster_theme(), width = NULL) {
  bullet <- paste0(intToUtf8(0x2022), " ")
  md <- gsub("^-\\s+", bullet, md)  # "- item" -> bullet + item (gridtext has no <ul> support)
  md <- gsub("`", "", md)         # drop code spans (gridtext has no <code> support)
  md <- paste(md, collapse = "<br>")
  gp <- grid::gpar(fontsize = theme$base_size, fontfamily = theme$base_family,
                   col = theme$body_text)
  width <- if (is.null(width)) grid::unit(1, "npc") else as_mm_unit(width)
  gridtext::textbox_grob(md, x = 0, y = 1, hjust = 0, vjust = 1,
                         gp = gp, box_gp = grid::gpar(col = NA),
                         padding = grid::unit(c(0, 0, 0, 0), "mm"),
                         width = width)
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
#'   leaves that dimension unconstrained.
#' @return The grob with a size-fixing viewport attached.
#' @keywords internal
#' @noRd
poster_fix_size <- function(g, width = NULL, height = NULL) {
  if (is.null(width) && is.null(height)) return(g)
  vp <- grid::viewport(
    width  = if (!is.null(width))  as_mm_unit(width)  else grid::unit(1, "npc"),
    height = if (!is.null(height)) as_mm_unit(height) else grid::unit(1, "npc")
  )
  grid::grobTree(g, vp = vp)
}

#' Image card content: one or more photos arranged in a row, with optional labels
#'
#' Reads image files with [magick::image_read()], converts them to raster
#' grobs, and lays them out in a horizontal strip -- e.g. a "dominant species"
#' photo band, as in the sample poster.
#'
#' @param files Character vector of image file paths.
#' @param labels Optional character vector of captions, same length as
#'   `files`, drawn under each photo.
#' @param theme A [poster_theme()] object.
#' @param height Height of each photo, as a [grid::unit] or millimetres.
#' @param space Gap between photos, as a [grid::unit] or millimetres.
#'
#' @return A grob suitable as the `body` argument of [poster_card()].
#' @export
#' @examples
#' f <- system.file("extdata", "small.JPG", package = "ggposter")
#' g <- card_image(f, labels = "sample", theme = poster_theme())
card_image <- function(files, labels = NULL, theme = poster_theme(),
                       height = 60, space = 3) {
  height <- as_mm_unit(height)
  space  <- as_mm_unit(space)
  n <- length(files)
  if (!is.null(labels) && length(labels) != n) {
    cli::cli_abort("{.arg labels} must have the same length as {.arg files}.")
  }

  labs <- if (is.null(labels)) NULL else lapply(labels, function(l) {
    grid::textGrob(l, gp = grid::gpar(fontsize = theme$base_size * 0.6,
                                      fontfamily = theme$base_family,
                                      col = theme$body_text))
  })
  row_height <- if (is.null(labs)) {
    height
  } else {
    label_h <- do.call(grid::unit.c, lapply(labs, grid::grobHeight))
    height + grid::unit(3, "mm") + max(label_h)
  }

  built <- lapply(seq_len(n), function(i) {
    img <- magick::image_read(files[[i]])
    info <- magick::image_info(img)
    ar <- info$width / info$height
    photo_width <- height * ar
    rg <- grid::rasterGrob(grDevices::as.raster(img), width = grid::unit(1, "npc"),
                           height = grid::unit(1, "npc"))
    photo_vp <- grid::viewport(width = photo_width, height = height)
    photo <- grid::grobTree(rg, vp = photo_vp)
    if (is.null(labs)) return(list(width = photo_width, grob = photo))
    cell <- gtable::gtable(widths = photo_width,
                          heights = grid::unit.c(height, grid::unit(3, "mm"),
                                                 row_height - height - grid::unit(3, "mm")))
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
