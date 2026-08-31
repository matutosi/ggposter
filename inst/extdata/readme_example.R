# README (英・日) で使う「例のポスター」を組み立てる．
#
# **README にはコードを直接置かない** (2026-08-31)．姉妹ツール acposter・qtposter が
# 見本を別ファイルに置き，README からはリンクするだけにしているのに揃えた．
# ここにある R のリストは `poster_readme_example.yml` と同じ内容で，
# 「同じポスターを R のリストでも YAML でも書ける」ことを示すための対になっている．
#
# 使い方:
#   source(system.file("extdata", "readme_example.R", package = "ggposter"))
#   p <- readme_example_poster()                     # R のリストから
#   render_poster(p, "poster.pdf")
#
#   objs <- readme_example_objects()                 # YAML から (同じものができる)
#   p_yml <- poster(system.file("extdata", "poster_readme_example.yml",
#                               package = "ggposter"), objects = objs)

# 図・表と，本文が参照する値 (最も良い/悪い区分など) をまとめて作る．
readme_example_parts <- function() {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE),
            requireNamespace("scales", quietly = TRUE))
  mpg <- ggplot2::mpg

  tbl_class <- aggregate(cbind(hwy, cty) ~ class, data = mpg, FUN = function(x) round(mean(x), 1))
  names(tbl_class) <- c("Class", "Mean hwy", "Mean cty")
  class_best  <- tbl_class$Class[which.max(tbl_class$`Mean hwy`)]
  class_worst <- tbl_class$Class[which.min(tbl_class$`Mean hwy`)]

  tbl_drv <- aggregate(cbind(hwy, cty) ~ drv, data = mpg, FUN = function(x) round(mean(x), 1))
  names(tbl_drv) <- c("Drivetrain", "Mean hwy", "Mean cty")
  drv_best  <- tbl_drv$Drivetrain[which.max(tbl_drv$`Mean hwy`)]
  drv_worst <- tbl_drv$Drivetrain[which.min(tbl_drv$`Mean hwy`)]

  fig_facet <- ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy, colour = class)) +
    ggplot2::geom_point() +
    ggplot2::labs(caption = paste(
      "• Highway mileage falls as engine displacement rises.",
      "• Compact and subcompact classes reach the highest mileage.",
      sep = "\n")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "inside", legend.position.inside = c(0.85, 0.72),
          legend.background = ggplot2::element_rect(fill = scales::alpha("white", 0.7), colour = NA),
          legend.key.size = ggplot2::unit(0.9, "lines"),
          plot.caption = ggplot2::element_text(hjust = 0, size = ggplot2::rel(1.3), colour = "black"))

  fig_scatter <- ggplot2::ggplot(mpg, ggplot2::aes(cty, hwy, colour = drv)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "inside", legend.position.inside = c(0.75, 0.32),
          legend.background = ggplot2::element_rect(fill = scales::alpha("white", 0.7), colour = NA))

  fig_box <- ggplot2::ggplot(mpg, ggplot2::aes(drv, hwy, fill = drv)) +
    ggplot2::geom_boxplot(show.legend = FALSE) +
    ggplot2::labs(x = "Drivetrain", y = "Highway mpg") +
    ggplot2::theme_bw()
  drv_med <- aggregate(hwy ~ drv, data = mpg, FUN = median)
  drv_box_best <- as.character(drv_med$drv[which.max(drv_med$hwy)])

  mpg_heat <- aggregate(hwy ~ class + drv, data = mpg, FUN = function(x) round(mean(x), 1))
  fig_heat <- ggplot2::ggplot(mpg_heat, ggplot2::aes(drv, class, fill = hwy)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = hwy), size = 2.6) +
    ggplot2::scale_fill_gradient(low = "#E8F5E9", high = "#2E7D32") +
    ggplot2::labs(x = "Drivetrain", y = "Class", fill = "Mean hwy") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  img_dir <- system.file("extdata", package = "ggposter")
  stock_photos <- c("small.JPG", "tall.jpg", "wide.jpg", "large.JPG")
  stock_labels <- c("Photo A", "Photo B", "Photo C", "Photo D")

  list(tbl_class = tbl_class, tbl_drv = tbl_drv,
       fig_facet = fig_facet, fig_scatter = fig_scatter,
       fig_box = fig_box, fig_heat = fig_heat,
       class_best = class_best, class_worst = class_worst,
       drv_best = drv_best, drv_worst = drv_worst, drv_box_best = drv_box_best,
       img_dir = img_dir, stock_photos = stock_photos, stock_labels = stock_labels)
}

# `objects =` に渡す図と表だけを取り出す (YAML から組むときはこれだけで足りる)．
readme_example_objects <- function(parts = readme_example_parts()) {
  parts[c("tbl_class", "tbl_drv", "fig_facet", "fig_scatter", "fig_box", "fig_heat")]
}

# 同じ内容を R のリストで書いた spec (`poster_readme_example.yml` と対になる)．
readme_example_spec <- function(parts = readme_example_parts()) {
  class_best   <- parts$class_best
  class_worst  <- parts$class_worst
  drv_best     <- parts$drv_best
  drv_worst    <- parts$drv_worst
  drv_box_best <- parts$drv_box_best
  stock_photos <- parts$stock_photos
  stock_labels <- parts$stock_labels

  spec <- list(
    title = list(
      title = "Example Poster: Fuel Economy Patterns in the mpg Dataset",
      authors = "*Jane Doe (Example University), John Smith (Example Institute)",
      funding = "This is a demonstration poster for the ggposter package; it does not describe real research."
    ),
    layout = list(
      left  = c("objectives", "background", "methods", "summary_table", "fig_box", "fig_heat"),
      right = c("conclusions", "results_table", "fig_facet", "fig_scatter", "photos_2")
    ),
    sections = list(
      objectives = list(header = "OBJECTIVES", height = "auto", body = list(type = "text", md = c(
        "- Demonstrate the ggposter package.",
        "- Use the mpg fuel-economy dataset as example content.",
        "- Combine text, tables, figures, and photos in one poster."
      ))),
      background = list(header = "BACKGROUND", height = "auto", body = list(type = "text", md = c(
        "- Conference posters often mix text, tables, and figures.",
        "- ggposter arranges these as rounded, tab-headed cards.",
        "- Layout and content can be declared as an R list or a YAML file."
      ))),
      methods = list(header = "METHODS", height = "auto", body = list(type = "text", md = c(
        "- Data: the mpg dataset (234 vehicles, model years 1999-2008).",
        "- Figures: highway/city mileage by class and drivetrain.",
        "- Photos: generic stock images bundled with ggposter."
      ))),
      summary_table = list(header = "SUMMARY by class", height = "auto", body = list(
        type = "table", object = "tbl_class", title = "Mean mileage by vehicle class",
        notes = c(
          sprintf("- **%s** has the best highway mileage of all vehicle classes in this dataset.", class_best),
          sprintf("- **%s** has the worst, largely due to its greater size and weight.", class_worst),
          "- Compact and subcompact classes have nearly identical mean highway mileage.",
          "- Midsize vehicles average close to the compact/subcompact classes.",
          "- Pickup and SUV classes have the two lowest highway mileages, both under 19 mpg.",
          "- City mileage tracks highway mileage closely across all seven classes."
        )
      )),
      fig_box = list(header = "Mileage spread by drivetrain", height = "auto", body = list(
        type = "figure", object = "fig_box", notes_width = 0.4, height = 117,
        notes = c(
          "- Boxes show the full spread of highway mileage, not just the mean.",
          sprintf("- **%s**-wheel drive has the highest median highway mileage.", drv_box_best)
        )
      )),
      fig_heat = list(header = "Mean mileage: class x drivetrain", height = "auto", body = list(
        type = "figure", object = "fig_heat", notes_width = 0.4, height = 137,
        notes = c(
          "- Colour shows mean highway mpg for each class/drivetrain combination.",
          "- Blank cells are combinations that don't occur in the data."
        )
      )),
      conclusions = list(header = "CONCLUSIONS", height = "auto", body = list(type = "text", md = c(
        "- Compact and subcompact cars get the best highway mileage.",
        "- SUVs and pickups get the lowest.",
        "- ggposter can lay out this kind of summary automatically."
      ))),
      results_table = list(header = "SUMMARY by drivetrain", height = "auto", body = list(
        type = "table", object = "tbl_drv", title = "Mean mileage by drivetrain",
        notes = c(
          sprintf("- **%s**-wheel drive has the best highway mileage among the three drivetrain types.", drv_best),
          sprintf("- **%s**-wheel drive has the worst, mainly due to the added weight of the drivetrain.", drv_worst),
          "- The gap between front- and four-wheel drive is nearly 9 mpg highway."
        )
      )),
      fig_facet = list(header = "Mileage by class", height = "auto",
        body = list(type = "figure", object = "fig_facet", height = 280)),
      fig_scatter = list(header = "Highway vs. city mileage", height = "auto", body = list(
        type = "figure", object = "fig_scatter", notes_width = 0.45, height = 102,
        notes = c(
          "- Highway and city mileage are closely correlated.",
          "- 4-wheel drive vehicles cluster at the low-mileage end.",
          "- Front-wheel drive vehicles cluster at the high-mileage end."
        )
      )),
      photos_2 = list(header = "More sample photos", height = "auto", body = list(
        type = "image", files = stock_photos, labels = stock_labels,
        width = 230
      ))
    )
  )

  spec
}

readme_example_poster <- function(parts = readme_example_parts()) {
  poster(
    readme_example_spec(parts),
    objects = readme_example_objects(parts),
    theme = theme_green(base_size = 24),
    base_dir = parts$img_dir
  )
}
