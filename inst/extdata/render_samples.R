# 見本4本をまとめて PDF にする．
#
# ggposter の見本は，acposter (`examples/` の4本)・qtposter (`poster_*.qmd`) と
# **同じ内容・同じ順**で揃えてある (2026-08-31)．
#
#   1. poster_sample_howto.yml   カードの種類の一巡り
#   2. poster_sample_howto2.yml  入力と出力の早見表
#   3. poster_sample_howto3.yml  非対称な配置 (`grid:`)
#   4. poster_sample.yml         実際のポスターに近い例 (架空のデータ)
#
# 表や図のカードは R オブジェクトを要るので，ここで作ってから渡す
# (YAML は「何をどこに置くか」だけを持ち，解析は R に残す，という分担)．
#
# 使い方:
#   source(system.file("extdata", "render_samples.R", package = "ggposter"))
#   render_ggposter_samples(out_dir = ".")

render_ggposter_samples <- function(out_dir = ".",
                                    which = c("howto", "howto2", "howto3", "sample"),
                                    scale = 1) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  which <- match.arg(which, several.ok = TRUE)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  extdata <- function(f) {
    p <- system.file("extdata", f, package = "ggposter")
    if (nzchar(p)) p else file.path("inst", "extdata", f)   # 開発中 (load_all) 用
  }

  # --- 見本が参照する R オブジェクト -----------------------------------------
  objs <- sample_objects()

  jobs <- list(
    howto  = list(yml = "poster_sample_howto.yml",  pdf = "poster_sample_howto.pdf"),
    howto2 = list(yml = "poster_sample_howto2.yml", pdf = "poster_sample_howto2.pdf"),
    howto3 = list(yml = "poster_sample_howto3.yml", pdf = "poster_sample_howto3.pdf"),
    sample = list(yml = "poster_sample.yml",        pdf = "poster_sample.pdf")
  )

  out <- character(0)
  for (nm in which) {
    job <- jobs[[nm]]
    p <- poster(extdata(job$yml), objects = objs)
    f <- file.path(out_dir, job$pdf)
    render_poster(p, f, scale = scale)
    message("created: ", f)
    out <- c(out, f)
  }
  invisible(out)
}

# 見本が名前で参照する図・表をまとめて作る．
# **名前は YAML の `object:` と一字一句そろえる** (食い違うとその場でエラーになる)．
sample_objects <- function() {
  ggplot2::theme_set(ggplot2::theme_bw())
  mpg <- ggplot2::mpg

  # 模式図 (入力と出力の見本で使う)．カードの並びを四角で描くだけのもの．
  boxes_plot <- function(df) {
    ggplot2::ggplot(df, ggplot2::aes(xmin = xmin, xmax = xmax,
                                     ymin = ymin, ymax = ymax)) +
      ggplot2::geom_rect(fill = "#E8F5E9", colour = "#2E7D32", linewidth = 0.8) +
      ggplot2::geom_text(ggplot2::aes(x = (xmin + xmax) / 2,
                                      y = (ymin + ymax) / 2,
                                      label = label), size = 5) +
      ggplot2::coord_equal(xlim = c(0, 2), ylim = c(0, 2)) +
      ggplot2::theme_void()
  }

  cols_df <- data.frame(
    label = c("a", "b", "c"),
    xmin  = c(0.05, 0.05, 1.05), xmax = c(0.95, 0.95, 1.95),
    ymin  = c(1.05, 0.05, 1.05), ymax = c(1.95, 0.95, 1.95)
  )
  grid_df <- data.frame(
    label = c("a (w: 2)", "b", "c"),
    xmin  = c(0.05, 0.05, 1.05), xmax = c(1.95, 0.95, 1.95),
    ymin  = c(1.05, 0.05, 0.05), ymax = c(1.95, 0.95, 0.95)
  )

  list(
    # poster_sample_howto.yml
    howto_fig = ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy)) +
      ggplot2::geom_point(colour = "#2E7D32"),
    howto_fig_notes = ggplot2::ggplot(mpg, ggplot2::aes(class, hwy)) +
      ggplot2::geom_boxplot(fill = "#A5D6A7") +
      ggplot2::labs(x = "Class", y = "Highway mpg"),
    howto_tbl_notes = data.frame(
      Drivetrain = c("f", "4", "r"),
      `Mean highway mpg` = c(28.2, 19.2, 21.0),
      check.names = FALSE
    ),
    # poster_sample_howto2.yml
    howto2_tbl = data.frame(Item = c("A", "B"), Value = c(1, 2)),
    howto2_fig = ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy)) +
      ggplot2::geom_point(colour = "#2E7D32"),
    howto2_cols = boxes_plot(cols_df),
    howto2_grid = boxes_plot(grid_df),
    # poster_sample.yml (架空のデータ．vignette と同じ中身を base R だけで作る)
    tbl_courses = data.frame(
      `Golf course` = c("A", "B", "C", "D"),
      `Established year` = c(1903, 1926, 1930, 1956),
      `Area (ha)` = c(20, 70, 55, 60),
      `Total grassland spp.` = c(79, 55, 78, 55),
      `Grassland spp. (mean +/- SD /m2)` = c("6.5 +/- 2.8", "2.9 +/- 2.0",
                                             "4.4 +/- 2.8", "2.1 +/- 1.5"),
      `All spp. (mean +/- SD /m2)` = c("11.2 +/- 4.6", "5.2 +/- 4.6",
                                       "8.1 +/- 5.1", "4.0 +/- 3.3"),
      check.names = FALSE
    ),
    tbl_community = data.frame(
      Community = paste0("com", 1:4),
      `No. of plots` = c(57, 38, 37, 68),
      `Grassland spp. (/m2)` = c("6.1 +/- 2.9", "5.3 +/- 2.7",
                                 "3.9 +/- 1.3", "1.4 +/- 0.7"),
      `All spp. (/m2)` = c("10.7 +/- 4.7", "10.6 +/- 5.2",
                           "6.9 +/- 3.1", "2.4 +/- 1.6"),
      `Cutting height (cm)` = c("12.9 +/- 15.3", "22.7 +/- 19.2",
                                "5.1 +/- 4.8", "5.2 +/- 5.0"),
      `Cutting freq. (/yr)` = c("4.2 +/- 6.1", "4.4 +/- 4.4",
                                "10.1 +/- 5.3", "14.6 +/- 5.5"),
      check.names = FALSE
    ),
    fig_cutting = {
      d <- community_points()
      ggplot2::ggplot(d, ggplot2::aes(cutting_freq, cutting_height)) +
        ggplot2::geom_point(alpha = 0.6, colour = "#2E7D32") +
        ggplot2::facet_wrap(~Community, nrow = 1) +
        ggplot2::labs(x = "Cutting frequency (/yr)", y = "Cutting height (cm)")
    },
    fig_ordination = {
      d <- ordination_points()
      ggplot2::ggplot(d, ggplot2::aes(NMDS1, NMDS2, colour = Community)) +
        ggplot2::geom_point(alpha = 0.7) +
        ggplot2::stat_ellipse() +
        ggplot2::theme(legend.position = "bottom")
    }
  )
}

# 報告された平均 ± SD に合う**説明用の点**を作る (生の調査データではない)．
community_params <- function() {
  data.frame(
    Community   = paste0("com", 1:4),
    n           = c(57, 38, 37, 68),
    height_mean = c(12.9, 22.7, 5.1, 5.2),
    height_sd   = c(15.3, 19.2, 4.8, 5.0),
    freq_mean   = c(4.2, 4.4, 10.1, 14.6),
    freq_sd     = c(6.1, 4.4, 5.3, 5.5),
    stringsAsFactors = FALSE
  )
}

community_points <- function() {
  set.seed(42)
  p <- community_params()
  do.call(rbind, lapply(seq_len(nrow(p)), function(i) {
    data.frame(
      Community      = p$Community[i],
      cutting_height = pmax(0, stats::rnorm(p$n[i], p$height_mean[i], p$height_sd[i])),
      cutting_freq   = pmax(1, stats::rnorm(p$n[i], p$freq_mean[i], p$freq_sd[i])),
      stringsAsFactors = FALSE
    )
  }))
}

ordination_points <- function() {
  set.seed(42)
  p <- community_params()
  centres <- list(com1 = c(1, 1), com2 = c(1, -1),
                  com3 = c(-1, 0.5), com4 = c(-1.2, -1))
  do.call(rbind, lapply(seq_len(nrow(p)), function(i) {
    ct <- centres[[p$Community[i]]]
    data.frame(
      Community = p$Community[i],
      NMDS1 = stats::rnorm(p$n[i], ct[1], 0.4),
      NMDS2 = stats::rnorm(p$n[i], ct[2], 0.4),
      stringsAsFactors = FALSE
    )
  }))
}
