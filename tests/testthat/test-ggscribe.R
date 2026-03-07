library(testthat)
library(ggplot2)

# Helper: base plot used across most tests
base_plot <- function() {
  ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    coord_cartesian(clip = "off")
}

# Helper: silently add layers and check plot builds without error
expect_builds <- function(layers) {
  p <- base_plot()
  for (l in layers) p <- p + l
  expect_no_error(ggplot_build(p))
}


# ==============================================================================
# is_col_dark / is_panel_dark
# ==============================================================================

test_that("is_col_dark returns FALSE for NULL", {
  expect_false(is_col_dark(NULL))
})

test_that("is_col_dark returns FALSE for empty", {
  expect_false(is_col_dark(character(0)))
})

test_that("is_col_dark returns TRUE for dark colours", {
  expect_true(is_col_dark("black"))
  expect_true(is_col_dark("#000000"))
  expect_true(is_col_dark("#1a1a2e"))
})

test_that("is_col_dark returns FALSE for light colours", {
  expect_false(is_col_dark("white"))
  expect_false(is_col_dark("#FFFFFF"))
  expect_false(is_col_dark("grey95"))
})

test_that("is_panel_dark returns FALSE for theme_classic", {
  set_theme(theme_classic())
  expect_false(is_panel_dark())
})

test_that("is_panel_dark accepts explicit theme argument", {
  expect_false(is_panel_dark(theme = theme_classic()))
})

test_that("is_panel_dark rejects unnamed extra arguments", {
  expect_error(is_panel_dark(theme_classic()))
})


# ==============================================================================
# annotate_axis_line — axis line mode
# ==============================================================================

test_that("annotate_axis_line builds with position = 'bottom'", {
  set_theme(theme_classic())
  expect_builds(annotate_axis_line(position = "bottom"))
})

test_that("annotate_axis_line builds with all four positions", {
  set_theme(theme_classic())
  for (pos in c("top", "bottom", "left", "right")) {
    expect_builds(annotate_axis_line(position = pos))
  }
})

test_that("annotate_axis_line returns a list", {
  set_theme(theme_classic())
  result <- annotate_axis_line(position = "bottom")
  expect_type(result, "list")
})

test_that("annotate_axis_line respects xmin/xmax for partial lines", {
  set_theme(theme_classic())
  expect_builds(annotate_axis_line(position = "bottom", xmin = 2, xmax = 4))
})

test_that("annotate_axis_line respects ymin/ymax for partial lines", {
  set_theme(theme_classic())
  expect_builds(annotate_axis_line(position = "left", ymin = 15, ymax = 30))
})

test_that("annotate_axis_line errors if both x and y specified without xend/yend", {
  set_theme(theme_classic())
  expect_error(annotate_axis_line(x = 3, y = 15))
})

test_that("annotate_axis_line errors if x and xmin/xmax both specified", {
  set_theme(theme_classic())
  expect_error(annotate_axis_line(x = 3, xmin = 2))
})

test_that("annotate_axis_line errors if y and ymin/ymax both specified", {
  set_theme(theme_classic())
  expect_error(annotate_axis_line(y = 15, ymin = 10))
})

test_that("annotate_axis_line errors with no position, x, or y", {
  set_theme(theme_classic())
  expect_error(annotate_axis_line())
})

test_that("annotate_axis_line element_to = 'transparent' adds theme layer", {
  set_theme(theme_classic())
  result <- annotate_axis_line(position = "bottom", element_to = "transparent")
  has_theme <- any(purrr::map_lgl(result, \(x) inherits(x, "theme")))
  expect_true(has_theme)
})

test_that("annotate_axis_line element_to = 'blank' adds theme layer", {
  set_theme(theme_classic())
  result <- annotate_axis_line(position = "bottom", element_to = "blank")
  has_theme <- any(purrr::map_lgl(result, \(x) inherits(x, "theme")))
  expect_true(has_theme)
})

test_that("annotate_axis_line element_to = 'keep' does not add theme layer", {
  set_theme(theme_classic())
  result <- annotate_axis_line(position = "bottom", element_to = "keep")
  has_theme <- any(purrr::map_lgl(result, \(x) inherits(x, "theme")))
  expect_false(has_theme)
})

test_that("annotate_axis_line warns when theme has no axis.line (theme_grey)", {
  set_theme(theme_grey())
  expect_warning(
    expect_warning(annotate_axis_line(position = "bottom"), "colour"),
    "linewidth"
  )
})

test_that("annotate_axis_line does not warn when colour supplied explicitly", {
  set_theme(theme_grey())
  expect_no_warning(annotate_axis_line(position = "bottom", colour = "black", linewidth = 0.5))
})

test_that("annotate_axis_line builds with normalized x coordinate", {
  set_theme(theme_classic())
  expect_builds(annotate_axis_line(x = I(0.5)))
})


# ==============================================================================
# annotate_axis_line — segment / curve mode
# ==============================================================================

test_that("annotate_axis_line segment mode builds", {
  set_theme(theme_classic())
  expect_builds(annotate_axis_line(x = 2, y = 15, xend = 5, yend = 30))
})

test_that("annotate_axis_line curve mode builds", {
  set_theme(theme_classic())
  expect_builds(annotate_axis_line(x = 2, y = 15, xend = 5, yend = 30, curvature = 0.3))
})

test_that("annotate_axis_line segment mode returns single-element list", {
  set_theme(theme_classic())
  result <- annotate_axis_line(x = 2, y = 15, xend = 5, yend = 30)
  expect_length(result, 1)
})

test_that("annotate_axis_line segment mode warns when theme has no axis.line", {
  set_theme(theme_grey())
  expect_warning(
    expect_warning(
      annotate_axis_line(x = 2, y = 15, xend = 5, yend = 30),
      "colour"
    ),
    "linewidth"
  )
})

test_that("annotate_axis_line curvature angle and ncp are accepted", {
  set_theme(theme_classic())
  expect_builds(annotate_axis_line(x = 2, y = 15, xend = 5, yend = 30,
                                   curvature = 0.3, angle = 45, ncp = 10))
})


# ==============================================================================
# annotate_axis_ticks
# ==============================================================================

test_that("annotate_axis_ticks builds with position and x", {
  set_theme(theme_classic())
  expect_builds(annotate_axis_ticks(position = "bottom", x = c(2, 3, 4, 5)))
})

test_that("annotate_axis_ticks builds with position and y", {
  set_theme(theme_classic())
  expect_builds(annotate_axis_ticks(position = "left", y = c(15, 20, 25)))
})

test_that("annotate_axis_ticks infers position from x", {
  set_theme(theme_classic())
  result <- annotate_axis_ticks(x = c(2, 3, 4))
  expect_type(result, "list")
  expect_gt(length(result), 0)
})

test_that("annotate_axis_ticks infers position from y", {
  set_theme(theme_classic())
  result <- annotate_axis_ticks(y = c(15, 20, 25))
  expect_type(result, "list")
  expect_gt(length(result), 0)
})

test_that("annotate_axis_ticks errors with both x and y", {
  set_theme(theme_classic())
  expect_error(annotate_axis_ticks(x = c(2, 3), y = c(15, 20)))
})

test_that("annotate_axis_ticks errors with no position, x, or y", {
  set_theme(theme_classic())
  expect_error(annotate_axis_ticks())
})

test_that("annotate_axis_ticks errors if y provided for bottom position", {
  set_theme(theme_classic())
  expect_error(annotate_axis_ticks(position = "bottom", y = c(15, 20)))
})

test_that("annotate_axis_ticks errors if x provided for left position", {
  set_theme(theme_classic())
  expect_error(annotate_axis_ticks(position = "left", x = c(2, 3)))
})

test_that("annotate_axis_ticks returns empty list for empty breaks", {
  set_theme(theme_classic())
  result <- annotate_axis_ticks(position = "bottom", x = numeric(0))
  expect_equal(result, list())
})

test_that("annotate_axis_ticks returns one grob per break", {
  set_theme(theme_classic())
  breaks <- c(2, 3, 4, 5)
  result <- annotate_axis_ticks(position = "bottom", x = breaks)
  n_grobs <- sum(purrr::map_lgl(result, \(x) inherits(x, "Layer")))
  expect_equal(n_grobs, length(breaks))
})

test_that("annotate_axis_ticks minor = TRUE builds", {
  set_theme(theme_classic())
  expect_builds(annotate_axis_ticks(position = "bottom", x = seq(2, 5, 0.5), minor = TRUE))
})

test_that("annotate_axis_ticks negative length flips direction", {
  set_theme(theme_classic())
  expect_builds(annotate_axis_ticks(position = "bottom", x = c(2, 3, 4),
                                    tick_length = grid::unit(-5, "pt")))
})

test_that("annotate_axis_ticks rel() length builds", {
  set_theme(theme_classic())
  expect_builds(annotate_axis_ticks(position = "bottom", x = c(2, 3, 4),
                                    tick_length = rel(1.5)))
})

test_that("annotate_axis_ticks element_to adds theme layer", {
  set_theme(theme_classic())
  result <- annotate_axis_ticks(position = "bottom", x = c(2, 3, 4),
                                element_to = "transparent")
  has_theme <- any(purrr::map_lgl(result, \(x) inherits(x, "theme")))
  expect_true(has_theme)
})

test_that("annotate_axis_ticks warns when theme has no axis.ticks", {
  set_theme(theme_void())
  expect_warning(
    expect_warning(
      annotate_axis_ticks(position = "bottom", x = c(2, 3, 4)),
      "colour"
    ),
    "linewidth"
  )
})

test_that("annotate_axis_ticks normalized x coordinates build", {
  set_theme(theme_classic())
  expect_builds(annotate_axis_ticks(position = "bottom", x = I(c(0.25, 0.5, 0.75))))
})

test_that("annotate_axis_ticks normalized coordinates out of range error", {
  set_theme(theme_classic())
  expect_error(annotate_axis_ticks(position = "bottom", x = I(c(0.5, 1.5))))
})


# ==============================================================================
# annotate_axis_text
# ==============================================================================

test_that("annotate_axis_text builds in axis mode", {
  set_theme(theme_classic())
  expect_builds(annotate_axis_text(position = "bottom", x = c(2, 3, 4, 5)))
})

test_that("annotate_axis_text builds for all positions", {
  set_theme(theme_classic())
  expect_builds(annotate_axis_text(position = "bottom", x = c(2, 3, 4)))
  expect_builds(annotate_axis_text(position = "top",    x = c(2, 3, 4)))
  expect_builds(annotate_axis_text(position = "left",   y = c(15, 20, 25)))
  expect_builds(annotate_axis_text(position = "right",  y = c(15, 20, 25)))
})

test_that("annotate_axis_text infers position from x", {
  set_theme(theme_classic())
  result <- annotate_axis_text(x = c(2, 3, 4))
  expect_type(result, "list")
})

test_that("annotate_axis_text infers position from y", {
  set_theme(theme_classic())
  result <- annotate_axis_text(y = c(15, 20, 25))
  expect_type(result, "list")
})

test_that("annotate_axis_text arbitrary positioning mode builds", {
  set_theme(theme_classic())
  expect_builds(annotate_axis_text(x = 3.215, y = 21.4, label = "here"))
})

test_that("annotate_axis_text arbitrary mode with multiple points builds", {
  set_theme(theme_classic())
  expect_builds(annotate_axis_text(x = c(2, 4), y = c(15, 30),
                                   label = c("low", "high")))
})

test_that("annotate_axis_text errors if x and y lengths differ", {
  set_theme(theme_classic())
  expect_error(annotate_axis_text(x = c(2, 3), y = c(15)))
})

test_that("annotate_axis_text errors with no args", {
  set_theme(theme_classic())
  expect_error(annotate_axis_text())
})

test_that("annotate_axis_text returns empty list for empty breaks", {
  set_theme(theme_classic())
  result <- annotate_axis_text(position = "bottom", x = numeric(0))
  expect_equal(result, list())
})

test_that("annotate_axis_text custom labels work", {
  set_theme(theme_classic())
  expect_builds(annotate_axis_text(position = "bottom", x = c(2, 3, 4),
                                   label = c("two", "three", "four")))
})

test_that("annotate_axis_text label function works", {
  set_theme(theme_classic())
  expect_builds(annotate_axis_text(position = "bottom", x = c(2, 3, 4),
                                   label = scales::comma))
})

test_that("annotate_axis_text errors if label length mismatches breaks", {
  set_theme(theme_classic())
  expect_error(annotate_axis_text(position = "bottom", x = c(2, 3, 4),
                                  label = c("a", "b")))
})

test_that("annotate_axis_text negative length flips inward", {
  set_theme(theme_classic())
  expect_builds(annotate_axis_text(position = "bottom", x = c(2, 3, 4),
                                   tick_length = grid::unit(-15, "pt")))
})

test_that("annotate_axis_text element_to adds theme layer in axis mode", {
  set_theme(theme_classic())
  result <- annotate_axis_text(position = "bottom", x = c(2, 3, 4),
                               element_to = "transparent")
  has_theme <- any(purrr::map_lgl(result, \(x) inherits(x, "theme")))
  expect_true(has_theme)
})

test_that("annotate_axis_text element_to ignored in arbitrary mode", {
  set_theme(theme_classic())
  # Should not error or warn
  expect_no_error(annotate_axis_text(x = 3, y = 20, label = "here",
                                     element_to = "transparent"))
})

test_that("annotate_axis_text normalized coordinates build", {
  set_theme(theme_classic())
  expect_builds(annotate_axis_text(position = "bottom", x = I(c(0.25, 0.5, 0.75))))
})

test_that("annotate_axis_text Date breaks format without error", {
  set_theme(theme_classic())
  dates <- as.Date(c("2023-01-01", "2023-06-01", "2024-01-01"))
  result <- annotate_axis_text(position = "bottom", x = dates)
  expect_type(result, "list")
})

test_that("annotate_axis_text combined: inward y labels + normalized top label builds", {
  set_theme(
    theme_classic() +
      theme(
        panel.heights = rep(grid::unit(50, "mm"), 100),
        panel.widths  = rep(grid::unit(75, "mm"), 100),
      )
  )
  p <- ggplot2::ggplot(
    ggplot2::mpg,
    ggplot2::aes(x = displ, y = hwy, colour = drv)
  ) +
    ggplot2::geom_point() +
    ggplot2::coord_cartesian(clip = "off") +
    annotate_axis_text(
      y           = c(20, 30, 40),
      element_to  = "blank",
      tick_length = rel(-1),
      hjust       = 0,
      vjust       = -0.5,
    ) +
    annotate_axis_text(
      position    = "top",
      x           = I(0),
      label       = "Highway mpg",
      tick_length = rel(0),
      hjust       = 0,
    ) +
    ggplot2::labs(title = "Fuel economy", subtitle = "Engine displacement vs highway mpg\n\n", y = NULL) +
    ggplot2::theme(plot.title.position = "panel")

  expect_no_error(ggplot_build(p))
})


# ==============================================================================
# annotate_panel_grid
# ==============================================================================

test_that("annotate_panel_grid builds with x breaks", {
  set_theme(theme_minimal())
  expect_builds(annotate_panel_grid(x = c(2, 3, 4, 5)))
})

test_that("annotate_panel_grid builds with y breaks", {
  set_theme(theme_minimal())
  expect_builds(annotate_panel_grid(y = c(15, 20, 25)))
})

test_that("annotate_panel_grid errors with no x or y", {
  set_theme(theme_minimal())
  expect_error(annotate_panel_grid())
})

test_that("annotate_panel_grid errors with both x and y", {
  set_theme(theme_minimal())
  expect_error(annotate_panel_grid(x = c(2, 3), y = c(15, 20)))
})

test_that("annotate_panel_grid returns empty list for empty breaks", {
  set_theme(theme_minimal())
  result <- annotate_panel_grid(x = numeric(0))
  expect_equal(result, list())
})

test_that("annotate_panel_grid minor = TRUE builds", {
  set_theme(theme_minimal())
  expect_builds(annotate_panel_grid(x = seq(2, 5, 0.5), minor = TRUE))
})

test_that("annotate_panel_grid element_to adds theme layer", {
  set_theme(theme_minimal())
  result <- annotate_panel_grid(y = c(15, 20, 25), element_to = "transparent")
  has_theme <- any(purrr::map_lgl(result, \(x) inherits(x, "theme")))
  expect_true(has_theme)
})

test_that("annotate_panel_grid partial lines with xmax build", {
  set_theme(theme_minimal())
  expect_builds(annotate_panel_grid(y = c(15, 25), xmax = I(0.5)))
})

test_that("annotate_panel_grid partial lines with data coordinates build", {
  set_theme(theme_minimal())
  expect_builds(annotate_panel_grid(y = c(15, 25), xmin = 2, xmax = 4))
})

test_that("annotate_panel_grid normalized x breaks build", {
  set_theme(theme_minimal())
  expect_builds(annotate_panel_grid(x = I(c(0.25, 0.5, 0.75))))
})

test_that("annotate_panel_grid normalized breaks out of range error", {
  set_theme(theme_minimal())
  expect_error(annotate_panel_grid(x = I(c(0.5, 1.5))))
})

test_that("annotate_panel_grid rel() linewidth builds", {
  set_theme(theme_minimal())
  expect_builds(annotate_panel_grid(y = c(15, 20), linewidth = rel(2)))
})

test_that("annotate_panel_grid custom colour builds", {
  set_theme(theme_minimal())
  expect_builds(annotate_panel_grid(y = c(15, 20, 25), colour = "steelblue"))
})


# ==============================================================================
# annotate_panel_shade
# ==============================================================================

test_that("annotate_panel_shade builds with defaults", {
  set_theme(theme_classic())
  expect_builds(annotate_panel_shade())
})

test_that("annotate_panel_shade returns single-element list", {
  set_theme(theme_classic())
  result <- annotate_panel_shade()
  expect_length(result, 1)
})

test_that("annotate_panel_shade builds with data range", {
  set_theme(theme_classic())
  expect_builds(annotate_panel_shade(xmin = 3, xmax = 4))
})

test_that("annotate_panel_shade builds with y range", {
  set_theme(theme_classic())
  expect_builds(annotate_panel_shade(ymin = 20, ymax = 30))
})

test_that("annotate_panel_shade builds with normalized coordinates", {
  set_theme(theme_classic())
  expect_builds(annotate_panel_shade(xmin = I(0.25), xmax = I(0.75)))
})

test_that("annotate_panel_shade builds with custom fill and alpha", {
  set_theme(theme_classic())
  expect_builds(annotate_panel_shade(fill = "steelblue", alpha = 0.1))
})

test_that("annotate_panel_shade errors mixing normalized and data x coords", {
  set_theme(theme_classic())
  expect_error(annotate_panel_shade(xmin = I(0.25), xmax = 4))
})

test_that("annotate_panel_shade errors mixing normalized and data y coords", {
  set_theme(theme_classic())
  expect_error(annotate_panel_shade(ymin = I(0.25), ymax = 30))
})

test_that("annotate_panel_shade errors for normalized coords out of range", {
  set_theme(theme_classic())
  expect_error(annotate_panel_shade(xmin = I(-0.1), xmax = I(0.75)))
})

test_that("annotate_panel_shade custom colour and linetype build", {
  set_theme(theme_classic())
  expect_builds(annotate_panel_shade(colour = "black", linetype = 2))
})

test_that("annotate_panel_shade rel() linewidth builds", {
  set_theme(theme_classic())
  expect_builds(annotate_panel_shade(linewidth = rel(2)))
})

test_that("annotate_panel_shade builds on dark theme", {
  set_theme(theme_dark())
  expect_builds(annotate_panel_shade())
})
