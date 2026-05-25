# axis_ticks ------------------------------------------------------------------

#' Annotate axis ticks
#'
#' Draws axis ticks at specified break positions along a floating axis line.
#' Requires `coord_cartesian(clip = "off")`.
#'
#' Ticks always point in the positive direction by default (right for
#' `xintercept`, up for `yintercept`). Use a negative `length` (e.g.
#' `length = -rel(1)`) to flip them left or down.
#'
#' @param xintercept One or more x positions for vertical axis lines, in data
#'   coordinates or wrapped in [I()] for normalised panel coordinates (npc).
#'   May be a vector; each value produces a separate axis.
#' @param yintercept One or more y positions for horizontal axis lines, in data
#'   coordinates or wrapped in [I()] for normalised panel coordinates (npc).
#'   May be a vector; each value produces a separate axis.
#' @param breaks A numeric vector of break positions in data coordinates, or
#'   wrapped in [I()] for npc. Pass a list the same length as the total number
#'   of axes (xintercept + yintercept combined) to use different breaks per
#'   axis.
#' @param length Total tick length. Supports `rel()`. Negative values flip the
#'   tick direction. Defaults to `rel(1)` (theme tick length). May be a vector
#'   the same length as the number of axes.
#' @param colour Inherits from `axis.ticks` in the set theme. May be a vector
#'   the same length as the number of axes — one style per axis, applied to
#'   all breaks on that axis.
#' @param linewidth Inherits from `axis.ticks` in the set theme. Supports
#'   `rel()`. May be a vector the same length as the number of axes.
#' @param linetype Inherits from `axis.ticks` in the set theme. May be a
#'   vector the same length as the number of axes.
#' @param arrow A [grid::arrow()] specification, or a list the same length as
#'   the number of axes. The arrowhead points toward the axis line. Must use
#'   `list()` not `c()` when supplying multiple values.
#'   E.g. `grid::arrow(angle = 15, length = unit(1.5, "mm"), type = "closed")`.
#' @param layout Controls which panels the annotation appears in. `NULL`
#'   (default) repeats in all panels. An integer targets a specific panel.
#'   `"fixed"` repeats in all panels ignoring faceting variables. See
#'   [ggplot2::layer()] for full details.
#'
#' @return A list of ggplot2 annotation layers.
#' @seealso [axis_line()], [axis_text()],
#'   [axis_bracket()], [reference_line()], [panel_shade()]
#' @export
axis_ticks <- function(
    xintercept = NULL,
    yintercept = NULL,
    breaks,
    length     = ggplot2::rel(1),
    colour     = NULL,
    linewidth  = NULL,
    linetype   = NULL,
    arrow      = NULL,
    layout     = NULL
) {
  groups   <- .build_axis_groups(xintercept, yintercept)
  n_groups <- length(groups)

  rec         <- .reconcile(groups, breaks)
  groups      <- rec$groups
  breaks_list <- rec$breaks_list
  n_groups    <- rec$n

  current_theme <- ggplot2::theme_get()

  # Theme resolution — use axis.ticks.{break_axis} since breaks mark that scale
  .get_tick_theme <- function(int_axis) {
    break_axis <- if (int_axis == "x") "y" else "x"
    hierarchy  <- c(paste0("axis.ticks.", break_axis), "axis.ticks")
    el <- .resolve_theme_el(hierarchy, current_theme)
    el %||% list(colour = "black", linewidth = 0.5, linetype = 1)
  }

  .get_length_theme_pts <- function(int_axis) {
    break_axis <- if (int_axis == "x") "y" else "x"
    hierarchy  <- c(paste0("axis.ticks.length.", break_axis), "axis.ticks.length")
    tl <- .resolve_theme_el(hierarchy, current_theme)
    if (is.null(tl)) {
      0.5 * (current_theme$text$size %||% 11)
    } else if (inherits(tl, "rel")) {
      spacing_pts <- as.numeric(grid::convertUnit(
        current_theme$spacing %||% grid::unit(5.5, "pt"), "pt"))
      as.numeric(tl) * spacing_pts
    } else if (!inherits(tl, "unit")) {
      if (is.numeric(tl)) tl else 0.5 * (current_theme$text$size %||% 11)
    } else {
      as.numeric(grid::convertUnit(tl, "pt"))
    }
  }

  # Use first group's int_axis for theme length (or average — both fine for rel)
  theme_length_pts <- .get_length_theme_pts(groups[[1]]$int_axis)
  len              <- .resolve_length(length, n_groups, theme_length_pts)

  # Style args — one per group
  th1 <- .get_tick_theme(groups[[1]]$int_axis)
  colour_vec    <- rep_len(colour    %||% th1$colour,   n_groups)
  linewidth_vec <- if (is.null(linewidth)) {
    rep_len(th1$linewidth %||% 0.5, n_groups)
  } else if (inherits(linewidth, "rel")) {
    rep_len(as.numeric(linewidth), n_groups) * (th1$linewidth %||% 0.5)
  } else {
    rep_len(linewidth, n_groups)
  }
  linetype_vec  <- rep_len(linetype  %||% th1$linetype %||% 1, n_groups)

  arrow_list <- if (inherits(arrow, "arrow")) {
    rep_len(list(arrow), n_groups)
  } else if (is.list(arrow)) {
    rep_len(arrow, n_groups)
  } else {
    rep_len(list(NULL), n_groups)
  }

  unlist(lapply(seq_len(n_groups), \(g) {
    grp        <- groups[[g]]
    brks       <- breaks_list[[g]]
    flip       <- len$flip[[g]]
    tick_len   <- grid::unit(len$pts[[g]], "pt")
    perp       <- .perp_unit(grp)

    gp <- grid::gpar(
      col     = colour_vec[[g]],
      fill    = colour_vec[[g]],
      lwd     = linewidth_vec[[g]] * ggplot2::.pt,
      lty     = linetype_vec[[g]],
      lineend = "butt"
    )
    tick_arrow <- arrow_list[[g]]

    lapply(seq_along(brks$vals), \(i) {
      bv    <- brks$vals[[i]]
      along <- .along_unit(bv, brks$npc)

      # Tip is always in the positive direction; flip reverses it.
      # Segment drawn tip → base so arrow points at axis line.
      tick_grob <- if (grp$int_axis == "x") {
        tip_x <- if (flip) perp - tick_len else perp + tick_len
        grid::segmentsGrob(
          x0 = tip_x, x1 = perp,
          y0 = along,  y1 = along,
          gp = gp, arrow = tick_arrow
        )
      } else {
        tip_y <- if (flip) perp - tick_len else perp + tick_len
        grid::segmentsGrob(
          x0 = along,  x1 = along,
          y0 = tip_y,  y1 = perp,
          gp = gp, arrow = tick_arrow
        )
      }

      .make_ann_layer(tick_grob, .anno_pos(grp, bv, brks$npc), layout)
    })
  }), recursive = FALSE)
}
