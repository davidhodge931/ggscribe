# axis_ticks ------------------------------------------------------------------

#' Annotate axis ticks
#'
#' Draws axis ticks at specified break positions along a floating axis line.
#' Requires `coord_cartesian(clip = "off")`.
#'
#' Ticks always point in the positive direction by default (right for
#' `xintercept`, up for `yintercept`). Use a negative `length` to flip them
#' (e.g. `length = -rel(1)`).
#'
#' @param xintercept One or more x positions for vertical axis lines, in data
#'   coordinates or wrapped in [I()] for normalised panel coordinates (npc).
#'   May be a vector; each value produces a separate axis.
#' @param yintercept One or more y positions for horizontal axis lines, in data
#'   coordinates or wrapped in [I()] for normalised panel coordinates (npc).
#'   May be a vector; each value produces a separate axis.
#' @param breaks A numeric vector of break positions in data coordinates, or
#'   wrapped in [I()] for npc. Pass a list the same length as the total number
#'   of axes to use different breaks per axis.
#' @param length Total tick length. Supports `rel()`. Negative values flip the
#'   tick direction. Defaults to `rel(1)`. May be a vector recycled across all
#'   breaks in order.
#' @param colour Inherits from `axis.ticks` in the set theme. May be a vector
#'   recycled across all breaks in order.
#' @param linewidth Inherits from `axis.ticks` in the set theme. Supports
#'   `rel()`. May be a vector recycled across all breaks in order.
#' @param linetype Inherits from `axis.ticks` in the set theme. May be a
#'   vector recycled across all breaks in order.
#' @param arrow A [grid::arrow()] specification, or a list recycled across all
#'   breaks. The arrowhead points toward the axis line. Must use `list()` not
#'   `c()` when supplying multiple values.
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

  tick_hierarchy <- c("axis.ticks.y", "axis.ticks.x", "axis.ticks")
  tick_el <- .resolve_theme_el(tick_hierarchy, current_theme) %||%
    list(colour = "black", linewidth = 0.5, linetype = 1)

  len_el <- .resolve_theme_el(c("axis.ticks.length.x", "axis.ticks.length"), current_theme)
  theme_length_pts <- if (is.null(len_el)) {
    0.5 * (current_theme$text$size %||% 11)
  } else if (inherits(len_el, "rel")) {
    spacing_pts <- as.numeric(grid::convertUnit(
      current_theme$spacing %||% grid::unit(5.5, "pt"), "pt"))
    as.numeric(len_el) * spacing_pts
  } else if (!inherits(len_el, "unit")) {
    if (is.numeric(len_el)) len_el else 0.5 * (current_theme$text$size %||% 11)
  } else {
    as.numeric(grid::convertUnit(len_el, "pt"))
  }

  theme_colour <- tick_el$colour    %||% "black"
  theme_lw     <- tick_el$linewidth %||% 0.5
  theme_lt     <- tick_el$linetype  %||% 1

  # ---- Per-break indexing --------------------------------------------------
  n_per_group <- vapply(breaks_list, \(b) length(b$vals), integer(1))
  n_total     <- sum(n_per_group)
  offsets     <- c(0L, cumsum(n_per_group[-length(n_per_group)]))

  # All style args recycle to n_total — one value per break across all axes
  colour_vec <- rep_len(colour   %||% theme_colour, n_total)
  lw_vec     <- if (is.null(linewidth)) {
    rep_len(theme_lw, n_total)
  } else if (inherits(linewidth, "rel")) {
    rep_len(as.numeric(linewidth), n_total) * theme_lw
  } else {
    rep_len(linewidth, n_total)
  }
  lt_vec <- rep_len(linetype %||% theme_lt, n_total)

  len_pts_vec <- if (inherits(length, "rel")) {
    abs(rep_len(as.numeric(length), n_total)) * theme_length_pts
  } else if (inherits(length, "unit")) {
    rep_len(abs(as.numeric(grid::convertUnit(length, "pt"))), n_total)
  } else {
    rep_len(abs(as.numeric(length)), n_total)
  }
  flip_vec <- if (inherits(length, "rel")) {
    rep_len(as.numeric(length) < 0, n_total)
  } else if (inherits(length, "unit")) {
    rep_len(as.numeric(grid::convertUnit(length, "pt")) < 0, n_total)
  } else {
    rep_len(as.numeric(length) < 0, n_total)
  }

  arrow_list <- if (inherits(arrow, "arrow")) {
    rep_len(list(arrow), n_total)
  } else if (is.list(arrow)) {
    rep_len(arrow, n_total)
  } else {
    rep_len(list(NULL), n_total)
  }

  # ---- Draw ----------------------------------------------------------------

  unlist(lapply(seq_len(n_groups), \(g) {
    grp    <- groups[[g]]
    brks   <- breaks_list[[g]]
    offset <- offsets[[g]]
    perp   <- .perp_unit(grp)

    lapply(seq_along(brks$vals), \(i) {
      gi   <- offset + i          # global break index
      bv   <- brks$vals[[i]]
      flip <- flip_vec[[gi]]
      tl   <- grid::unit(len_pts_vec[[gi]], "pt")
      along <- .along_unit(bv, brks$npc)

      gp <- grid::gpar(
        col     = colour_vec[[gi]],
        fill    = colour_vec[[gi]],
        lwd     = lw_vec[[gi]] * ggplot2::.pt,
        lty     = lt_vec[[gi]],
        lineend = "butt"
      )

      tick_grob <- if (grp$int_axis == "x") {
        tip_x <- if (flip) perp - tl else perp + tl
        grid::segmentsGrob(x0 = tip_x, x1 = perp,
                           y0 = along,  y1 = along,
                           gp = gp, arrow = arrow_list[[gi]])
      } else {
        tip_y <- if (flip) perp - tl else perp + tl
        grid::segmentsGrob(x0 = along,  x1 = along,
                           y0 = tip_y,  y1 = perp,
                           gp = gp, arrow = arrow_list[[gi]])
      }

      .make_ann_layer(tick_grob, .anno_pos(grp, bv, brks$npc), layout)
    })
  }), recursive = FALSE)
}
