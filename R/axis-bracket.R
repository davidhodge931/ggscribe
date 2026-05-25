# axis_bracket ----------------------------------------------------------------

#' Annotate an axis bracket
#'
#' Draws one or more brackets along a floating axis line. Each bracket spans
#' `min(breaks)` to `max(breaks)` with caps at every break value.
#' Requires `coord_cartesian(clip = "off")`.
#'
#' Caps always point in the positive direction by default (right for
#' `xintercept`, up for `yintercept`). Use a negative `length` to flip them.
#'
#' @param xintercept One or more x positions for vertical axis lines, in data
#'   coordinates or wrapped in [I()] for normalised panel coordinates (npc).
#'   May be a vector; each value produces a separate axis.
#' @param yintercept One or more y positions for horizontal axis lines, in data
#'   coordinates or wrapped in [I()] for normalised panel coordinates (npc).
#'   May be a vector; each value produces a separate axis.
#' @param breaks A numeric vector of length >= 2 in data coordinates, or
#'   wrapped in [I()] for npc. The bar spans `min(breaks)` to `max(breaks)`;
#'   caps are drawn at every break value. Pass a list the same length as the
#'   total number of axes to use different breaks per axis.
#' @param length Length of the bracket caps. Supports `rel()`. Negative values
#'   flip the cap direction. Defaults to `rel(1)`. May be a vector the same
#'   length as the number of axes.
#' @param colour Inherits from `axis.ticks` in the set theme (falling back
#'   through `axis.line` and `line`). May be a vector the same length as the
#'   number of axes.
#' @param linewidth Inherits from `axis.ticks` in the set theme. Supports
#'   `rel()`. May be a vector the same length as the number of axes.
#' @param linetype Inherits from `axis.ticks` in the set theme. May be a
#'   vector the same length as the number of axes.
#' @param layout Controls which panels the annotation appears in. `NULL`
#'   (default) repeats in all panels. An integer targets a specific panel.
#'   `"fixed"` repeats in all panels ignoring faceting variables. See
#'   [ggplot2::layer()] for full details.
#'
#' @return A list of ggplot2 annotation layers.
#' @seealso [axis_line()], [axis_ticks()],
#'   [axis_text()], [reference_line()], [panel_shade()]
#' @export
axis_bracket <- function(
    xintercept = NULL,
    yintercept = NULL,
    breaks,
    length     = ggplot2::rel(1),
    colour     = NULL,
    linewidth  = NULL,
    linetype   = NULL,
    layout     = NULL
) {
  groups   <- .build_axis_groups(xintercept, yintercept)
  n_groups <- length(groups)

  rec         <- .reconcile(groups, breaks)
  groups      <- rec$groups
  breaks_list <- rec$breaks_list
  n_groups    <- rec$n

  for (g in seq_len(n_groups)) {
    if (length(breaks_list[[g]]$vals) < 2) {
      rlang::abort(glue::glue(
        "Each element of `breaks` must have at least 2 values. ",
        "Group {g} has {length(breaks_list[[g]]$vals)}."
      ))
    }
  }

  current_theme <- ggplot2::theme_get()

  line_hierarchy <- c(
    "axis.line.x", "axis.line",
    "axis.ticks.x", "axis.ticks",
    "line"
  )
  resolved_element <- .resolve_theme_el(line_hierarchy, current_theme) %||%
    list(colour = "#333333FF", linewidth = 0.5, linetype = 1)

  theme_colour    <- resolved_element$colour   %||% "#333333FF"
  theme_linewidth <- resolved_element$linewidth %||% 0.5
  theme_linetype  <- resolved_element$linetype  %||% 1

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

  len <- .resolve_length(length, n_groups, theme_length_pts)

  colour_vec    <- rep_len(colour    %||% theme_colour,    n_groups)
  linewidth_vec <- if (is.null(linewidth)) {
    rep_len(theme_linewidth, n_groups)
  } else if (inherits(linewidth, "rel")) {
    rep_len(as.numeric(linewidth), n_groups) * theme_linewidth
  } else {
    rep_len(linewidth, n_groups)
  }
  linetype_vec  <- rep_len(linetype  %||% theme_linetype,  n_groups)

  unlist(lapply(seq_len(n_groups), \(g) {
    grp          <- groups[[g]]
    brks         <- breaks_list[[g]]
    flip         <- len$flip[[g]]
    cap_length   <- grid::unit(len$pts[[g]], "pt")
    perp         <- .perp_unit(grp)
    bracket_from <- min(brks$vals)
    bracket_to   <- max(brks$vals)

    gp <- grid::gpar(
      col     = colour_vec[[g]],
      lwd     = linewidth_vec[[g]] * ggplot2::.pt,
      lty     = linetype_vec[[g]],
      lineend = "square"
    )

    # ---- Bar ----------------------------------------------------------------
    # Bar runs along the break axis (perpendicular to the intercept axis).
    # Uses bounding-box trick for data breaks: grob spans 0-1 npc, annotation
    # pins the data range.

    bar_layer <- if (grp$int_axis == "x") {
      # Vertical axis: bar is vertical, running along y
      if (brks$npc) {
        bar_grob <- grid::segmentsGrob(
          x0 = perp, x1 = perp,
          y0 = grid::unit(bracket_from, "npc"),
          y1 = grid::unit(bracket_to,   "npc"),
          gp = gp
        )
        .make_ann_layer(bar_grob,
                        list(xmin = if (grp$npc) -Inf else grp$intercept,
                             xmax = if (grp$npc) Inf  else grp$intercept,
                             ymin = -Inf, ymax = Inf), layout)
      } else {
        bar_grob <- grid::segmentsGrob(
          x0 = perp, x1 = perp,
          y0 = grid::unit(0, "npc"), y1 = grid::unit(1, "npc"),
          gp = gp
        )
        .make_ann_layer(bar_grob,
                        list(xmin = if (grp$npc) -Inf else grp$intercept,
                             xmax = if (grp$npc) Inf  else grp$intercept,
                             ymin = bracket_from, ymax = bracket_to), layout)
      }
    } else {
      # Horizontal axis: bar is horizontal, running along x
      if (brks$npc) {
        bar_grob <- grid::segmentsGrob(
          x0 = grid::unit(bracket_from, "npc"),
          x1 = grid::unit(bracket_to,   "npc"),
          y0 = perp, y1 = perp,
          gp = gp
        )
        .make_ann_layer(bar_grob,
                        list(xmin = -Inf, xmax = Inf,
                             ymin = if (grp$npc) -Inf else grp$intercept,
                             ymax = if (grp$npc) Inf  else grp$intercept), layout)
      } else {
        bar_grob <- grid::segmentsGrob(
          x0 = grid::unit(0, "npc"), x1 = grid::unit(1, "npc"),
          y0 = perp, y1 = perp,
          gp = gp
        )
        .make_ann_layer(bar_grob,
                        list(xmin = bracket_from, xmax = bracket_to,
                             ymin = if (grp$npc) -Inf else grp$intercept,
                             ymax = if (grp$npc) Inf  else grp$intercept), layout)
      }
    }

    # ---- Caps ---------------------------------------------------------------

    cap_layers <- lapply(brks$vals, \(bv) {
      along <- .along_unit(bv, brks$npc)

      cap_grob <- if (grp$int_axis == "x") {
        tip_x <- if (flip) perp - cap_length else perp + cap_length
        grid::segmentsGrob(
          x0 = tip_x, x1 = perp,
          y0 = along,  y1 = along,
          gp = gp
        )
      } else {
        tip_y <- if (flip) perp - cap_length else perp + cap_length
        grid::segmentsGrob(
          x0 = along, x1 = along,
          y0 = tip_y,  y1 = perp,
          gp = gp
        )
      }

      .make_ann_layer(cap_grob, .anno_pos(grp, bv, brks$npc), layout)
    })

    c(list(bar_layer), cap_layers)
  }), recursive = FALSE)
}
