# reference_line --------------------------------------------------------------

#' Annotate a reference line
#'
#' Draws one or more reference lines within the panel, with style defaults
#' taken from the `axis.line` element of the set theme.
#'
#' The arrow (if any) points in the positive direction by default — rightward
#' for `xintercept` lines, upward for `yintercept` lines.
#'
#' @param xintercept One or more x positions for vertical reference lines, in
#'   data coordinates or wrapped in [I()] for normalised panel coordinates
#'   (npc). May be a vector; each value produces a separate line.
#' @param yintercept One or more y positions for horizontal reference lines, in
#'   data coordinates or wrapped in [I()] for normalised panel coordinates
#'   (npc). May be a vector; each value produces a separate line.
#' @param colour Inherits from `axis.line` in the set theme. May be a vector
#'   the same length as the total number of lines.
#' @param linewidth Inherits from `axis.line` in the set theme. Supports
#'   `rel()`. May be a vector the same length as the total number of lines.
#' @param linetype Defaults to `"dashed"`. May be a vector the same length as
#'   the total number of lines.
#' @param arrow A [grid::arrow()] specification, or a list the same length as
#'   the total number of lines. Must use `list()` not `c()` when supplying
#'   multiple values.
#'   E.g. `grid::arrow(angle = 15, length = unit(1.5, "mm"), type = "closed")`.
#' @param layout Controls which panels the annotation appears in. `NULL`
#'   (default) repeats in all panels. An integer targets a specific panel.
#'   `"fixed"` repeats in all panels ignoring faceting variables. See
#'   [ggplot2::layer()] for full details.
#'
#' @return A list of ggplot2 annotation layers.
#' @seealso [axis_line()], [axis_ticks()],
#'   [axis_text()], [axis_bracket()], [panel_shade()]
#' @export
reference_line <- function(
    xintercept = NULL,
    yintercept = NULL,
    colour     = NULL,
    linewidth  = NULL,
    linetype   = "dashed",
    arrow      = NULL,
    layout     = NULL
) {
  groups   <- .build_axis_groups(xintercept, yintercept)
  n_groups <- length(groups)

  current_theme <- ggplot2::theme_get()

  resolved_element <- .resolve_theme_el(
    c("axis.line.x", "axis.line.y", "axis.line"), current_theme
  ) %||% list(colour = "black", linewidth = 0.5, linetype = 1)

  theme_colour    <- resolved_element$colour   %||% "black"
  theme_linewidth <- resolved_element$linewidth %||% 0.5

  colour_vec    <- rep_len(colour   %||% theme_colour,    n_groups)
  linewidth_vec <- if (is.null(linewidth)) {
    rep_len(theme_linewidth, n_groups)
  } else if (inherits(linewidth, "rel")) {
    rep_len(as.numeric(linewidth), n_groups) * theme_linewidth
  } else {
    rep_len(linewidth, n_groups)
  }
  linetype_vec  <- rep_len(linetype, n_groups)

  arrow_list <- if (inherits(arrow, "arrow")) {
    rep_len(list(arrow), n_groups)
  } else if (is.list(arrow)) {
    rep_len(arrow, n_groups)
  } else {
    rep_len(list(NULL), n_groups)
  }

  lapply(seq_len(n_groups), \(g) {
    grp  <- groups[[g]]
    perp <- .perp_unit(grp)

    gp <- grid::gpar(
      col     = colour_vec[[g]],
      fill    = colour_vec[[g]],
      lwd     = linewidth_vec[[g]] * ggplot2::.pt,
      lty     = linetype_vec[[g]],
      lineend = "butt"
    )

    # Line drawn in positive direction so arrow points right (x) or up (y)
    line_grob <- if (grp$int_axis == "x") {
      grid::segmentsGrob(
        x0 = perp, x1 = perp,
        y0 = grid::unit(0, "npc"), y1 = grid::unit(1, "npc"),
        gp = gp, arrow = arrow_list[[g]]
      )
    } else {
      grid::segmentsGrob(
        x0 = grid::unit(0, "npc"), x1 = grid::unit(1, "npc"),
        y0 = perp, y1 = perp,
        gp = gp, arrow = arrow_list[[g]]
      )
    }

    anno_pos <- if (grp$int_axis == "x") {
      list(
        xmin = if (grp$npc) -Inf else grp$intercept,
        xmax = if (grp$npc) Inf  else grp$intercept,
        ymin = -Inf, ymax = Inf
      )
    } else {
      list(
        xmin = -Inf, xmax = Inf,
        ymin = if (grp$npc) -Inf else grp$intercept,
        ymax = if (grp$npc) Inf  else grp$intercept
      )
    }

    .make_ann_layer(line_grob, anno_pos, layout)
  })
}
