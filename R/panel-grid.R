# panel_grid ------------------------------------------------------------------

#' Annotate panel gridlines
#'
#' Draws gridlines at specified positions, with style defaults taken from the
#' `panel.grid.major` element of the set theme. Crop bounds (`xmin`, `xmax`,
#' `ymin`, `ymax`) both filter which lines are drawn and control how far they
#' run across the panel.
#'
#' @param xintercept One or more x positions for vertical gridlines, in data
#'   coordinates or wrapped in [I()] for normalised panel coordinates (npc).
#'   May be a vector.
#' @param yintercept One or more y positions for horizontal gridlines, in data
#'   coordinates or wrapped in [I()] for normalised panel coordinates (npc).
#'   May be a vector.
#' @param xmin,xmax Left and right crop bounds. Vertical gridlines outside
#'   `[xmin, xmax]` are not drawn; horizontal gridlines run only from `xmin`
#'   to `xmax`. Defaults to `-Inf` and `Inf` (full panel). Use [I()] for
#'   normalised coordinates (npc). Note: filtering (removing lines outside the
#'   range) only works when both the crop bound and the intercept are in data
#'   coordinates. npc bounds affect extent only.
#' @param ymin,ymax Bottom and top crop bounds. Horizontal gridlines outside
#'   `[ymin, ymax]` are not drawn; vertical gridlines run only from `ymin` to
#'   `ymax`. Defaults to `-Inf` and `Inf` (full panel). Use [I()] for
#'   normalised coordinates (npc). Note: filtering only works when both the
#'   crop bound and the intercept are in data coordinates. npc bounds affect
#'   extent only.
#' @param colour Inherits from `panel.grid.major` in the set theme. May be a
#'   vector the same length as the total number of lines.
#' @param linewidth Inherits from `panel.grid.major` in the set theme. Supports
#'   `rel()`. May be a vector the same length as the total number of lines.
#' @param linetype Inherits from `panel.grid.major` in the set theme. May be a
#'   vector the same length as the total number of lines.
#' @param layout Controls which panels the annotation appears in. `NULL`
#'   (default) repeats in all panels. An integer targets a specific panel.
#'   `"fixed"` repeats in all panels ignoring faceting variables. See
#'   [ggplot2::layer()] for full details.
#'
#' @return A list of ggplot2 annotation layers.
#' @seealso [axis_line()], [reference_line()], [panel_shade()]
#' @export
panel_grid <- function(
    xintercept = NULL,
    yintercept = NULL,
    xmin       = -Inf,
    xmax       = Inf,
    ymin       = -Inf,
    ymax       = Inf,
    colour     = NULL,
    linewidth  = NULL,
    linetype   = NULL,
    layout     = NULL
) {
  groups   <- .build_axis_groups(xintercept, yintercept)
  n_groups <- length(groups)

  # Detect npc per crop bound, then strip AsIs
  xmin_npc <- inherits(xmin, "AsIs"); xmin <- as.numeric(xmin)
  xmax_npc <- inherits(xmax, "AsIs"); xmax <- as.numeric(xmax)
  ymin_npc <- inherits(ymin, "AsIs"); ymin <- as.numeric(ymin)
  ymax_npc <- inherits(ymax, "AsIs"); ymax <- as.numeric(ymax)

  current_theme <- ggplot2::theme_get()

  # Theme resolution — major.x for vertical lines, major.y for horizontal
  .get_grid_el <- function(int_axis) {
    .resolve_theme_el(c(
      paste0("panel.grid.major.", int_axis),
      "panel.grid.major",
      "panel.grid"
    ), current_theme) %||% list(colour = "grey92", linewidth = 1, linetype = 1)
  }

  grid_el        <- .get_grid_el(groups[[1]]$int_axis)
  theme_colour   <- grid_el$colour    %||% "grey92"
  theme_lw       <- grid_el$linewidth %||% 1
  theme_lt       <- grid_el$linetype  %||% 1

  colour_vec    <- rep_len(colour   %||% theme_colour, n_groups)
  linewidth_vec <- if (is.null(linewidth)) {
    rep_len(theme_lw, n_groups)
  } else if (inherits(linewidth, "rel")) {
    rep_len(as.numeric(linewidth), n_groups) * theme_lw
  } else {
    rep_len(linewidth, n_groups)
  }
  linetype_vec  <- rep_len(linetype %||% theme_lt, n_groups)

  # Resolve one extent bound to a grob unit and annotation_custom value.
  # npc   → grob uses unit(val, "npc"), anno spans ±Inf (grob handles position)
  # ±Inf  → grob uses unit(0/1, "npc"), anno = ±Inf (panel edge)
  # data  → grob uses unit(0/1, "npc"), anno = data value (anno_custom pins it)
  .ext <- function(val, is_npc, lo) {
    if (is_npc) {
      list(grob = grid::unit(val, "npc"), anno = if (lo) -Inf else Inf)
    } else {
      list(grob = grid::unit(if (lo) 0 else 1, "npc"), anno = val)
    }
  }

  layers <- lapply(seq_len(n_groups), \(g) {
    grp       <- groups[[g]]
    intercept <- grp$intercept
    npc_int   <- grp$npc

    # ---- Filter: skip lines outside the relevant crop range ----------------
    # Filtering only applies when BOTH the crop bound and the intercept are
    # finite data values. npc crop bounds (I()) affect line extent only —
    # comparing a data intercept to an npc bound requires scale knowledge not
    # available at call time. Use data coordinates for filtering.
    if (grp$int_axis == "x") {
      # Vertical line: filter by xmin/xmax
      if (!npc_int && !xmin_npc && is.finite(xmin) && intercept < xmin) return(NULL)
      if (!npc_int && !xmax_npc && is.finite(xmax) && intercept > xmax) return(NULL)
    } else {
      # Horizontal line: filter by ymin/ymax
      if (!npc_int && !ymin_npc && is.finite(ymin) && intercept < ymin) return(NULL)
      if (!npc_int && !ymax_npc && is.finite(ymax) && intercept > ymax) return(NULL)
    }

    # ---- Extent: resolve start/end bounds for this line --------------------
    # Vertical lines run along y (controlled by ymin/ymax crop).
    # Horizontal lines run along x (controlled by xmin/xmax crop).
    if (grp$int_axis == "x") {
      lo <- .ext(ymin, ymin_npc, lo = TRUE)
      hi <- .ext(ymax, ymax_npc, lo = FALSE)
    } else {
      lo <- .ext(xmin, xmin_npc, lo = TRUE)
      hi <- .ext(xmax, xmax_npc, lo = FALSE)
    }

    perp <- .perp_unit(grp)

    gp <- grid::gpar(
      col     = colour_vec[[g]],
      lwd     = linewidth_vec[[g]] * ggplot2::.pt,
      lty     = linetype_vec[[g]],
      lineend = "butt"
    )

    line_grob <- if (grp$int_axis == "x") {
      grid::segmentsGrob(
        x0 = perp,       x1 = perp,
        y0 = lo$grob,    y1 = hi$grob,
        gp = gp
      )
    } else {
      grid::segmentsGrob(
        x0 = lo$grob,    x1 = hi$grob,
        y0 = perp,       y1 = perp,
        gp = gp
      )
    }

    anno_pos <- if (grp$int_axis == "x") {
      list(
        xmin = if (npc_int) -Inf else intercept,
        xmax = if (npc_int) Inf  else intercept,
        ymin = lo$anno,
        ymax = hi$anno
      )
    } else {
      list(
        xmin = lo$anno,
        xmax = hi$anno,
        ymin = if (npc_int) -Inf else intercept,
        ymax = if (npc_int) Inf  else intercept
      )
    }

    .make_ann_layer(line_grob, anno_pos, layout)
  })

  # Drop NULL entries from filtered-out lines
  Filter(Negate(is.null), layers)
}
