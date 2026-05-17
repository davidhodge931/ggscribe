# axis_bracket ----------------------------------------------------------------

#' Annotate an axis bracket
#'
#' Draws a bracket spanning `min(breaks)` to `max(breaks)` along an axis edge
#' or at a floating data position. The bar uses the same rendering path as
#' [axis_line()]; the caps use the same path as [axis_ticks()].
#' Requires `coord_cartesian(clip = "off")`.
#'
#' @param ... Not used. Forces named arguments.
#' @param position One of `"top"`, `"bottom"`, `"left"`, or `"right"`. Inferred
#'   from `xintercept` or `yintercept` if not provided.
#' @param breaks A numeric vector of length >= 2 in data coordinates, or
#'   wrapped in [I()] for normalised panel coordinates (npc), where `I(0)`
#'   is the left/bottom edge and `I(1)` is the right/top edge of the panel.
#'   The bar spans `min(breaks)` to `max(breaks)`; caps are drawn at every
#'   break value.
#' @param colour Inherits from `axis.ticks` in the set theme (falling back
#'   through `axis.line` and `line`).
#' @param linewidth Inherits from `axis.ticks` in the set theme. Supports `rel()`.
#' @param linetype Inherits from `axis.ticks` in the set theme.
#' @param length Length of the bracket caps as a grid unit. Supports
#'   `rel()`. Negative values flip the cap direction. Defaults to `rel(1)`
#'   (outward at theme tick length).
#' @param xintercept For `"left"`/`"right"` axes: float the bracket to this x
#'   position in data coordinates instead of the panel edge.
#' @param yintercept For `"top"`/`"bottom"` axes: float the bracket to this y
#'   position in data coordinates instead of the panel edge.
#'
#' @return A list of ggplot2 annotation layers.
#' @seealso [axis_line()], [axis_ticks()],
#'   [axis_text()], [reference_line()],
#'   [panel_shade()], [sec_axis_text()]
#' @export
#'
#' @inherit sec_axis_text examples
#'
axis_bracket <- function(
    ...,
    position     = NULL,
    breaks,
    colour       = NULL,
    linewidth    = NULL,
    linetype     = NULL,
    length       = ggplot2::rel(1),
    xintercept   = NULL,
    yintercept   = NULL
) {
  rlang::check_dots_empty()

  position <- .infer_position(position, xintercept, yintercept)
  axis     <- if (position %in% c("top", "bottom")) "x" else "y"

  .validate_intercept(axis, position, xintercept, yintercept)

  if (length(breaks) < 2) {
    rlang::abort("`breaks` must have at least 2 values to define the bracket span.")
  }

  # Check once whether breaks are in npc (I()) or data coordinates
  npc_breaks <- inherits(breaks, "AsIs")
  breaks     <- as.numeric(breaks)

  bracket_from  <- min(breaks)
  bracket_to    <- max(breaks)
  intercept     <- .resolve_intercept(axis, position, xintercept, yintercept)
  current_theme <- ggplot2::theme_get()

  # ---- Resolve Style Element ------------------------------------------------
  # Walk axis.line → axis.ticks → line for consistent appearance.

  line_hierarchy <- c(
    paste0("axis.line.", axis, ".", position),
    paste0("axis.line.", axis),
    "axis.line",
    paste0("axis.ticks.", axis, ".", position),
    paste0("axis.ticks.", axis),
    "axis.ticks",
    "line"
  )
  resolved_element <- NULL
  for (nm in line_hierarchy) {
    el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
    if (!is.null(el) && !inherits(el, "element_blank")) { resolved_element <- el; break }
  }
  if (is.null(resolved_element)) {
    resolved_element <- list(colour = "#333333FF", linewidth = 0.5, linetype = 1)
  }

  line_colour    <- colour   %||% resolved_element$colour   %||% "#333333FF"
  line_linewidth <- if (is.null(linewidth)) {
    resolved_element$linewidth %||% 0.5
  } else if (inherits(linewidth, "rel")) {
    as.numeric(linewidth) * (resolved_element$linewidth %||% 0.5)
  } else {
    linewidth
  }
  line_linetype  <- linetype %||% resolved_element$linetype %||% 1

  # ---- Resolve Cap Length ---------------------------------------------------

  length_hierarchy <- c(
    paste0("axis.ticks.length.", axis, ".", position),
    paste0("axis.ticks.length.", axis),
    "axis.ticks.length"
  )
  resolved_length <- NULL
  for (nm in length_hierarchy) {
    el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
    if (!is.null(el) && !inherits(el, "element_blank")) { resolved_length <- el; break }
  }

  calculate_theme_length <- function() {
    tl <- resolved_length
    if (is.null(tl)) {
      return(grid::unit(0.5 * (current_theme$text$size %||% 11), "pt"))
    } else if (inherits(tl, "rel")) {
      spacing_pts <- as.numeric(grid::convertUnit(current_theme$spacing %||% grid::unit(5.5, "pt"), "pt"))
      return(grid::unit(as.numeric(tl) * spacing_pts, "pt"))
    } else if (!inherits(tl, "unit")) {
      return(grid::unit(if (is.numeric(tl)) tl else 0.5 * (current_theme$text$size %||% 11), "pt"))
    } else {
      return(tl)
    }
  }

  flip_direction <- FALSE
  if (inherits(length, "rel")) {
    rel_value      <- as.numeric(length)
    default_pts    <- as.numeric(grid::convertUnit(calculate_theme_length(), "pt"))
    length         <- grid::unit(abs(rel_value) * default_pts, "pt")
    flip_direction <- rel_value < 0
  } else if (inherits(length, "unit")) {
    tick_pts       <- as.numeric(grid::convertUnit(length, "pt"))
    length         <- grid::unit(abs(tick_pts), "pt")
    flip_direction <- tick_pts < 0
  } else if (is.numeric(length)) {
    flip_direction <- length < 0
    length         <- grid::unit(abs(length), "pt")
  }

  gp_bar <- grid::gpar(
    col     = line_colour,
    lwd     = line_linewidth * ggplot2::.pt,
    lty     = line_linetype,
    lineend = "square"
  )

  gp_cap <- grid::gpar(
    col     = line_colour,
    lwd     = line_linewidth * ggplot2::.pt,
    lty     = line_linetype,
    lineend = "square"
  )

  # ---- Bar ------------------------------------------------------------------
  # For data coords: annotate("segment") — same rendering path as axis_line().
  # For npc coords: annotation_custom + segmentsGrob with npc units, spanning
  # the full panel in the perpendicular direction with intercept preserved.

  stamp <- if (!npc_breaks && axis == "x") {
    list(ggplot2::annotate(
      "segment",
      x = bracket_from, xend = bracket_to,
      y = intercept,    yend = intercept,
      colour = line_colour, linewidth = line_linewidth, linetype = line_linetype,
      lineend = "square"
    ))
  } else if (!npc_breaks && axis == "y") {
    list(ggplot2::annotate(
      "segment",
      x = intercept,    xend = intercept,
      y = bracket_from, yend = bracket_to,
      colour = line_colour, linewidth = line_linewidth, linetype = line_linetype,
      lineend = "square"
    ))
  } else if (npc_breaks && axis == "x") {
    bar_grob <- grid::segmentsGrob(
      x0 = grid::unit(bracket_from, "npc"), x1 = grid::unit(bracket_to, "npc"),
      y0 = grid::unit(0, "npc"),            y1 = grid::unit(0, "npc"),
      gp = gp_bar
    )
    list(ggplot2::annotation_custom(
      grob = bar_grob,
      xmin = -Inf, xmax = Inf, ymin = intercept, ymax = intercept
    ))
  } else {
    bar_grob <- grid::segmentsGrob(
      x0 = grid::unit(0, "npc"), x1 = grid::unit(0, "npc"),
      y0 = grid::unit(bracket_from, "npc"), y1 = grid::unit(bracket_to, "npc"),
      gp = gp_bar
    )
    list(ggplot2::annotation_custom(
      grob = bar_grob,
      xmin = intercept, xmax = intercept, ymin = -Inf, ymax = Inf
    ))
  }

  # ---- Caps -----------------------------------------------------------------
  # annotation_custom + segmentsGrob pinned to each break.
  # For npc breaks, the along-axis coordinate is set directly in the grob
  # and the annotation spans the full panel in that direction.

  cap_grob_template <- if (position == "bottom") {
    function(grob_along) grid::segmentsGrob(
      x0 = grob_along, x1 = grob_along,
      y0 = grid::unit(0, "npc"),
      y1 = if (flip_direction) grid::unit(0, "npc") + length
      else                grid::unit(0, "npc") - length,
      gp = gp_cap
    )
  } else if (position == "top") {
    function(grob_along) grid::segmentsGrob(
      x0 = grob_along, x1 = grob_along,
      y0 = grid::unit(1, "npc"),
      y1 = if (flip_direction) grid::unit(1, "npc") - length
      else                grid::unit(1, "npc") + length,
      gp = gp_cap
    )
  } else if (position == "left") {
    function(grob_along) grid::segmentsGrob(
      x0 = grid::unit(0, "npc"),
      x1 = if (flip_direction) grid::unit(0, "npc") + length
      else                grid::unit(0, "npc") - length,
      y0 = grob_along, y1 = grob_along,
      gp = gp_cap
    )
  } else {
    function(grob_along) grid::segmentsGrob(
      x0 = grid::unit(1, "npc"),
      x1 = if (flip_direction) grid::unit(1, "npc") - length
      else                grid::unit(1, "npc") + length,
      y0 = grob_along, y1 = grob_along,
      gp = gp_cap
    )
  }

  cap_annotations <- lapply(breaks, \(break_val) {
    grob_along <- if (npc_breaks) {
      grid::unit(break_val, "npc")
    } else {
      grid::unit(0.5, "npc")
    }

    cap_pos <- if (npc_breaks && axis == "x") {
      list(xmin = -Inf, xmax = Inf, ymin = intercept, ymax = intercept)
    } else if (npc_breaks && axis == "y") {
      list(xmin = intercept, xmax = intercept, ymin = -Inf, ymax = Inf)
    } else if (axis == "x") {
      list(xmin = break_val, xmax = break_val, ymin = intercept, ymax = intercept)
    } else {
      list(xmin = intercept, xmax = intercept, ymin = break_val, ymax = break_val)
    }

    rlang::exec(
      ggplot2::annotation_custom,
      grob = cap_grob_template(grob_along),
      !!!cap_pos
    )
  })

  c(stamp, cap_annotations)
}
