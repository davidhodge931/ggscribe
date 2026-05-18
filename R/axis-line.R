# axis_line -------------------------------------------------------------------

#' Annotate an axis line
#'
#' Draws a line along an axis edge, with style defaults taken from the
#' `axis.line` element of the set theme. Requires
#' `coord_cartesian(clip = "off")`.
#'
#' @param ... Not used. Forces named arguments.
#' @param position One of `"top"`, `"bottom"`, `"left"`, or `"right"`. Inferred
#'   from `xintercept` or `yintercept` if not provided.
#' @param colour Inherits from `axis.line` in the set theme. May be a vector
#'   the same length as `xintercept`/`yintercept` to style each line
#'   individually.
#' @param linewidth Inherits from `axis.line` in the set theme. Supports
#'   `rel()`. May be a vector the same length as `xintercept`/`yintercept`.
#' @param linetype Inherits from `axis.line` in the set theme. May be a vector
#'   the same length as `xintercept`/`yintercept`.
#' @param arrow A [grid::arrow()] specification, or a list of such
#'   specifications the same length as `xintercept`/`yintercept`. The
#'   arrowhead points in the positive axis direction (right for x, up for y).
#'   Must use `list()` not `c()` when supplying multiple values.
#'   E.g. `grid::arrow(angle = 15, length = unit(1.5, "mm"), type = "closed")`.
#' @param xintercept For `"left"`/`"right"` axes: draw lines at these x
#'   positions in data coordinates, or wrapped in [I()] for normalised panel
#'   coordinates (npc). May be a vector for multiple lines.
#' @param yintercept For `"top"`/`"bottom"` axes: draw lines at these y
#'   positions in data coordinates, or wrapped in [I()] for normalised panel
#'   coordinates (npc). May be a vector for multiple lines.
#'
#' @return A list of ggplot2 annotation layers.
#' @seealso [axis_ticks()], [axis_text()],
#'   [axis_bracket()], [reference_line()],
#'   [panel_shade()], [sec_axis_text()]
#' @export
axis_line <- function(
    ...,
    position   = NULL,
    colour     = NULL,
    linewidth  = NULL,
    linetype   = NULL,
    arrow      = NULL,
    xintercept = NULL,
    yintercept = NULL
) {
  rlang::check_dots_empty()
  position <- .infer_position(position, xintercept, yintercept)
  axis     <- if (position %in% c("top", "bottom")) "x" else "y"
  .validate_intercept(axis, position, xintercept, yintercept)

  # Detect npc once for the whole intercept vector, then strip AsIs
  if (axis == "x") {
    npc_intercept <- inherits(yintercept, "AsIs")
    intercepts    <- if (is.null(yintercept)) {
      if (position == "bottom") -Inf else Inf
    } else {
      as.numeric(yintercept)
    }
  } else {
    npc_intercept <- inherits(xintercept, "AsIs")
    intercepts    <- if (is.null(xintercept)) {
      if (position == "left") -Inf else Inf
    } else {
      as.numeric(xintercept)
    }
  }

  intercepts <- as.numeric(intercepts)
  n          <- length(intercepts)
  current_theme <- ggplot2::theme_get()

  # ---- Resolve theme element ------------------------------------------------

  element_hierarchy <- c(
    paste0("axis.line.", axis, ".", position),
    paste0("axis.line.", axis),
    "axis.line"
  )
  theme_element_blank <- NULL
  for (nm in element_hierarchy) {
    el <- ggplot2::calc_element(nm, current_theme, skip_blank = FALSE)
    if (!is.null(el)) { theme_element_blank <- el; break }
  }
  axis_line_intentionally_blank <- is.null(theme_element_blank) ||
    inherits(theme_element_blank, "element_blank")

  resolved_element <- NULL
  if (!axis_line_intentionally_blank) {
    for (nm in element_hierarchy) {
      el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
      if (!is.null(el) && !inherits(el, "element_blank")) { resolved_element <- el; break }
    }
  }
  if (is.null(colour) && (axis_line_intentionally_blank || is.null(resolved_element$colour))) {
    rlang::warn("The set theme does not define an `axis.line` colour. Defaulting to \"black\".")
  }
  if (is.null(linewidth) && (axis_line_intentionally_blank || is.null(resolved_element$linewidth))) {
    rlang::warn("The set theme does not define an `axis.line` linewidth. Defaulting to `0.5`.")
  }
  if (is.null(resolved_element)) {
    resolved_element <- list(colour = "black", linewidth = 0.5, linetype = 1)
  }

  # ---- Resolve scalar theme defaults ----------------------------------------

  theme_colour    <- resolved_element$colour   %||% "black"
  theme_linewidth <- resolved_element$linewidth %||% 0.5
  theme_linetype  <- resolved_element$linetype  %||% 1

  # ---- Vectorise and recycle style args to n --------------------------------

  colour_vec <- if (is.null(colour)) rep_len(theme_colour, n) else rep_len(colour, n)

  linewidth_vec <- if (is.null(linewidth)) {
    rep_len(theme_linewidth, n)
  } else if (inherits(linewidth, "rel")) {
    rep_len(as.numeric(linewidth), n) * theme_linewidth
  } else {
    rep_len(linewidth, n)
  }

  linetype_vec <- if (is.null(linetype)) rep_len(theme_linetype, n) else rep_len(linetype, n)

  arrow_list <- if (inherits(arrow, "arrow")) {
    rep_len(list(arrow), n)
  } else if (is.list(arrow)) {
    rep_len(arrow, n)
  } else {
    rep_len(list(NULL), n)
  }

  # ---- Draw one grob per intercept ------------------------------------------

  lapply(seq_len(n), \(i) {
    intercept <- intercepts[[i]]

    gp <- grid::gpar(
      col     = colour_vec[[i]],
      fill    = colour_vec[[i]],
      lwd     = linewidth_vec[[i]] * ggplot2::.pt,
      lty     = linetype_vec[[i]],
      lineend = "butt"
    )

    # For npc intercepts, the value goes directly into the grob as
    # unit(val, "npc") and the annotation spans the full panel.
    # For data intercepts, the grob spans the full panel in npc and
    # annotation_custom pins it at the intercept in data coordinates.
    # The segment is drawn in the positive axis direction so the arrowhead
    # points right for x axes and up for y axes.
    if (axis == "x") {
      line_grob <- if (npc_intercept) {
        grid::segmentsGrob(
          x0 = grid::unit(0, "npc"), x1 = grid::unit(1, "npc"),
          y0 = grid::unit(intercept, "npc"), y1 = grid::unit(intercept, "npc"),
          gp = gp, arrow = arrow_list[[i]]
        )
      } else {
        grid::segmentsGrob(
          x0 = grid::unit(0, "npc"), x1 = grid::unit(1, "npc"),
          y0 = grid::unit(0.5, "npc"), y1 = grid::unit(0.5, "npc"),
          gp = gp, arrow = arrow_list[[i]]
        )
      }
      anno_pos <- if (npc_intercept) {
        list(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
      } else {
        list(xmin = -Inf, xmax = Inf, ymin = intercept, ymax = intercept)
      }
    } else {
      line_grob <- if (npc_intercept) {
        grid::segmentsGrob(
          x0 = grid::unit(intercept, "npc"), x1 = grid::unit(intercept, "npc"),
          y0 = grid::unit(0, "npc"), y1 = grid::unit(1, "npc"),
          gp = gp, arrow = arrow_list[[i]]
        )
      } else {
        grid::segmentsGrob(
          x0 = grid::unit(0.5, "npc"), x1 = grid::unit(0.5, "npc"),
          y0 = grid::unit(0, "npc"),   y1 = grid::unit(1, "npc"),
          gp = gp, arrow = arrow_list[[i]]
        )
      }
      anno_pos <- if (npc_intercept) {
        list(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
      } else {
        list(xmin = intercept, xmax = intercept, ymin = -Inf, ymax = Inf)
      }
    }

    rlang::exec(ggplot2::annotation_custom, grob = line_grob, !!!anno_pos)
  })
}
