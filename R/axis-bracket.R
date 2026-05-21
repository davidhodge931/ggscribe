# axis_bracket ----------------------------------------------------------------

#' Annotate an axis bracket
#'
#' Draws one or more brackets along an axis edge or at a floating data
#' position. Each bracket spans `min(breaks)` to `max(breaks)` with caps at
#' every break value. The bar uses the same rendering path as [axis_line()];
#' the caps use the same path as [axis_ticks()].
#' Requires `coord_cartesian(clip = "off")`.
#'
#' @param ... Not used. Forces named arguments.
#' @param position One of `"top"`, `"bottom"`, `"left"`, or `"right"`. Inferred
#'   from `xintercept` or `yintercept` if not provided.
#' @param breaks A numeric vector of length >= 2 in data coordinates, or
#'   wrapped in [I()] for normalised panel coordinates (npc), where `I(0)`
#'   is the left/bottom edge and `I(1)` is the right/top edge of the panel.
#'   The bar spans `min(breaks)` to `max(breaks)`; caps are drawn at every
#'   break value. Pass a list of such vectors to draw multiple brackets in one
#'   call — e.g. `breaks = list(c(2, 4), c(6, 8))` draws two brackets. Style
#'   args are then recycled to the number of brackets.
#' @param colour Inherits from `axis.ticks` in the set theme (falling back
#'   through `axis.line` and `line`). May be a vector the same length as the
#'   number of brackets to style each bracket individually.
#' @param linewidth Inherits from `axis.ticks` in the set theme. Supports
#'   `rel()`. May be a vector the same length as the number of brackets.
#' @param linetype Inherits from `axis.ticks` in the set theme. May be a
#'   vector the same length as the number of brackets.
#' @param length Length of the bracket caps as a grid unit. Supports `rel()`.
#'   Negative values flip the cap direction. Defaults to `rel(1)` (outward at
#'   theme tick length). May be a vector the same length as the number of
#'   brackets.
#' @param layout Controls which panels the annotation appears in. `NULL`
#'   (default) repeats in all panels. An integer targets a specific panel.
#'   `"fixed"` repeats in all panels ignoring faceting variables. See
#'   [ggplot2::layer()] for full details.
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
    layout       = NULL,
    xintercept   = NULL,
    yintercept   = NULL
) {
  rlang::check_dots_empty()

  position <- .infer_position(position, xintercept, yintercept)
  axis     <- if (position %in% c("top", "bottom")) "x" else "y"

  .validate_intercept(axis, position, xintercept, yintercept)

  # Normalise breaks to a list of vectors — a plain vector becomes list of one
  npc_breaks <- inherits(breaks, "AsIs")
  if (!is.list(breaks)) {
    breaks_list <- list(as.numeric(breaks))
  } else {
    breaks_list <- lapply(breaks, as.numeric)
  }

  n_brackets <- length(breaks_list)

  for (i in seq_len(n_brackets)) {
    if (length(breaks_list[[i]]) < 2) {
      rlang::abort(glue::glue(
        "Each element of `breaks` must have at least 2 values. ",
        "Element {i} has {length(breaks_list[[i]])}."
      ))
    }
  }

  intercept     <- .resolve_intercept(axis, position, xintercept, yintercept)
  current_theme <- ggplot2::theme_get()

  # ---- Resolve theme element ------------------------------------------------

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

  theme_colour    <- resolved_element$colour   %||% "#333333FF"
  theme_linewidth <- resolved_element$linewidth %||% 0.5
  theme_linetype  <- resolved_element$linetype  %||% 1

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

  theme_length_pts <- {
    tl <- resolved_length
    if (is.null(tl)) {
      0.5 * (current_theme$text$size %||% 11)
    } else if (inherits(tl, "rel")) {
      spacing_pts <- as.numeric(grid::convertUnit(current_theme$spacing %||% grid::unit(5.5, "pt"), "pt"))
      as.numeric(tl) * spacing_pts
    } else if (!inherits(tl, "unit")) {
      if (is.numeric(tl)) tl else 0.5 * (current_theme$text$size %||% 11)
    } else {
      as.numeric(grid::convertUnit(tl, "pt"))
    }
  }

  # ---- Vectorise style args to n_brackets ----------------------------------
  # Each bracket gets one style value; all caps within a bracket share it.

  colour_vec <- if (is.null(colour)) rep_len(theme_colour, n_brackets) else rep_len(colour, n_brackets)

  linewidth_vec <- if (is.null(linewidth)) {
    rep_len(theme_linewidth, n_brackets)
  } else if (inherits(linewidth, "rel")) {
    rep_len(as.numeric(linewidth), n_brackets) * theme_linewidth
  } else {
    rep_len(linewidth, n_brackets)
  }

  linetype_vec <- if (is.null(linetype)) rep_len(theme_linetype, n_brackets) else rep_len(linetype, n_brackets)

  length_pts_vec <- if (inherits(length, "rel")) {
    abs(rep_len(as.numeric(length), n_brackets)) * theme_length_pts
  } else if (inherits(length, "unit")) {
    rep_len(abs(as.numeric(grid::convertUnit(length, "pt"))), n_brackets)
  } else {
    rep_len(abs(as.numeric(length)), n_brackets)
  }

  flip_vec <- if (inherits(length, "rel")) {
    rep_len(as.numeric(length) < 0, n_brackets)
  } else if (inherits(length, "unit")) {
    rep_len(as.numeric(grid::convertUnit(length, "pt")) < 0, n_brackets)
  } else {
    rep_len(as.numeric(length) < 0, n_brackets)
  }

  .make_layer <- function(grob, anno_pos) {
    ggplot2::layer(
      geom     = ggplot2::GeomCustomAnn,
      stat     = "identity",
      data     = NULL,
      mapping  = ggplot2::aes(),
      position = "identity",
      params   = c(list(grob = grob, na.rm = FALSE), anno_pos),
      layout   = layout
    )
  }

  # ---- Draw one bracket per element of breaks_list -------------------------

  layers <- lapply(seq_len(n_brackets), \(b) {
    brks          <- breaks_list[[b]]
    bracket_from  <- min(brks)
    bracket_to    <- max(brks)
    cap_length    <- grid::unit(length_pts_vec[[b]], "pt")
    flip_direction <- flip_vec[[b]]

    gp_bar <- grid::gpar(
      col     = colour_vec[[b]],
      lwd     = linewidth_vec[[b]] * ggplot2::.pt,
      lty     = linetype_vec[[b]],
      lineend = "square"
    )

    gp_cap <- grid::gpar(
      col     = colour_vec[[b]],
      lwd     = linewidth_vec[[b]] * ggplot2::.pt,
      lty     = linetype_vec[[b]],
      lineend = "square"
    )

    # ---- Bar ----------------------------------------------------------------

    stamp <- if (!npc_breaks && axis == "x") {
      bar_grob <- grid::segmentsGrob(
        x0 = grid::unit(0, "npc"), x1 = grid::unit(1, "npc"),
        y0 = grid::unit(0.5, "npc"), y1 = grid::unit(0.5, "npc"),
        gp = gp_bar
      )
      list(.make_layer(bar_grob, list(
        xmin = bracket_from, xmax = bracket_to,
        ymin = intercept,    ymax = intercept
      )))
    } else if (!npc_breaks && axis == "y") {
      bar_grob <- grid::segmentsGrob(
        x0 = grid::unit(0.5, "npc"), x1 = grid::unit(0.5, "npc"),
        y0 = grid::unit(0, "npc"),   y1 = grid::unit(1, "npc"),
        gp = gp_bar
      )
      list(.make_layer(bar_grob, list(
        xmin = intercept,    xmax = intercept,
        ymin = bracket_from, ymax = bracket_to
      )))
    } else if (npc_breaks && axis == "x") {
      bar_grob <- grid::segmentsGrob(
        x0 = grid::unit(bracket_from, "npc"), x1 = grid::unit(bracket_to, "npc"),
        y0 = grid::unit(0, "npc"),            y1 = grid::unit(0, "npc"),
        gp = gp_bar
      )
      list(.make_layer(bar_grob, list(
        xmin = -Inf, xmax = Inf, ymin = intercept, ymax = intercept
      )))
    } else {
      bar_grob <- grid::segmentsGrob(
        x0 = grid::unit(0, "npc"), x1 = grid::unit(0, "npc"),
        y0 = grid::unit(bracket_from, "npc"), y1 = grid::unit(bracket_to, "npc"),
        gp = gp_bar
      )
      list(.make_layer(bar_grob, list(
        xmin = intercept, xmax = intercept, ymin = -Inf, ymax = Inf
      )))
    }

    # ---- Caps ---------------------------------------------------------------

    cap_annotations <- lapply(brks, \(break_val) {
      grob_along <- if (npc_breaks) {
        grid::unit(break_val, "npc")
      } else {
        grid::unit(0.5, "npc")
      }

      cap_grob <- if (position == "bottom") {
        grid::segmentsGrob(
          x0 = grob_along, x1 = grob_along,
          y0 = grid::unit(0, "npc"),
          y1 = if (flip_direction) grid::unit(0, "npc") + cap_length
          else                grid::unit(0, "npc") - cap_length,
          gp = gp_cap
        )
      } else if (position == "top") {
        grid::segmentsGrob(
          x0 = grob_along, x1 = grob_along,
          y0 = grid::unit(1, "npc"),
          y1 = if (flip_direction) grid::unit(1, "npc") - cap_length
          else                grid::unit(1, "npc") + cap_length,
          gp = gp_cap
        )
      } else if (position == "left") {
        grid::segmentsGrob(
          x0 = grid::unit(0, "npc"),
          x1 = if (flip_direction) grid::unit(0, "npc") + cap_length
          else                grid::unit(0, "npc") - cap_length,
          y0 = grob_along, y1 = grob_along,
          gp = gp_cap
        )
      } else {
        grid::segmentsGrob(
          x0 = grid::unit(1, "npc"),
          x1 = if (flip_direction) grid::unit(1, "npc") - cap_length
          else                grid::unit(1, "npc") + cap_length,
          y0 = grob_along, y1 = grob_along,
          gp = gp_cap
        )
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

      .make_layer(cap_grob, cap_pos)
    })

    c(stamp, cap_annotations)
  })

  unlist(layers, recursive = FALSE)
}
