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
#'   args and intercepts are recycled to the number of brackets.
#' @param colour Inherits from `axis.ticks` in the set theme. May be a vector
#'   the same length as the number of brackets.
#' @param linewidth Inherits from `axis.ticks` in the set theme. Supports
#'   `rel()`. May be a vector the same length as the number of brackets.
#' @param linetype Inherits from `axis.ticks` in the set theme. May be a
#'   vector the same length as the number of brackets.
#' @param length Length of the bracket caps. Supports `rel()`. Negative values
#'   flip the cap direction. Defaults to `rel(1)`. May be a vector the same
#'   length as the number of brackets.
#' @param layout Controls which panels the annotation appears in. `NULL`
#'   (default) repeats in all panels. An integer targets a specific panel.
#'   `"fixed"` repeats in all panels ignoring faceting variables. See
#'   [ggplot2::layer()] for full details.
#' @param xintercept For `"left"`/`"right"` axes: float the bracket to these x
#'   positions in data coordinates. Paired 1:1 with brackets — each bracket
#'   gets its own intercept, recycling applies.
#' @param yintercept For `"top"`/`"bottom"` axes: float the bracket to these y
#'   positions in data coordinates. Paired 1:1 with brackets — each bracket
#'   gets its own intercept, recycling applies.
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
  # 

  position <- .infer_position(position, xintercept, yintercept)
  axis     <- if (position %in% c("top", "bottom")) "x" else "y"

  .validate_intercept(axis, position, xintercept, yintercept)

  # Normalise breaks to a list
  npc_breaks <- inherits(breaks, "AsIs")
  breaks_list <- if (!is.list(breaks)) list(as.numeric(breaks)) else lapply(breaks, as.numeric)

  for (i in seq_along(breaks_list)) {
    if (length(breaks_list[[i]]) < 2) {
      rlang::abort(glue::glue(
        "Each element of `breaks` must have at least 2 values. ",
        "Element {i} has {length(breaks_list[[i]])}."
      ))
    }
  }

  raw_intercepts <- if (axis == "x") {
    if (is.null(yintercept)) if (position == "bottom") -Inf else Inf
    else as.numeric(yintercept)
  } else {
    if (is.null(xintercept)) if (position == "left") -Inf else Inf
    else as.numeric(xintercept)
  }

  # 1:1 pairing between brackets and intercepts — recycle both to the longer
  n           <- max(length(breaks_list), length(raw_intercepts))
  breaks_list <- rep_len(breaks_list, n)
  intercepts  <- rep_len(raw_intercepts, n)

  current_theme <- ggplot2::theme_get()

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

  # Style args recycle to n (one per bracket)
  colour_vec <- if (is.null(colour)) rep_len(theme_colour, n) else rep_len(colour, n)

  linewidth_vec <- if (is.null(linewidth)) {
    rep_len(theme_linewidth, n)
  } else if (inherits(linewidth, "rel")) {
    rep_len(as.numeric(linewidth), n) * theme_linewidth
  } else {
    rep_len(linewidth, n)
  }

  linetype_vec <- if (is.null(linetype)) rep_len(theme_linetype, n) else rep_len(linetype, n)

  length_pts_vec <- if (inherits(length, "rel")) {
    abs(rep_len(as.numeric(length), n)) * theme_length_pts
  } else if (inherits(length, "unit")) {
    rep_len(abs(as.numeric(grid::convertUnit(length, "pt"))), n)
  } else {
    rep_len(abs(as.numeric(length)), n)
  }

  flip_vec <- if (inherits(length, "rel")) {
    rep_len(as.numeric(length) < 0, n)
  } else if (inherits(length, "unit")) {
    rep_len(as.numeric(grid::convertUnit(length, "pt")) < 0, n)
  } else {
    rep_len(as.numeric(length) < 0, n)
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

  # Single loop over paired (bracket, intercept) values
  unlist(lapply(seq_len(n), \(i) {
    brks           <- breaks_list[[i]]
    intercept      <- intercepts[[i]]
    bracket_from   <- min(brks)
    bracket_to     <- max(brks)
    cap_length     <- grid::unit(length_pts_vec[[i]], "pt")
    flip_direction <- flip_vec[[i]]

    gp <- grid::gpar(
      col     = colour_vec[[i]],
      lwd     = linewidth_vec[[i]] * ggplot2::.pt,
      lty     = linetype_vec[[i]],
      lineend = "square"
    )

    # ---- Bar ----------------------------------------------------------------

    stamp <- if (!npc_breaks && axis == "x") {
      bar_grob <- grid::segmentsGrob(
        x0 = grid::unit(0, "npc"), x1 = grid::unit(1, "npc"),
        y0 = grid::unit(0.5, "npc"), y1 = grid::unit(0.5, "npc"),
        gp = gp
      )
      list(.make_layer(bar_grob, list(
        xmin = bracket_from, xmax = bracket_to,
        ymin = intercept,    ymax = intercept
      )))
    } else if (!npc_breaks && axis == "y") {
      bar_grob <- grid::segmentsGrob(
        x0 = grid::unit(0.5, "npc"), x1 = grid::unit(0.5, "npc"),
        y0 = grid::unit(0, "npc"),   y1 = grid::unit(1, "npc"),
        gp = gp
      )
      list(.make_layer(bar_grob, list(
        xmin = intercept,    xmax = intercept,
        ymin = bracket_from, ymax = bracket_to
      )))
    } else if (npc_breaks && axis == "x") {
      bar_grob <- grid::segmentsGrob(
        x0 = grid::unit(bracket_from, "npc"), x1 = grid::unit(bracket_to, "npc"),
        y0 = grid::unit(0, "npc"),            y1 = grid::unit(0, "npc"),
        gp = gp
      )
      list(.make_layer(bar_grob, list(
        xmin = -Inf, xmax = Inf, ymin = intercept, ymax = intercept
      )))
    } else {
      bar_grob <- grid::segmentsGrob(
        x0 = grid::unit(0, "npc"), x1 = grid::unit(0, "npc"),
        y0 = grid::unit(bracket_from, "npc"), y1 = grid::unit(bracket_to, "npc"),
        gp = gp
      )
      list(.make_layer(bar_grob, list(
        xmin = intercept, xmax = intercept, ymin = -Inf, ymax = Inf
      )))
    }

    # ---- Caps ---------------------------------------------------------------

    cap_annotations <- lapply(brks, \(break_val) {
      grob_along <- if (npc_breaks) grid::unit(break_val, "npc") else grid::unit(0.5, "npc")

      cap_grob <- if (position == "bottom") {
        grid::segmentsGrob(
          x0 = grob_along, x1 = grob_along,
          y0 = grid::unit(0, "npc"),
          y1 = if (flip_direction) grid::unit(0, "npc") + cap_length
          else                grid::unit(0, "npc") - cap_length,
          gp = gp
        )
      } else if (position == "top") {
        grid::segmentsGrob(
          x0 = grob_along, x1 = grob_along,
          y0 = grid::unit(1, "npc"),
          y1 = if (flip_direction) grid::unit(1, "npc") - cap_length
          else                grid::unit(1, "npc") + cap_length,
          gp = gp
        )
      } else if (position == "left") {
        grid::segmentsGrob(
          x0 = grid::unit(0, "npc"),
          x1 = if (flip_direction) grid::unit(0, "npc") + cap_length
          else                grid::unit(0, "npc") - cap_length,
          y0 = grob_along, y1 = grob_along,
          gp = gp
        )
      } else {
        grid::segmentsGrob(
          x0 = grid::unit(1, "npc"),
          x1 = if (flip_direction) grid::unit(1, "npc") - cap_length
          else                grid::unit(1, "npc") + cap_length,
          y0 = grob_along, y1 = grob_along,
          gp = gp
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
  }), recursive = FALSE)
}
