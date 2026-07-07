# axis_text ------------------------------------------------------------------

#' Annotate axis text
#'
#' Draws text labels at specified break positions along a floating axis line,
#' with style defaults taken from the `axis.text` element of the set theme.
#' Requires `coord_cartesian(clip = "off")`.
#'
#' Text always sits on the positive side of the axis by default (right of
#' `xintercept` lines, above `yintercept` lines). Use a negative `length`
#'  to place text on the opposite side (e.g. `length = -rel(1)`).
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
#' @param labels One of:
#'   - `NULL` (default) to use break values as labels
#'   - A character vector recycled across all breaks in order
#'   - A function taking break values and returning labels
#'   - A list the same length as the number of axes, each element being one
#'     of the above
#' @param length Offset from the axis line including tick length and margin.
#'   Supports `rel()`. Negative values place text on the opposite side.
#'   Defaults to `rel(1)`. May be a vector recycled across all breaks in order.
#' @param angle Text rotation angle. Defaults to `0`. May be a vector recycled
#'   across all breaks in order.
#' @param hjust,vjust Justification. Auto-calculated from axis direction and
#'   `angle` if `NULL`. May be a vector recycled across all breaks in order.
#' @param colour Inherits from `axis.text` in the set theme. May be a vector
#'   recycled across all breaks in order.
#' @param size Inherits from `axis.text` in the set theme. May be a vector
#'   recycled across all breaks in order.
#' @param family Inherits from `axis.text` in the set theme. May be a vector
#'   recycled across all breaks in order.
#' @param layout Controls which panels the annotation appears in. `NULL`
#'   (default) repeats in all panels. An integer targets a specific panel.
#'   `"fixed"` repeats in all panels ignoring faceting variables. See
#'   [ggplot2::layer()] for full details.
#'
#' @return A list of ggplot2 annotation layers.
#' @export
axis_text <- function(
  xintercept = NULL,
  yintercept = NULL,
  breaks,
  labels = NULL,
  length = ggplot2::rel(1),
  angle = 0,
  hjust = NULL,
  vjust = NULL,
  colour = NULL,
  size = NULL,
  family = NULL,
  layout = NULL
) {
  axis_specs <- .build_axis_specs(xintercept, yintercept)

  aligned <- .align_axis_breaks(axis_specs, breaks)
  axis_specs <- aligned$axis_specs
  break_specs <- aligned$break_specs
  n_axes <- aligned$n_axes

  current_theme <- ggplot2::get_theme()

  n_per_axis <- vapply(break_specs, function(x) length(x$vals), integer(1))
  n_total <- sum(n_per_axis)
  offsets <- c(0L, cumsum(n_per_axis[-length(n_per_axis)]))

  axis_defaults <- lapply(axis_specs, function(axis_spec) {
    text_el <- .default(
      .calc_theme_element(
        .axis_text_hierarchy(axis_spec$int_axis),
        current_theme
      ),
      ggplot2::element_text(colour = "black", size = 11, family = "")
    )

    ticks_length_el <- .calc_theme_element(
      .axis_ticks_length_hierarchy(axis_spec$int_axis),
      current_theme
    )

    list(
      text_el = text_el,
      colour = .theme_value(text_el, "colour", "black"),
      size = .theme_value(text_el, "size", 11),
      family = .theme_value(text_el, "family", ""),
      theme_length_pt = .theme_length_to_pt(ticks_length_el, current_theme)
    )
  })

  default_colour <- unlist(
    Map(function(x, n) rep(x$colour, n), axis_defaults, n_per_axis),
    use.names = FALSE
  )
  default_size <- unlist(
    Map(function(x, n) rep(x$size, n), axis_defaults, n_per_axis),
    use.names = FALSE
  )
  default_family <- unlist(
    Map(function(x, n) rep(x$family, n), axis_defaults, n_per_axis),
    use.names = FALSE
  )
  default_theme_length <- unlist(
    Map(function(x, n) rep(x$theme_length_pt, n), axis_defaults, n_per_axis),
    use.names = FALSE
  )

  colour_vec <- if (is.null(colour)) {
    default_colour
  } else {
    .recycle_values(colour, n_total, "colour")
  }
  size_vec <- if (is.null(size)) {
    default_size
  } else {
    .recycle_values(size, n_total, "size")
  }
  family_vec <- if (is.null(family)) {
    default_family
  } else {
    .recycle_values(family, n_total, "family")
  }
  angle_vec <- .recycle_values(angle, n_total, "angle")

  offset <- .resolve_offset(length, n_total, default_theme_length, "length")

  axis_index <- rep(seq_len(n_axes), n_per_axis)
  side_vec <- mapply(
    function(i, flip) .effective_side(axis_specs[[i]]$int_axis, flip),
    axis_index,
    offset$flip,
    SIMPLIFY = TRUE
  )

  margin_vec <- vapply(
    seq_len(n_total),
    function(i) {
      axis_i <- axis_index[[i]]
      .text_margin_pt(axis_defaults[[axis_i]]$text_el, side_vec[[i]])
    },
    numeric(1)
  )

  hjust_vec <- if (is.null(hjust)) {
    mapply(function(side, angle) .get_hjust(side, angle), side_vec, angle_vec)
  } else {
    .recycle_values(hjust, n_total, "hjust")
  }

  vjust_vec <- if (is.null(vjust)) {
    mapply(function(side, angle) .get_vjust(side, angle), side_vec, angle_vec)
  } else {
    .recycle_values(vjust, n_total, "vjust")
  }

  label_specs <- .normalise_label_specs(labels, break_specs, n_axes)

  unlist(
    lapply(seq_len(n_axes), function(g) {
      axis_spec <- axis_specs[[g]]
      break_spec <- break_specs[[g]]
      labels_g <- label_specs[[g]]
      offset_g <- offsets[[g]]
      intercept <- .intercept_unit(axis_spec)

      lapply(seq_along(break_spec$vals), function(i) {
        global_i <- offset_g + i
        break_i <- break_spec$vals[[i]]
        along <- .break_unit(break_i, break_spec$npc)

        total_offset <- grid::unit(
          offset$pts[[global_i]] + margin_vec[[global_i]],
          "pt"
        )

        gp <- grid::gpar(
          col = colour_vec[[global_i]],
          fontsize = size_vec[[global_i]],
          fontfamily = family_vec[[global_i]]
        )

        just <- c(hjust = hjust_vec[[global_i]], vjust = vjust_vec[[global_i]])
        rot <- angle_vec[[global_i]]

        text_grob <- if (axis_spec$int_axis == "x") {
          x_text <- if (offset$flip[[global_i]]) {
            intercept - total_offset
          } else {
            intercept + total_offset
          }
          grid::textGrob(
            labels_g[[i]],
            x = x_text,
            y = along,
            just = just,
            rot = rot,
            gp = gp
          )
        } else {
          y_text <- if (offset$flip[[global_i]]) {
            intercept - total_offset
          } else {
            intercept + total_offset
          }
          grid::textGrob(
            labels_g[[i]],
            x = along,
            y = y_text,
            just = just,
            rot = rot,
            gp = gp
          )
        }

        .new_annotation_layer(
          text_grob,
          .annotation_bounds(axis_spec, break_i, break_spec$npc),
          layout
        )
      })
    }),
    recursive = FALSE
  )
}

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
#' @export
axis_ticks <- function(
  xintercept = NULL,
  yintercept = NULL,
  breaks,
  length = ggplot2::rel(1),
  colour = NULL,
  linewidth = NULL,
  linetype = NULL,
  arrow = NULL,
  layout = NULL
) {
  axis_specs <- .build_axis_specs(xintercept, yintercept)

  aligned <- .align_axis_breaks(axis_specs, breaks)
  axis_specs <- aligned$axis_specs
  break_specs <- aligned$break_specs
  n_axes <- aligned$n_axes

  current_theme <- ggplot2::get_theme()

  n_per_axis <- vapply(break_specs, function(x) length(x$vals), integer(1))
  n_total <- sum(n_per_axis)
  offsets <- c(0L, cumsum(n_per_axis[-length(n_per_axis)]))

  axis_defaults <- lapply(axis_specs, function(axis_spec) {
    ticks_el <- .default(
      .calc_theme_element(
        .axis_ticks_hierarchy(axis_spec$int_axis),
        current_theme
      ),
      ggplot2::element_line(colour = "black", linewidth = 0.5, linetype = 1)
    )

    ticks_length_el <- .calc_theme_element(
      .axis_ticks_length_hierarchy(axis_spec$int_axis),
      current_theme
    )

    list(
      colour = .theme_value(ticks_el, "colour", "black"),
      linewidth = .theme_value(ticks_el, "linewidth", 0.5),
      linetype = .theme_value(ticks_el, "linetype", 1),
      theme_length_pt = .theme_length_to_pt(ticks_length_el, current_theme)
    )
  })

  default_colour <- unlist(
    Map(function(x, n) rep(x$colour, n), axis_defaults, n_per_axis),
    use.names = FALSE
  )
  default_linewidth <- unlist(
    Map(function(x, n) rep(x$linewidth, n), axis_defaults, n_per_axis),
    use.names = FALSE
  )
  default_linetype <- unlist(
    Map(function(x, n) rep(x$linetype, n), axis_defaults, n_per_axis),
    use.names = FALSE
  )
  default_theme_length <- unlist(
    Map(function(x, n) rep(x$theme_length_pt, n), axis_defaults, n_per_axis),
    use.names = FALSE
  )

  colour_vec <- if (is.null(colour)) {
    default_colour
  } else {
    .recycle_values(colour, n_total, "colour")
  }
  linetype_vec <- if (is.null(linetype)) {
    default_linetype
  } else {
    .recycle_values(linetype, n_total, "linetype")
  }
  linewidth_vec <- .resolve_linewidth(
    linewidth,
    n_total,
    default_linewidth,
    "linewidth"
  )
  offset <- .resolve_offset(length, n_total, default_theme_length, "length")
  arrow_list <- .recycle_arrow_spec(arrow, n_total, "arrow")

  unlist(
    lapply(seq_len(n_axes), function(g) {
      axis_spec <- axis_specs[[g]]
      break_spec <- break_specs[[g]]
      offset_g <- offsets[[g]]
      intercept <- .intercept_unit(axis_spec)

      lapply(seq_along(break_spec$vals), function(i) {
        global_i <- offset_g + i
        break_i <- break_spec$vals[[i]]
        along <- .break_unit(break_i, break_spec$npc)
        tick_len <- grid::unit(offset$pts[[global_i]], "pt")

        gp <- grid::gpar(
          col = colour_vec[[global_i]],
          fill = colour_vec[[global_i]],
          lwd = linewidth_vec[[global_i]] * ggplot2::.pt,
          lty = linetype_vec[[global_i]],
          lineend = "butt"
        )

        tick_grob <- if (axis_spec$int_axis == "x") {
          x_tip <- if (offset$flip[[global_i]]) {
            intercept - tick_len
          } else {
            intercept + tick_len
          }
          grid::segmentsGrob(
            x0 = x_tip,
            x1 = intercept,
            y0 = along,
            y1 = along,
            gp = gp,
            arrow = arrow_list[[global_i]]
          )
        } else {
          y_tip <- if (offset$flip[[global_i]]) {
            intercept - tick_len
          } else {
            intercept + tick_len
          }
          grid::segmentsGrob(
            x0 = along,
            x1 = along,
            y0 = y_tip,
            y1 = intercept,
            gp = gp,
            arrow = arrow_list[[global_i]]
          )
        }

        .new_annotation_layer(
          tick_grob,
          .annotation_bounds(axis_spec, break_i, break_spec$npc),
          layout
        )
      })
    }),
    recursive = FALSE
  )
}

# axis_bracket ----------------------------------------------------------------

#' Annotate an axis bracket
#'
#' Draws one or more brackets along a floating axis line. Each bracket spans
#' `min(breaks)` to `max(breaks)` with caps at every break value.
#' Requires `coord_cartesian(clip = "off")`.
#'
#' Caps always point in the positive direction by default (right for
#' `xintercept`, up for `yintercept`). Use a negative `length` to flip them
#' (e.g. `length = -rel(1)`).
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
#' @export
axis_bracket <- function(
  xintercept = NULL,
  yintercept = NULL,
  breaks,
  length = ggplot2::rel(1),
  colour = NULL,
  linewidth = NULL,
  linetype = NULL,
  layout = NULL
) {
  axis_specs <- .build_axis_specs(xintercept, yintercept)

  aligned <- .align_axis_breaks(axis_specs, breaks)
  axis_specs <- aligned$axis_specs
  break_specs <- aligned$break_specs
  n_axes <- aligned$n_axes

  for (g in seq_len(n_axes)) {
    if (length(break_specs[[g]]$vals) < 2) {
      rlang::abort(
        sprintf(
          "Each element of `breaks` must have at least 2 values. Group %d has %d.",
          g,
          length(break_specs[[g]]$vals)
        )
      )
    }
  }

  current_theme <- ggplot2::get_theme()

  axis_defaults <- lapply(axis_specs, function(axis_spec) {
    line_el <- .default(
      .calc_theme_element(
        .axis_bracket_hierarchy(axis_spec$int_axis),
        current_theme
      ),
      ggplot2::element_line(colour = "black", linewidth = 0.5, linetype = 1)
    )

    ticks_length_el <- .calc_theme_element(
      .axis_ticks_length_hierarchy(axis_spec$int_axis),
      current_theme
    )

    list(
      colour = .theme_value(line_el, "colour", "black"),
      linewidth = .theme_value(line_el, "linewidth", 0.5),
      linetype = .theme_value(line_el, "linetype", 1),
      theme_length_pt = .theme_length_to_pt(ticks_length_el, current_theme)
    )
  })

  default_colour <- vapply(axis_defaults, function(x) x$colour, character(1))
  default_linewidth <- vapply(
    axis_defaults,
    function(x) x$linewidth,
    numeric(1)
  )
  default_linetype <- vapply(axis_defaults, function(x) x$linetype, numeric(1))
  default_theme_length <- vapply(
    axis_defaults,
    function(x) x$theme_length_pt,
    numeric(1)
  )

  colour_vec <- if (is.null(colour)) {
    default_colour
  } else {
    .recycle_values(colour, n_axes, "colour")
  }
  linetype_vec <- if (is.null(linetype)) {
    default_linetype
  } else {
    .recycle_values(linetype, n_axes, "linetype")
  }
  linewidth_vec <- .resolve_linewidth(
    linewidth,
    n_axes,
    default_linewidth,
    "linewidth"
  )
  offset <- .resolve_offset(length, n_axes, default_theme_length, "length")

  unlist(
    lapply(seq_len(n_axes), function(g) {
      axis_spec <- axis_specs[[g]]
      break_spec <- break_specs[[g]]
      intercept <- .intercept_unit(axis_spec)
      cap_len <- grid::unit(offset$pts[[g]], "pt")
      bracket_from <- min(break_spec$vals)
      bracket_to <- max(break_spec$vals)

      gp <- grid::gpar(
        col = colour_vec[[g]],
        lwd = linewidth_vec[[g]] * ggplot2::.pt,
        lty = linetype_vec[[g]],
        lineend = "square"
      )

      bar_layer <- if (axis_spec$int_axis == "x") {
        if (break_spec$npc) {
          bar_grob <- grid::segmentsGrob(
            x0 = intercept,
            x1 = intercept,
            y0 = grid::unit(bracket_from, "npc"),
            y1 = grid::unit(bracket_to, "npc"),
            gp = gp
          )

          .new_annotation_layer(
            bar_grob,
            list(
              xmin = if (axis_spec$npc) -Inf else axis_spec$intercept,
              xmax = if (axis_spec$npc) Inf else axis_spec$intercept,
              ymin = -Inf,
              ymax = Inf
            ),
            layout
          )
        } else {
          bar_grob <- grid::segmentsGrob(
            x0 = intercept,
            x1 = intercept,
            y0 = grid::unit(0, "npc"),
            y1 = grid::unit(1, "npc"),
            gp = gp
          )

          .new_annotation_layer(
            bar_grob,
            list(
              xmin = if (axis_spec$npc) -Inf else axis_spec$intercept,
              xmax = if (axis_spec$npc) Inf else axis_spec$intercept,
              ymin = bracket_from,
              ymax = bracket_to
            ),
            layout
          )
        }
      } else {
        if (break_spec$npc) {
          bar_grob <- grid::segmentsGrob(
            x0 = grid::unit(bracket_from, "npc"),
            x1 = grid::unit(bracket_to, "npc"),
            y0 = intercept,
            y1 = intercept,
            gp = gp
          )

          .new_annotation_layer(
            bar_grob,
            list(
              xmin = -Inf,
              xmax = Inf,
              ymin = if (axis_spec$npc) -Inf else axis_spec$intercept,
              ymax = if (axis_spec$npc) Inf else axis_spec$intercept
            ),
            layout
          )
        } else {
          bar_grob <- grid::segmentsGrob(
            x0 = grid::unit(0, "npc"),
            x1 = grid::unit(1, "npc"),
            y0 = intercept,
            y1 = intercept,
            gp = gp
          )

          .new_annotation_layer(
            bar_grob,
            list(
              xmin = bracket_from,
              xmax = bracket_to,
              ymin = if (axis_spec$npc) -Inf else axis_spec$intercept,
              ymax = if (axis_spec$npc) Inf else axis_spec$intercept
            ),
            layout
          )
        }
      }

      cap_layers <- lapply(break_spec$vals, function(break_i) {
        along <- .break_unit(break_i, break_spec$npc)

        cap_grob <- if (axis_spec$int_axis == "x") {
          x_tip <- if (offset$flip[[g]]) {
            intercept - cap_len
          } else {
            intercept + cap_len
          }
          grid::segmentsGrob(
            x0 = x_tip,
            x1 = intercept,
            y0 = along,
            y1 = along,
            gp = gp
          )
        } else {
          y_tip <- if (offset$flip[[g]]) {
            intercept - cap_len
          } else {
            intercept + cap_len
          }
          grid::segmentsGrob(
            x0 = along,
            x1 = along,
            y0 = y_tip,
            y1 = intercept,
            gp = gp
          )
        }

        .new_annotation_layer(
          cap_grob,
          .annotation_bounds(axis_spec, break_i, break_spec$npc),
          layout
        )
      })

      c(list(bar_layer), cap_layers)
    }),
    recursive = FALSE
  )
}

# axis_line -------------------------------------------------------------------

#' Annotate an axis line
#'
#' Draws a full line at one or more floating positions, with style defaults
#' taken from the `axis.line` element of the set theme. Requires
#' `coord_cartesian(clip = "off")`.
#'
#' The arrow (if any) points in the positive direction by default — rightward
#' for `xintercept` lines, upward for `yintercept` lines.
#'
#' @param xintercept One or more x positions for vertical axis lines, in data
#'   coordinates or wrapped in [I()] for normalised panel coordinates (npc).
#'   May be a vector; each value produces a separate line.
#' @param yintercept One or more y positions for horizontal axis lines, in data
#'   coordinates or wrapped in [I()] for normalised panel coordinates (npc).
#'   May be a vector; each value produces a separate line.
#' @param colour Inherits from `axis.line` in the set theme. May be a vector
#'   the same length as the total number of lines.
#' @param linewidth Inherits from `axis.line` in the set theme. Supports
#'   `rel()`. May be a vector the same length as the total number of lines.
#' @param linetype Inherits from `axis.line` in the set theme. May be a vector
#'   the same length as the total number of lines.
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
#' @export
axis_line <- function(
  xintercept = NULL,
  yintercept = NULL,
  colour = NULL,
  linewidth = NULL,
  linetype = NULL,
  arrow = NULL,
  layout = NULL
) {
  axis_specs <- .build_axis_specs(xintercept, yintercept)
  n_axes <- length(axis_specs)
  current_theme <- ggplot2::get_theme()

  axis_defaults <- lapply(axis_specs, function(axis_spec) {
    line_el <- .default(
      .calc_theme_element(
        .axis_line_hierarchy(axis_spec$int_axis),
        current_theme
      ),
      ggplot2::element_line(colour = "black", linewidth = 0.5, linetype = 1)
    )

    list(
      colour = .theme_value(line_el, "colour", "black"),
      linewidth = .theme_value(line_el, "linewidth", 0.5),
      linetype = .theme_value(line_el, "linetype", 1)
    )
  })

  default_colour <- vapply(axis_defaults, function(x) x$colour, character(1))
  default_linewidth <- vapply(
    axis_defaults,
    function(x) x$linewidth,
    numeric(1)
  )
  default_linetype <- vapply(axis_defaults, function(x) x$linetype, numeric(1))

  colour_vec <- if (is.null(colour)) {
    default_colour
  } else {
    .recycle_values(colour, n_axes, "colour")
  }
  linetype_vec <- if (is.null(linetype)) {
    default_linetype
  } else {
    .recycle_values(linetype, n_axes, "linetype")
  }
  linewidth_vec <- .resolve_linewidth(
    linewidth,
    n_axes,
    default_linewidth,
    "linewidth"
  )
  arrow_list <- .recycle_arrow_spec(arrow, n_axes, "arrow")

  lapply(seq_len(n_axes), function(g) {
    axis_spec <- axis_specs[[g]]
    intercept <- .intercept_unit(axis_spec)

    gp <- grid::gpar(
      col = colour_vec[[g]],
      fill = colour_vec[[g]],
      lwd = linewidth_vec[[g]] * ggplot2::.pt,
      lty = linetype_vec[[g]],
      lineend = "butt"
    )

    line_grob <- if (axis_spec$int_axis == "x") {
      grid::segmentsGrob(
        x0 = intercept,
        x1 = intercept,
        y0 = grid::unit(0, "npc"),
        y1 = grid::unit(1, "npc"),
        gp = gp,
        arrow = arrow_list[[g]]
      )
    } else {
      grid::segmentsGrob(
        x0 = grid::unit(0, "npc"),
        x1 = grid::unit(1, "npc"),
        y0 = intercept,
        y1 = intercept,
        gp = gp,
        arrow = arrow_list[[g]]
      )
    }

    bounds <- if (axis_spec$int_axis == "x") {
      list(
        xmin = if (axis_spec$npc) -Inf else axis_spec$intercept,
        xmax = if (axis_spec$npc) Inf else axis_spec$intercept,
        ymin = -Inf,
        ymax = Inf
      )
    } else {
      list(
        xmin = -Inf,
        xmax = Inf,
        ymin = if (axis_spec$npc) -Inf else axis_spec$intercept,
        ymax = if (axis_spec$npc) Inf else axis_spec$intercept
      )
    }

    .new_annotation_layer(line_grob, bounds, layout)
  })
}

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
#' @export
reference_line <- function(
  xintercept = NULL,
  yintercept = NULL,
  colour = NULL,
  linewidth = NULL,
  linetype = "dashed",
  arrow = NULL,
  layout = NULL
) {
  axis_specs <- .build_axis_specs(xintercept, yintercept)
  n_axes <- length(axis_specs)
  current_theme <- ggplot2::get_theme()

  axis_defaults <- lapply(axis_specs, function(axis_spec) {
    line_el <- .default(
      .calc_theme_element(
        .axis_line_hierarchy(axis_spec$int_axis),
        current_theme
      ),
      ggplot2::element_line(colour = "black", linewidth = 0.5, linetype = 1)
    )

    list(
      colour = .theme_value(line_el, "colour", "black"),
      linewidth = .theme_value(line_el, "linewidth", 0.5)
    )
  })

  default_colour <- vapply(axis_defaults, function(x) x$colour, character(1))
  default_linewidth <- vapply(
    axis_defaults,
    function(x) x$linewidth,
    numeric(1)
  )

  colour_vec <- if (is.null(colour)) {
    default_colour
  } else {
    .recycle_values(colour, n_axes, "colour")
  }
  linetype_vec <- .recycle_values(linetype, n_axes, "linetype")
  linewidth_vec <- .resolve_linewidth(
    linewidth,
    n_axes,
    default_linewidth,
    "linewidth"
  )
  arrow_list <- .recycle_arrow_spec(arrow, n_axes, "arrow")

  lapply(seq_len(n_axes), function(g) {
    axis_spec <- axis_specs[[g]]
    intercept <- .intercept_unit(axis_spec)

    gp <- grid::gpar(
      col = colour_vec[[g]],
      fill = colour_vec[[g]],
      lwd = linewidth_vec[[g]] * ggplot2::.pt,
      lty = linetype_vec[[g]],
      lineend = "butt"
    )

    line_grob <- if (axis_spec$int_axis == "x") {
      grid::segmentsGrob(
        x0 = intercept,
        x1 = intercept,
        y0 = grid::unit(0, "npc"),
        y1 = grid::unit(1, "npc"),
        gp = gp,
        arrow = arrow_list[[g]]
      )
    } else {
      grid::segmentsGrob(
        x0 = grid::unit(0, "npc"),
        x1 = grid::unit(1, "npc"),
        y0 = intercept,
        y1 = intercept,
        gp = gp,
        arrow = arrow_list[[g]]
      )
    }

    bounds <- if (axis_spec$int_axis == "x") {
      list(
        xmin = if (axis_spec$npc) -Inf else axis_spec$intercept,
        xmax = if (axis_spec$npc) Inf else axis_spec$intercept,
        ymin = -Inf,
        ymax = Inf
      )
    } else {
      list(
        xmin = -Inf,
        xmax = Inf,
        ymin = if (axis_spec$npc) -Inf else axis_spec$intercept,
        ymax = if (axis_spec$npc) Inf else axis_spec$intercept
      )
    }

    .new_annotation_layer(line_grob, bounds, layout)
  })
}

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
#' @export
panel_grid <- function(
  xintercept = NULL,
  yintercept = NULL,
  xmin = -Inf,
  xmax = Inf,
  ymin = -Inf,
  ymax = Inf,
  colour = NULL,
  linewidth = NULL,
  linetype = NULL,
  layout = NULL
) {
  axis_specs <- .build_axis_specs(xintercept, yintercept)
  n_axes <- length(axis_specs)

  xmin_npc <- inherits(xmin, "AsIs")
  xmin <- as.numeric(xmin)
  xmax_npc <- inherits(xmax, "AsIs")
  xmax <- as.numeric(xmax)
  ymin_npc <- inherits(ymin, "AsIs")
  ymin <- as.numeric(ymin)
  ymax_npc <- inherits(ymax, "AsIs")
  ymax <- as.numeric(ymax)

  current_theme <- ggplot2::get_theme()

  axis_defaults <- lapply(axis_specs, function(axis_spec) {
    grid_el <- .default(
      .calc_theme_element(
        .panel_grid_hierarchy(axis_spec$int_axis),
        current_theme
      ),
      ggplot2::element_line(colour = "grey92", linewidth = 1, linetype = 1)
    )

    list(
      colour = .theme_value(grid_el, "colour", "grey92"),
      linewidth = .theme_value(grid_el, "linewidth", 1),
      linetype = .theme_value(grid_el, "linetype", 1)
    )
  })

  default_colour <- vapply(axis_defaults, function(x) x$colour, character(1))
  default_linewidth <- vapply(
    axis_defaults,
    function(x) x$linewidth,
    numeric(1)
  )
  default_linetype <- vapply(axis_defaults, function(x) x$linetype, numeric(1))

  colour_vec <- if (is.null(colour)) {
    default_colour
  } else {
    .recycle_values(colour, n_axes, "colour")
  }
  linetype_vec <- if (is.null(linetype)) {
    default_linetype
  } else {
    .recycle_values(linetype, n_axes, "linetype")
  }
  linewidth_vec <- .resolve_linewidth(
    linewidth,
    n_axes,
    default_linewidth,
    "linewidth"
  )

  extent_bound <- function(value, is_npc, lower) {
    if (is_npc) {
      list(grob = grid::unit(value, "npc"), data = if (lower) -Inf else Inf)
    } else {
      list(grob = grid::unit(if (lower) 0 else 1, "npc"), data = value)
    }
  }

  layers <- lapply(seq_len(n_axes), function(g) {
    axis_spec <- axis_specs[[g]]
    intercept <- axis_spec$intercept
    npc_int <- axis_spec$npc

    # Filter only when both intercept and crop bounds are finite data values
    if (axis_spec$int_axis == "x") {
      if (!npc_int && !xmin_npc && is.finite(xmin) && intercept < xmin) {
        return(NULL)
      }
      if (!npc_int && !xmax_npc && is.finite(xmax) && intercept > xmax) {
        return(NULL)
      }
    } else {
      if (!npc_int && !ymin_npc && is.finite(ymin) && intercept < ymin) {
        return(NULL)
      }
      if (!npc_int && !ymax_npc && is.finite(ymax) && intercept > ymax) {
        return(NULL)
      }
    }

    if (axis_spec$int_axis == "x") {
      lower <- extent_bound(ymin, ymin_npc, TRUE)
      upper <- extent_bound(ymax, ymax_npc, FALSE)
    } else {
      lower <- extent_bound(xmin, xmin_npc, TRUE)
      upper <- extent_bound(xmax, xmax_npc, FALSE)
    }

    intercept_unit <- .intercept_unit(axis_spec)

    gp <- grid::gpar(
      col = colour_vec[[g]],
      lwd = linewidth_vec[[g]] * ggplot2::.pt,
      lty = linetype_vec[[g]],
      lineend = "butt"
    )

    line_grob <- if (axis_spec$int_axis == "x") {
      grid::segmentsGrob(
        x0 = intercept_unit,
        x1 = intercept_unit,
        y0 = lower$grob,
        y1 = upper$grob,
        gp = gp
      )
    } else {
      grid::segmentsGrob(
        x0 = lower$grob,
        x1 = upper$grob,
        y0 = intercept_unit,
        y1 = intercept_unit,
        gp = gp
      )
    }

    bounds <- if (axis_spec$int_axis == "x") {
      list(
        xmin = if (npc_int) -Inf else intercept,
        xmax = if (npc_int) Inf else intercept,
        ymin = lower$data,
        ymax = upper$data
      )
    } else {
      list(
        xmin = lower$data,
        xmax = upper$data,
        ymin = if (npc_int) -Inf else intercept,
        ymax = if (npc_int) Inf else intercept
      )
    }

    .new_annotation_layer(line_grob, bounds, layout)
  })

  Filter(Negate(is.null), layers)
}

# panel_background ------------------------------------------------------------

#' Annotate a panel background region
#'
#' Draws filled rectangles over the panel. Defaults to the `panel.background`
#' fill from the set theme at full opacity, making it useful for layering a
#' solid background over existing content. Should be placed before geom layers.
#'
#' @param xmin,xmax Left and right edges of the rectangle in data coordinates.
#'   Defaults to `-Inf` and `Inf`. Use [I()] for normalised coordinates (0-1).
#'   May be a vector for multiple rectangles. Bounds may be mixed freely,
#'   e.g. `xmin = I(0.5), xmax = Inf` fills from 50% to the right panel edge.
#' @param ymin,ymax Bottom and top edges of the rectangle in data coordinates.
#'   Defaults to `-Inf` and `Inf`. Use [I()] for normalised coordinates (0-1).
#'   May be a vector for multiple rectangles.
#' @param fill Fill colour. Defaults to the `panel.background` fill from the
#'   set theme, falling back to `"white"`. May be a vector the same length as
#'   the bounds to style each rectangle individually.
#' @param alpha Opacity. Defaults to `1` (fully opaque). May be a vector.
#' @param colour Border colour. Defaults to the resolved `fill` value, giving
#'   a seamless border. May be a vector.
#' @param linewidth Inherits from `panel.border` in the set theme. Supports
#'   `rel()`. May be a vector.
#' @param linetype Border linetype. Defaults to `0` (none). May be a vector.
#' @param layout Controls which panels the annotation appears in. `NULL`
#'   (default) repeats in all panels. An integer targets a specific panel.
#'   `"fixed"` repeats in all panels ignoring faceting variables. See
#'   [ggplot2::layer()] for full details.
#'
#' @return A list containing annotation layers.
#' @export
panel_background <- function(
  xmin = -Inf,
  xmax = Inf,
  ymin = -Inf,
  ymax = Inf,
  fill = NULL,
  alpha = 1,
  colour = NULL,
  linewidth = NULL,
  linetype = 0,
  layout = NULL
) {
  current_theme <- ggplot2::get_theme()

  panel_bg <- ggplot2::calc_element(
    "panel.background",
    current_theme,
    skip_blank = TRUE
  )
  panel_border <- ggplot2::calc_element(
    "panel.border",
    current_theme,
    skip_blank = TRUE
  )

  default_fill <- .theme_value(panel_bg, "fill", "white")
  default_linewidth <- .theme_value(panel_border, "linewidth", 0.5)

  if (is.null(fill)) {
    fill <- default_fill
  }

  if (is.null(colour)) {
    colour <- fill
  }

  xmin_npc <- inherits(xmin, "AsIs")
  xmax_npc <- inherits(xmax, "AsIs")
  ymin_npc <- inherits(ymin, "AsIs")
  ymax_npc <- inherits(ymax, "AsIs")

  xmin <- as.numeric(xmin)
  xmax <- as.numeric(xmax)
  ymin <- as.numeric(ymin)
  ymax <- as.numeric(ymax)

  size_inputs <- list(xmin, xmax, ymin, ymax, fill, alpha, colour, linetype)
  if (!is.null(linewidth)) {
    size_inputs <- c(size_inputs, list(linewidth))
  }

  n <- do.call(vctrs::vec_size_common, size_inputs)

  xmin_vec <- .recycle_values(xmin, n, "xmin")
  xmax_vec <- .recycle_values(xmax, n, "xmax")
  ymin_vec <- .recycle_values(ymin, n, "ymin")
  ymax_vec <- .recycle_values(ymax, n, "ymax")
  fill_vec <- .recycle_values(fill, n, "fill")
  alpha_vec <- .recycle_values(alpha, n, "alpha")
  colour_vec <- .recycle_values(colour, n, "colour")
  linetype_vec <- .recycle_values(linetype, n, "linetype")
  linewidth_vec <- .resolve_linewidth(
    linewidth,
    n,
    rep(default_linewidth, n),
    "linewidth"
  )

  bounds_to_units <- function(value, is_npc, lower) {
    if (is_npc) {
      list(grob = grid::unit(value, "npc"), data = if (lower) -Inf else Inf)
    } else {
      list(grob = grid::unit(if (lower) 0 else 1, "npc"), data = value)
    }
  }

  lapply(seq_len(n), function(i) {
    x1 <- bounds_to_units(xmin_vec[[i]], xmin_npc, TRUE)
    x2 <- bounds_to_units(xmax_vec[[i]], xmax_npc, FALSE)
    y1 <- bounds_to_units(ymin_vec[[i]], ymin_npc, TRUE)
    y2 <- bounds_to_units(ymax_vec[[i]], ymax_npc, FALSE)

    rect_grob <- grid::rectGrob(
      x = x1$grob,
      y = y1$grob,
      width = x2$grob - x1$grob,
      height = y2$grob - y1$grob,
      just = c("left", "bottom"),
      gp = grid::gpar(
        fill = scales::alpha(fill_vec[[i]], alpha_vec[[i]]),
        col = colour_vec[[i]],
        lwd = linewidth_vec[[i]] * ggplot2::.pt,
        lty = linetype_vec[[i]]
      )
    )

    .new_annotation_layer(
      rect_grob,
      list(
        xmin = x1$data,
        xmax = x2$data,
        ymin = y1$data,
        ymax = y2$data
      ),
      layout
    )
  })
}

# panel_shade -----------------------------------------------------------------

#' Annotate a shaded panel region
#'
#' A convenience wrapper around `panel_background()` with a smart shade default
#' which blends the panel background fill with `jumble::slate` at 25% opacity
#' with no border. Should be placed before geom layers.
#'
#' @inheritParams panel_background
#' @param fill Fill colour.
#' @param alpha Opacity. Defaults to `0.2`. May be a vector.
#' @param colour Border colour. Defaults to `"transparent"`. May be a vector.
#' @param linetype Border linetype. Defaults to `1`. May be a vector.
#'
#' @return A list containing annotation layers.
#' @export
panel_shade <- function(
  xmin = -Inf,
  xmax = Inf,
  ymin = -Inf,
  ymax = Inf,
  fill = NULL,
  alpha = 0.2,
  colour = "transparent",
  linewidth = NULL,
  linetype = 1,
  layout = NULL
) {

  if (is.null(fill)) {
    fill <- .shade_fill()
  }

  panel_background(
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax,
    fill = fill,
    alpha = alpha,
    colour = colour,
    linewidth = linewidth,
    linetype = linetype,
    layout = layout
  )
}


# axis-helpers.R ---------------------------------------------------------------

# Small null-default helper
.default <- function(x, default) {
  if (is.null(x)) default else x
}

# Build a list of axis specs from xintercept / yintercept.
# Each element is:
#   list(
#     int_axis  = "x" or "y",
#     intercept = numeric scalar,
#     npc       = logical
#   )
#
# int_axis:
#   "x" = xintercept (vertical line, breaks run along y)
#   "y" = yintercept (horizontal line, breaks run along x)
.build_axis_specs <- function(xintercept, yintercept) {
  if (is.null(xintercept) && is.null(yintercept)) {
    rlang::abort("Must supply at least one of `xintercept` or `yintercept`.")
  }

  x_specs <- if (!is.null(xintercept)) {
    npc <- inherits(xintercept, "AsIs")
    vals <- as.numeric(xintercept)
    lapply(vals, function(v) list(int_axis = "x", intercept = v, npc = npc))
  } else {
    list()
  }

  y_specs <- if (!is.null(yintercept)) {
    npc <- inherits(yintercept, "AsIs")
    vals <- as.numeric(yintercept)
    lapply(vals, function(v) list(int_axis = "y", intercept = v, npc = npc))
  } else {
    list()
  }

  c(x_specs, y_specs)
}

# One breaks spec
.new_break_spec <- function(x) {
  list(
    vals = as.numeric(x),
    npc = inherits(x, "AsIs")
  )
}

# vctrs recycling for atomic vectors / list vectors
.recycle_values <- function(x, size, arg) {
  x_size <- vctrs::vec_size(x)

  if (!(x_size %in% c(1L, size))) {
    rlang::abort(sprintf(
      "`%s` must have size 1 or %d, not %d.",
      arg,
      size,
      x_size
    ))
  }

  vctrs::vec_recycle(x, size = size)
}

# Recycling for grouped list specs like breaks/labels
.recycle_list_spec <- function(x, size, arg) {
  if (is.list(x) && !inherits(x, "AsIs")) {
    x_size <- vctrs::vec_size(x)

    if (!(x_size %in% c(1L, size))) {
      rlang::abort(sprintf(
        "`%s` must have size 1 or %d, not %d.",
        arg,
        size,
        x_size
      ))
    }

    return(vctrs::vec_recycle(x, size = size))
  }

  rep(list(x), size)
}

# Recycling for arrow specs
.recycle_arrow_spec <- function(arrow, size, arg = "arrow") {
  if (is.null(arrow)) {
    return(rep(list(NULL), size))
  }

  if (inherits(arrow, "arrow")) {
    return(rep(list(arrow), size))
  }

  if (is.list(arrow)) {
    return(.recycle_values(arrow, size, arg))
  }

  rlang::abort(
    sprintf(
      "`%s` must be either `NULL`, a single `grid::arrow()`, or a list of arrows.",
      arg
    )
  )
}

# Normalise breaks to one spec per axis
.normalise_break_specs <- function(breaks, n_axes) {
  if (is.list(breaks) && !inherits(breaks, "AsIs")) {
    specs <- .recycle_list_spec(breaks, n_axes, "breaks")
    lapply(specs, .new_break_spec)
  } else {
    rep(list(.new_break_spec(breaks)), n_axes)
  }
}

# Labels normalised to one character vector per axis
.normalise_label_specs <- function(labels, break_specs, n_axes) {
  label_specs <- if (is.list(labels) && !is.function(labels)) {
    .recycle_list_spec(labels, n_axes, "labels")
  } else {
    rep(list(labels), n_axes)
  }

  Map(
    function(lbl, brk, i) {
      out <- if (is.null(lbl)) {
        as.character(brk$vals)
      } else if (is.function(lbl)) {
        lbl(brk$vals)
      } else {
        lbl
      }

      out <- .recycle_values(
        out,
        length(brk$vals),
        sprintf("labels (group %d)", i)
      )
      as.character(out)
    },
    label_specs,
    break_specs,
    seq_len(n_axes)
  )
}

# Reconcile axis specs and break specs
.align_axis_breaks <- function(axis_specs, breaks) {
  break_specs <- .normalise_break_specs(breaks, length(axis_specs))

  list(
    axis_specs = axis_specs,
    break_specs = break_specs,
    n_axes = length(axis_specs)
  )
}

# Bounds for a single annotation grob
#
# int_axis = "x" -> vertical line:
#   x pinned at intercept, y pinned at break
#
# int_axis = "y" -> horizontal line:
#   y pinned at intercept, x pinned at break
#
# npc intercept / break -> use ±Inf so the grob's own npc coordinates apply
.annotation_bounds <- function(axis_spec, break_value, break_npc) {
  npc_int <- axis_spec$npc
  intercept <- axis_spec$intercept

  if (axis_spec$int_axis == "x") {
    x_bounds <- if (npc_int) {
      list(xmin = -Inf, xmax = Inf)
    } else {
      list(xmin = intercept, xmax = intercept)
    }

    y_bounds <- if (break_npc) {
      list(ymin = -Inf, ymax = Inf)
    } else {
      list(ymin = break_value, ymax = break_value)
    }
  } else {
    y_bounds <- if (npc_int) {
      list(ymin = -Inf, ymax = Inf)
    } else {
      list(ymin = intercept, ymax = intercept)
    }

    x_bounds <- if (break_npc) {
      list(xmin = -Inf, xmax = Inf)
    } else {
      list(xmin = break_value, xmax = break_value)
    }
  }

  c(x_bounds, y_bounds)
}

# Wrap a grob in a ggplot2 annotation layer
.new_annotation_layer <- function(grob, bounds, layout) {
  ggplot2::layer(
    geom = ggplot2::GeomCustomAnn,
    stat = "identity",
    data = NULL,
    mapping = ggplot2::aes(),
    position = "identity",
    params = c(list(grob = grob, na.rm = FALSE), bounds),
    layout = layout
  )
}

# NPC coordinate for the intercept direction
# npc intercept -> use intercept directly
# data intercept -> use 0.5 npc because annotation bounds pin the data position
.intercept_unit <- function(axis_spec) {
  if (axis_spec$npc) {
    grid::unit(axis_spec$intercept, "npc")
  } else {
    grid::unit(0.5, "npc")
  }
}

# NPC coordinate for the break direction
# npc break -> use break directly
# data break -> use 0.5 npc because annotation bounds pin the data position
.break_unit <- function(break_value, break_npc) {
  if (break_npc) grid::unit(break_value, "npc") else grid::unit(0.5, "npc")
}

# Theme element resolution with fallback hierarchy
.calc_theme_element <- function(hierarchy, current_theme) {
  for (name in hierarchy) {
    el <- ggplot2::calc_element(name, current_theme, skip_blank = TRUE)
    if (!is.null(el) && !inherits(el, "element_blank")) {
      return(el)
    }
  }

  NULL
}

# Safe field pluck from theme/theme element objects.
# Works with current ggplot2 objects without relying on old slot access.
.theme_value <- function(x, name, default = NULL) {
  if (is.null(x) || inherits(x, "element_blank")) {
    return(default)
  }

  value <- tryCatch(x[[name]], error = function(e) NULL)

  if (is.null(value)) {
    value <- tryCatch(as.list(x)[[name]], error = function(e) NULL)
  }

  .default(value, default)
}

.theme_text_size_pt <- function(current_theme) {
  text_el <- ggplot2::calc_element("text", current_theme, skip_blank = TRUE)
  .theme_value(text_el, "size", 11)
}

.theme_spacing_pt <- function(current_theme) {
  spacing <- .theme_value(current_theme, "spacing", grid::unit(5.5, "pt"))
  as.numeric(grid::convertUnit(spacing, "pt"))
}

.theme_length_to_pt <- function(length_el, current_theme) {
  if (is.null(length_el)) {
    return(0.5 * .theme_text_size_pt(current_theme))
  }

  if (inherits(length_el, "rel")) {
    return(as.numeric(length_el) * .theme_spacing_pt(current_theme))
  }

  if (inherits(length_el, "unit")) {
    return(as.numeric(grid::convertUnit(length_el, "pt")))
  }

  if (is.numeric(length_el)) {
    return(as.numeric(length_el))
  }

  0.5 * .theme_text_size_pt(current_theme)
}

# Resolve text margin in points using effective side
.text_margin_pt <- function(text_el, side) {
  margin <- .theme_value(text_el, "margin", NULL)

  if (
    is.null(margin) || !(inherits(margin, "margin") || inherits(margin, "unit"))
  ) {
    return(2)
  }

  idx <- switch(
    side,
    top = 1L,
    right = 2L,
    bottom = 3L,
    left = 4L,
    1L
  )

  as.numeric(grid::convertUnit(margin[[idx]], "pt"))
}

# Resolve signed offset/length
.resolve_offset <- function(x, size, theme_pt, arg = "length") {
  if (inherits(x, "rel")) {
    vals <- .recycle_values(as.numeric(x), size, arg)
    return(list(pts = abs(vals) * theme_pt, flip = vals < 0))
  }

  if (inherits(x, "unit")) {
    vals <- .recycle_values(as.numeric(grid::convertUnit(x, "pt")), size, arg)
    return(list(pts = abs(vals), flip = vals < 0))
  }

  vals <- .recycle_values(as.numeric(x), size, arg)
  list(pts = abs(vals), flip = vals < 0)
}

# Resolve linewidth, supporting rel()
.resolve_linewidth <- function(linewidth, size, base, arg = "linewidth") {
  if (is.null(linewidth)) {
    return(base)
  }

  if (inherits(linewidth, "rel")) {
    vals <- .recycle_values(as.numeric(linewidth), size, arg)
    return(vals * base)
  }

  .recycle_values(linewidth, size, arg)
}

# Effective text side from axis direction and flip
.effective_side <- function(int_axis, flip) {
  if (int_axis == "x") {
    if (flip) "left" else "right"
  } else {
    if (flip) "bottom" else "top"
  }
}

.deg2rad <- function(deg) deg * pi / 180

.get_hjust <- function(side, angle, flip = FALSE) {
  rad <- .deg2rad(angle)
  cosine <- sign(round(cos(rad), 3)) / 2 + 0.5
  sine <- sign(round(sin(rad), 3)) / 2 + 0.5

  h <- switch(
    side,
    left = cosine,
    right = 1 - cosine,
    top = 1 - sine,
    bottom = sine
  )

  if (flip) 1 - h else h
}

.get_vjust <- function(side, angle, flip = FALSE) {
  rad <- .deg2rad(angle)
  cosine <- sign(round(cos(rad), 3)) / 2 + 0.5
  sine <- sign(round(sin(rad), 3)) / 2 + 0.5

  v <- switch(
    side,
    left = 1 - sine,
    right = sine,
    top = 1 - cosine,
    bottom = cosine
  )

  if (flip) 1 - v else v
}

# Theme hierarchies by axis direction
# int_axis == "x" means xintercept -> vertical line -> y-axis styling
.axis_text_hierarchy <- function(int_axis) {
  if (int_axis == "x") {
    c("axis.text.y", "axis.text")
  } else {
    c("axis.text.x", "axis.text")
  }
}

.axis_ticks_hierarchy <- function(int_axis) {
  if (int_axis == "x") {
    c("axis.ticks.y", "axis.ticks")
  } else {
    c("axis.ticks.x", "axis.ticks")
  }
}

.axis_ticks_length_hierarchy <- function(int_axis) {
  if (int_axis == "x") {
    c("axis.ticks.length.y", "axis.ticks.length")
  } else {
    c("axis.ticks.length.x", "axis.ticks.length")
  }
}

.axis_line_hierarchy <- function(int_axis) {
  if (int_axis == "x") {
    c("axis.line.y", "axis.line")
  } else {
    c("axis.line.x", "axis.line")
  }
}

.axis_bracket_hierarchy <- function(int_axis) {
  if (int_axis == "x") {
    c("axis.ticks.y", "axis.ticks", "axis.line.y", "axis.line", "line")
  } else {
    c("axis.ticks.x", "axis.ticks", "axis.line.x", "axis.line", "line")
  }
}

.panel_grid_hierarchy <- function(int_axis) {
  if (int_axis == "x") {
    c("panel.grid.major.x", "panel.grid.major", "panel.grid")
  } else {
    c("panel.grid.major.y", "panel.grid.major", "panel.grid")
  }
}

.shade_fill <- function() {
  panel_background <- ggplot2::get_theme()$panel.background@fill
  if (.is_col_dark(panel_background)) {
    blends::screen(panel_background, jumble::slate)
  }
  else {
    blends::multiply(panel_background, jumble::slate)
  }
}

