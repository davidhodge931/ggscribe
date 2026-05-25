# axis_text -------------------------------------------------------------------

#' Annotate axis text
#'
#' Draws text labels at specified break positions along a floating axis line,
#' with style defaults taken from the `axis.text` element of the set theme.
#' Requires `coord_cartesian(clip = "off")`.
#'
#' Text always sits on the positive side of the axis by default (right of
#' `xintercept` lines, above `yintercept` lines). Use a negative `length`
#' (e.g. `length = -rel(1)`) to place text on the opposite side.
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
#'   - A character vector the same length as `breaks`
#'   - A function taking break values and returning labels
#'   - A list the same length as the number of axes, each element being one
#'     of the above
#' @param length Offset from the axis line including tick length and margin.
#'   Supports `rel()`. Negative values place text on the opposite side.
#'   Defaults to `rel(1)`. May be a vector the same length as the number of
#'   axes.
#' @param angle Text rotation angle. Defaults to `0`. May be a vector the same
#'   length as the number of axes.
#' @param hjust,vjust Justification. Auto-calculated from axis direction and
#'   `angle` if `NULL`. Text always anchors to the tick end. May be a vector
#'   the same length as the number of axes.
#' @param colour Inherits from `axis.text` in the set theme. May be a vector
#'   the same length as the number of axes.
#' @param size Inherits from `axis.text` in the set theme. May be a vector
#'   the same length as the number of axes.
#' @param family Inherits from `axis.text` in the set theme. May be a vector
#'   the same length as the number of axes.
#' @param layout Controls which panels the annotation appears in. `NULL`
#'   (default) repeats in all panels. An integer targets a specific panel.
#'   `"fixed"` repeats in all panels ignoring faceting variables. See
#'   [ggplot2::layer()] for full details.
#'
#' @return A list of ggplot2 annotation layers.
#' @seealso [axis_line()], [axis_ticks()],
#'   [axis_bracket()], [reference_line()], [panel_shade()]
#' @export
axis_text <- function(
    xintercept = NULL,
    yintercept = NULL,
    breaks,
    labels     = NULL,
    length     = ggplot2::rel(1),
    angle      = 0,
    hjust      = NULL,
    vjust      = NULL,
    colour     = NULL,
    size       = NULL,
    family     = NULL,
    layout     = NULL
) {
  groups   <- .build_axis_groups(xintercept, yintercept)
  n_groups <- length(groups)

  rec         <- .reconcile(groups, breaks)
  groups      <- rec$groups
  breaks_list <- rec$breaks_list
  n_groups    <- rec$n

  current_theme <- ggplot2::theme_get()

  # Theme resolution
  text_el <- {
    el <- .resolve_theme_el(c("axis.text.x", "axis.text"), current_theme)
    el %||% ggplot2::element_text(colour = "black", size = 11, family = "")
  }

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

  margin_pts <- {
    tm <- text_el$margin
    if (!is.null(tm) && inherits(tm, c("margin", "unit")) && length(tm) >= 1) {
      as.numeric(grid::convertUnit(tm[[1]], "pt"))
    } else 2
  }

  len <- .resolve_length(length, n_groups, theme_length_pts)

  # Style args — one per group
  colour_vec <- rep_len(colour %||% text_el$colour %||% "black", n_groups)
  size_vec   <- rep_len(size   %||% text_el$size   %||% 11,      n_groups)
  family_vec <- rep_len(family %||% text_el$family %||% "",      n_groups)
  angle_vec  <- rep_len(angle, n_groups)

  hjust_provided <- !is.null(hjust)
  vjust_provided <- !is.null(vjust)
  hjust_vec_raw  <- if (hjust_provided) rep_len(hjust, n_groups) else NULL
  vjust_vec_raw  <- if (vjust_provided) rep_len(vjust, n_groups) else NULL

  # Normalise labels to list of n_groups
  labels_list <- lapply(seq_len(n_groups), \(g) {
    brks <- breaks_list[[g]]$vals
    lbl  <- if (is.list(labels)) labels[[g]] else labels
    if (is.null(lbl))          as.character(brks)
    else if (is.function(lbl)) lbl(brks)
    else                       rep_len(lbl, length(brks))
  })

  unlist(lapply(seq_len(n_groups), \(g) {
    grp          <- groups[[g]]
    brks         <- breaks_list[[g]]
    flip         <- len$flip[[g]]
    total_length <- grid::unit(len$pts[[g]] + margin_pts, "pt")
    perp         <- .perp_unit(grp)
    eff_pos      <- .effective_pos(grp$int_axis, flip)

    gp <- grid::gpar(
      col        = colour_vec[[g]],
      fontsize   = size_vec[[g]],
      fontfamily = family_vec[[g]]
    )

    ang   <- angle_vec[[g]]
    just  <- c(
      hjust = if (hjust_provided) hjust_vec_raw[[g]] else .get_hjust(eff_pos, ang, FALSE),
      vjust = if (vjust_provided) vjust_vec_raw[[g]] else .get_vjust(eff_pos, ang, FALSE)
    )

    lbs <- labels_list[[g]]

    lapply(seq_along(brks$vals), \(i) {
      bv    <- brks$vals[[i]]
      along <- .along_unit(bv, brks$npc)

      text_grob <- if (grp$int_axis == "x") {
        # Vertical axis: text sits right (or left if flip) of the line
        text_x <- if (flip) perp - total_length else perp + total_length
        grid::textGrob(lbs[[i]], x = text_x, y = along, just = just, rot = ang, gp = gp)
      } else {
        # Horizontal axis: text sits above (or below if flip) the line
        text_y <- if (flip) perp - total_length else perp + total_length
        grid::textGrob(lbs[[i]], x = along, y = text_y, just = just, rot = ang, gp = gp)
      }

      .make_ann_layer(text_grob, .anno_pos(grp, bv, brks$npc), layout)
    })
  }), recursive = FALSE)
}
