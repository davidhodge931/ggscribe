# axis_text -------------------------------------------------------------------

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

  text_el <- .resolve_theme_el(c("axis.text.x", "axis.text"), current_theme) %||%
    ggplot2::element_text(colour = "black", size = 11, family = "")

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
    if (!is.null(tm) && inherits(tm, c("margin", "unit")) && length(tm) >= 1)
      as.numeric(grid::convertUnit(tm[[1]], "pt"))
    else 2
  }

  # ---- Per-break indexing --------------------------------------------------
  n_per_group <- vapply(breaks_list, \(b) length(b$vals), integer(1))
  n_total     <- sum(n_per_group)
  offsets     <- c(0L, cumsum(n_per_group[-length(n_per_group)]))

  # All style args recycle to n_total — one value per break across all axes
  colour_vec <- rep_len(colour %||% text_el$colour %||% "black", n_total)
  size_vec   <- rep_len(size   %||% text_el$size   %||% 11,      n_total)
  family_vec <- rep_len(family %||% text_el$family %||% "",      n_total)
  angle_vec  <- rep_len(angle, n_total)

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

  # Pre-compute effective position per break for auto hjust/vjust
  group_for_break <- rep(seq_len(n_groups), n_per_group)
  eff_pos_vec <- mapply(
    \(g_idx, fl) .effective_pos(groups[[g_idx]]$int_axis, fl),
    group_for_break, flip_vec
  )

  hjust_vec <- if (!is.null(hjust)) {
    rep_len(hjust, n_total)
  } else {
    mapply(\(pos, ang) .get_hjust(pos, ang, FALSE), eff_pos_vec, angle_vec)
  }
  vjust_vec <- if (!is.null(vjust)) {
    rep_len(vjust, n_total)
  } else {
    mapply(\(pos, ang) .get_vjust(pos, ang, FALSE), eff_pos_vec, angle_vec)
  }

  # Normalise labels to list of n_groups
  labels_list <- lapply(seq_len(n_groups), \(g) {
    brks <- breaks_list[[g]]$vals
    lbl  <- if (is.list(labels)) labels[[g]] else labels
    if (is.null(lbl))          as.character(brks)
    else if (is.function(lbl)) lbl(brks)
    else                       rep_len(lbl, length(brks))
  })

  # ---- Draw ----------------------------------------------------------------

  unlist(lapply(seq_len(n_groups), \(g) {
    grp    <- groups[[g]]
    brks   <- breaks_list[[g]]
    offset <- offsets[[g]]
    perp   <- .perp_unit(grp)
    lbs    <- labels_list[[g]]

    lapply(seq_along(brks$vals), \(i) {
      gi           <- offset + i
      bv           <- brks$vals[[i]]
      flip         <- flip_vec[[gi]]
      total_length <- grid::unit(len_pts_vec[[gi]] + margin_pts, "pt")
      along        <- .along_unit(bv, brks$npc)

      gp <- grid::gpar(
        col        = colour_vec[[gi]],
        fontsize   = size_vec[[gi]],
        fontfamily = family_vec[[gi]]
      )

      just <- c(hjust = hjust_vec[[gi]], vjust = vjust_vec[[gi]])
      ang  <- angle_vec[[gi]]

      text_grob <- if (grp$int_axis == "x") {
        text_x <- if (flip) perp - total_length else perp + total_length
        grid::textGrob(lbs[[i]], x = text_x, y = along,
                       just = just, rot = ang, gp = gp)
      } else {
        text_y <- if (flip) perp - total_length else perp + total_length
        grid::textGrob(lbs[[i]], x = along, y = text_y,
                       just = just, rot = ang, gp = gp)
      }

      .make_ann_layer(text_grob, .anno_pos(grp, bv, brks$npc), layout)
    })
  }), recursive = FALSE)
}
