#' # panel_grid ------------------------------------------------------------------
#'
#' #' Annotate panel gridlines
#' #'
#' #' Draws gridlines at specified positions, with style defaults taken from the
#' #' `panel.grid.major` element of the set theme. Works like [axis_line()] but
#' #' uses the grid theme rather than the axis line theme.
#' #'
#' #' @param xintercept One or more x positions for vertical gridlines, in data
#' #'   coordinates or wrapped in [I()] for normalised panel coordinates (npc).
#' #'   May be a vector; each value produces a separate gridline.
#' #' @param yintercept One or more y positions for horizontal gridlines, in data
#' #'   coordinates or wrapped in [I()] for normalised panel coordinates (npc).
#' #'   May be a vector; each value produces a separate gridline.
#' #' @param xmin,xmax Clip the span of horizontal gridlines. Defaults to `-Inf`/`Inf`.
#' #'   Supports [I()] for normalised coordinates.
#' #' @param ymin,ymax Clip the span of vertical gridlines. Defaults to `-Inf`/`Inf`.
#' #'   Supports [I()] for normalised coordinates.
#' #' @param colour Inherits from `panel.grid.major` in the set theme. May be a
#' #'   vector the same length as the total number of lines.
#' #' @param linewidth Inherits from `panel.grid.major` in the set theme. Supports
#' #'   `rel()`. May be a vector the same length as the total number of lines.
#' #' @param linetype Inherits from `panel.grid.major` in the set theme. May be a
#' #'   vector the same length as the total number of lines.
#' #' @param layout Controls which panels the annotation appears in. `NULL`
#' #'   (default) repeats in all panels. An integer targets a specific panel.
#' #'   `"fixed"` repeats in all panels ignoring faceting variables. See
#' #'   [ggplot2::layer()] for full details.
#' panel_grid <- function(
#'     xintercept = NULL,
#'     yintercept = NULL,
#'     xmin       = -Inf,
#'     xmax       = Inf,
#'     ymin       = -Inf,
#'     ymax       = Inf,
#'     colour     = NULL,
#'     linewidth  = NULL,
#'     linetype   = NULL,
#'     layout     = NULL
#' ) {
#'   # --- Strict Vector Length Checks ---
#'   if (!is.null(colour) && length(colour) != 1) {
#'     rlang::abort("`colour` must be a scalar (length 1).")
#'   }
#'   if (!is.null(linewidth) && length(linewidth) != 1) {
#'     rlang::abort("`linewidth` must be a scalar (length 1).")
#'   }
#'   if (!is.null(linetype) && length(linetype) != 1) {
#'     rlang::abort("`linetype` must be a scalar (length 1).")
#'   }
#'   # -----------------------------------
#'
#'   # --- Auto-resolve mixed bounding coordinates ---
#'   if (any(inherits(xmin, "AsIs")) || any(inherits(xmax, "AsIs"))) {
#'     if (identical(xmin, -Inf)) xmin <- I(0)
#'     if (identical(xmax, Inf))  xmax <- I(1)
#'   }
#'   if (any(inherits(ymin, "AsIs")) || any(inherits(ymax, "AsIs"))) {
#'     if (identical(ymin, -Inf)) ymin <- I(0)
#'     if (identical(ymax, Inf))  ymax <- I(1)
#'   }
#'   # -----------------------------------------------
#'
#'   xmin_is_normalized <- inherits(xmin, "AsIs")
#'   xmax_is_normalized <- inherits(xmax, "AsIs")
#'   ymin_is_normalized <- inherits(ymin, "AsIs")
#'   ymax_is_normalized <- inherits(ymax, "AsIs")
#'
#'   groups   <- .build_axis_groups(xintercept, yintercept)
#'   n_groups <- length(groups)
#'
#'   if (n_groups == 0) return(list())
#'
#'   current_theme <- ggplot2::theme_get()
#'
#'   .get_grid_el <- function(int_axis) {
#'     axis_name <- if (int_axis == "x") "y" else "x"
#'     .resolve_theme_el(c(
#'       paste0("panel.grid.major.", axis_name),
#'       "panel.grid.major",
#'       "panel.grid"
#'     ), current_theme)
#'   }
#'
#'   grid_el         <- .get_grid_el(groups[[1]]$int_axis) %||%
#'     list(colour = "grey92", linewidth = 1, linetype = 1)
#'
#'   theme_colour    <- grid_el$colour    %||% "grey92"
#'   theme_linewidth <- grid_el$linewidth %||% 1
#'   theme_linetype  <- grid_el$linetype  %||% 1
#'
#'   # --- Simplified Scalar Resolution ---
#'   final_colour    <- colour %||% theme_colour
#'   final_linetype  <- linetype %||% theme_linetype
#'   final_linewidth <- if (is.null(linewidth)) {
#'     theme_linewidth
#'   } else if (inherits(linewidth, "rel")) {
#'     as.numeric(linewidth) * theme_linewidth
#'   } else {
#'     linewidth
#'   }
#'
#'   # Strip classes once up front for clean looping
#'   xmin_num <- as.numeric(xmin)
#'   xmax_num <- as.numeric(xmax)
#'   ymin_num <- as.numeric(ymin)
#'   ymax_num <- as.numeric(ymax)
#'
#'   lapply(seq_len(n_groups), \(g) {
#'     grp  <- groups[[g]]
#'     perp <- .perp_unit(grp)
#'
#'     gp <- grid::gpar(
#'       col     = final_colour,
#'       lwd     = final_linewidth * ggplot2::.pt,
#'       lty     = final_linetype,
#'       lineend = "butt"
#'     )
#'
#'     # --- Construct Bounded Grobs ---
#'     if (grp$int_axis == "x") {
#'       y_bottom <- if (ymin_is_normalized) grid::unit(ymin_num, "npc") else grid::unit(0, "npc")
#'       y_top    <- if (ymax_is_normalized) grid::unit(ymax_num, "npc") else grid::unit(1, "npc")
#'
#'       line_grob <- grid::segmentsGrob(
#'         x0 = perp, x1 = perp,
#'         y0 = y_bottom, y1 = y_top,
#'         gp = gp
#'       )
#'     } else {
#'       x_left  <- if (xmin_is_normalized) grid::unit(xmin_num, "npc") else grid::unit(0, "npc")
#'       x_right <- if (xmax_is_normalized) grid::unit(xmax_num, "npc") else grid::unit(1, "npc")
#'
#'       line_grob <- grid::segmentsGrob(
#'         x0 = x_left, x1 = x_right,
#'         y0 = perp, y1 = perp,
#'         gp = gp
#'       )
#'     }
#'
#'     # --- Annotation Placement Positioning ---
#'     anno_pos <- if (grp$int_axis == "x") {
#'       list(
#'         xmin = if (grp$npc) -Inf else grp$intercept,
#'         xmax = if (grp$npc) Inf  else grp$intercept,
#'         ymin = if (ymin_is_normalized) -Inf else ymin_num,
#'         ymax = if (ymax_is_normalized) Inf  else ymax_num
#'       )
#'     } else {
#'       list(
#'         xmin = if (xmin_is_normalized) -Inf else xmin_num,
#'         xmax = if (xmax_is_normalized) Inf  else xmax_num,
#'         ymin = if (grp$npc) -Inf else grp$intercept,
#'         ymax = if (grp$npc) Inf  else grp$intercept
#'       )
#'     }
#'
#'     .make_ann_layer(line_grob, anno_pos, layout)
#'   })
#' }
