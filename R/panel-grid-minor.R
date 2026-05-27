#' #' Annotate minor panel gridlines
#' #'
#' #' Draws minor gridlines at specified positions, with style defaults taken from the
#' #' `panel.grid.minor` element of the set theme.
#' #'
#' #' @param xintercept One or more x positions for vertical minor gridlines, in data
#' #'   coordinates or wrapped in [I()] for normalised panel coordinates (npc).
#' #' @param yintercept One or more y positions for horizontal minor gridlines, in data
#' #'   coordinates or wrapped in [I()] for normalised panel coordinates (npc).
#' #' @param xmin,xmax Clip the span of horizontal minor gridlines. Defaults to `-Inf`/`Inf`.
#' #'   Supports [I()] for normalised coordinates.
#' #' @param ymin,ymax Clip the span of vertical minor gridlines. Defaults to `-Inf`/`Inf`.
#' #'   Supports [I()] for normalised coordinates.
#' #' @param colour Inherits from `panel.grid.minor`. Must be a scalar (length 1).
#' #' @param linewidth Inherits from `panel.grid.minor`. Supports `rel()`. Must be a scalar (length 1).
#' #' @param linetype Inherits from `panel.grid.minor`. Must be a scalar (length 1).
#' #' @param layout Controls which panels the annotation appears in.
#' #'
#' #' @return A list of ggplot2 annotation layers.
#' #' @export
#' panel_grid_minor <- function(
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
#'   # --- Minor Grid Hierarchy Resolution ---
#'   .get_minor_grid_el <- function(int_axis) {
#'     axis_name <- if (int_axis == "x") "y" else "x"
#'     .resolve_theme_el(c(
#'       paste0("panel.grid.minor.", axis_name),
#'       "panel.grid.minor",
#'       "panel.grid"
#'     ), current_theme)
#'   }
#'
#'   grid_el         <- .get_minor_grid_el(groups[[1]]$int_axis) %||%
#'     list(colour = "grey92", linewidth = 0.5, linetype = 1)
#'
#'   theme_colour    <- grid_el$colour    %||% "grey92"
#'   theme_linewidth <- grid_el$linewidth %||% 0.5  # Minor grids are typically thinner
#'   theme_linetype  <- grid_el$linetype  %||% 1
#'
#'   # --- Scalar Resolution ---
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
