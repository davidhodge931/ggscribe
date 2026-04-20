#' # annotate_axis_title ---------------------------------------------------------
#'
#' #' Annotate an axis title
#' #'
#' #' Draws a title label along a panel edge, with style defaults taken from the
#' #' `axis.title` element of the set theme. Requires `coord_cartesian(clip = "off")`
#' #' to render outside the panel boundary.
#' #'
#' #' @param ... Not used. Allows trailing commas and named-argument style calls.
#' #' @param position One of `"top"`, `"bottom"`, `"left"`, or `"right"`.
#' #' @param label A single string for the title.
#' #' @param colour Inherits from `axis.title` in the set theme.
#' #' @param size Inherits from `axis.title` in the set theme.
#' #' @param family Inherits from `axis.title` in the set theme.
#' #' @param angle Text rotation. Auto-inferred from `position` if `NULL`
#' #'   (0 for top/bottom, 90 for left, -90 for right).
#' #' @param hjust,vjust Justification. Auto-calculated from `position` if `NULL`.
#' #' @param offset Distance from the panel edge as a [grid::unit()]. Defaults to
#' #'   `unit(30, "pt")`. Increase if the title overlaps tick labels.
#' #' @param elements_to One of `"keep"`, `"transparent"`, or `"blank"`. Controls
#' #'   whether the native theme axis title is suppressed. Defaults to `"keep"`.
#' #'
#' #' @return A list of ggplot2 annotation layers and theme elements.
#' #' @seealso [annotate_axis_text()], [annotate_axis_ticks()], [annotate_axis_line()]
#' #' @export
#' #'
#' #' @examples
#' #' library(ggplot2)
#' #'
#' #' set_theme(
#' #'   ggrefine::theme_grey(
#' #'     panel_heights = rep(unit(50, "mm"), 100),
#' #'     panel_widths  = rep(unit(75, "mm"), 100),
#' #'   )
#' #' )
#' #'
#' #' p <- ggplot(mtcars, aes(wt, mpg)) +
#' #'   geom_point() +
#' #'   coord_cartesian(clip = "off")
#' #'
#' #' # Replace the bottom axis title
#' #' p + labs(x = NULL) +
#' #'   annotate_axis_title(position = "bottom", label = "Weight (1000 lbs)")
#' #'
#' #' # Suppress the native title and annotate instead
#' #' p + annotate_axis_title(
#' #'   position = "left",
#' #'   label    = "Miles per gallon",
#' #'   elements_to = "blank"
#' #' )
#' #'
#' #' # Custom offset when tick labels are large
#' #' p + annotate_axis_title(
#' #'   position = "bottom",
#' #'   label    = "Weight (1000 lbs)",
#' #'   offset   = grid::unit(45, "pt")
#' #' )
#' annotate_axis_title <- function(
    #'     ...,
#'     position,
#'     label,
#'     colour    = NULL,
#'     size      = NULL,
#'     family    = NULL,
#'     angle     = NULL,
#'     hjust     = NULL,
#'     vjust     = NULL,
#'     offset    = NULL,
#'     elements_to = "keep"
#' ) {
#'   rlang::check_dots_empty()
#'
#'   position    <- rlang::arg_match(position, c("top", "bottom", "left", "right"))
#'   elements_to <- rlang::arg_match(elements_to, c("keep", "transparent", "blank"))
#'
#'   axis <- if (position %in% c("top", "bottom")) "x" else "y"
#'
#'   current_theme <- ggplot2::theme_get()
#'
#'   # ---- Resolve title text element -------------------------------------------
#'
#'   title_hierarchy <- c(
#'     paste0("axis.title.", axis, ".", position),
#'     paste0("axis.title.", axis),
#'     "axis.title",
#'     "title",
#'     "text"
#'   )
#'
#'   resolved_title_element <- NULL
#'   for (nm in title_hierarchy) {
#'     el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
#'     if (!is.null(el) && !inherits(el, "element_blank")) {
#'       resolved_title_element <- el
#'       break
#'     }
#'   }
#'   if (is.null(resolved_title_element)) {
#'     resolved_title_element <- ggplot2::element_text(colour = "black", size = 11, family = "")
#'   }
#'
#'   text_colour <- colour %||% resolved_title_element$colour %||% "black"
#'   text_size   <- size   %||% resolved_title_element$size   %||% 11
#'   text_family <- family %||% resolved_title_element$family %||% ""
#'
#'   if (is.null(angle)) {
#'     angle <- resolved_title_element$angle %||% switch(
#'       position, bottom = 0, top = 0, left = 90, right = -90
#'     )
#'   }
#'
#'   # ---- Auto-calculate offset ------------------------------------------------
#'   # offset = tick_length + axis_text_margin + axis_text_size + title_margin
#'   # This mirrors ggplot2's own layout: each component stacks outward from the panel.
#'
#'   if (is.null(offset)) {
#'
#'     # 1. Tick length
#'     tick_length_hierarchy <- c(
#'       paste0("axis.ticks.length.", axis, ".", position),
#'       paste0("axis.ticks.length.", axis),
#'       "axis.ticks.length"
#'     )
#'     resolved_tick_length <- NULL
#'     for (nm in tick_length_hierarchy) {
#'       el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
#'       if (!is.null(el) && !inherits(el, "element_blank")) {
#'         resolved_tick_length <- el
#'         break
#'       }
#'     }
#'     if (is.null(resolved_tick_length)) {
#'       tick_length_pts <- 0.5 * (current_theme$text$size %||% 11)
#'     } else if (inherits(resolved_tick_length, "rel")) {
#'       spacing_pts <- as.numeric(grid::convertUnit(
#'         current_theme$spacing %||% grid::unit(5.5, "pt"), "pt"
#'       ))
#'       tick_length_pts <- as.numeric(resolved_tick_length) * spacing_pts
#'     } else if (inherits(resolved_tick_length, "unit")) {
#'       tick_length_pts <- as.numeric(grid::convertUnit(resolved_tick_length, "pt"))
#'     } else {
#'       tick_length_pts <- as.numeric(resolved_tick_length)
#'     }
#'
#'     # 2. Axis text element (size + inner margin)
#'     text_hierarchy <- c(
#'       paste0("axis.text.", axis, ".", position),
#'       paste0("axis.text.", axis),
#'       "axis.text"
#'     )
#'     resolved_text_element <- NULL
#'     for (nm in text_hierarchy) {
#'       el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
#'       if (!is.null(el) && !inherits(el, "element_blank")) {
#'         resolved_text_element <- el
#'         break
#'       }
#'     }
#'
#'     axis_text_size_pts <- if (!is.null(resolved_text_element$size)) {
#'       if (inherits(resolved_text_element$size, "rel")) {
#'         as.numeric(resolved_text_element$size) * (current_theme$text$size %||% 11)
#'       } else {
#'         resolved_text_element$size
#'       }
#'     } else {
#'       current_theme$text$size %||% 11
#'     }
#'
#'     # Inner margin = the side of the axis text that faces outward toward the title
#'     inner_margin_index <- switch(position, bottom = 1L, top = 3L, left = 2L, right = 4L)
#'     axis_text_margin_pts <- 0
#'     if (!is.null(resolved_text_element$margin)) {
#'       m <- resolved_text_element$margin
#'       if (inherits(m, c("margin", "unit")) && length(m) >= inner_margin_index) {
#'         axis_text_margin_pts <- as.numeric(
#'           grid::convertUnit(m[inner_margin_index], "pt")
#'         )
#'       }
#'     }
#'
#'     # 3. Title inner margin = the side facing inward toward the axis text
#'     title_margin_pts <- 0
#'     if (!is.null(resolved_title_element$margin)) {
#'       m <- resolved_title_element$margin
#'       if (inherits(m, c("margin", "unit")) && length(m) >= inner_margin_index) {
#'         title_margin_pts <- as.numeric(
#'           grid::convertUnit(m[inner_margin_index], "pt")
#'         )
#'       }
#'     }
#'
#'     offset_pts <- tick_length_pts + axis_text_margin_pts + axis_text_size_pts + title_margin_pts
#'     offset <- grid::unit(offset_pts, "pt")
#'
#'   } else if (!inherits(offset, "unit")) {
#'     offset <- grid::unit(offset, "pt")
#'   }
#'
#'   # ---- Justification --------------------------------------------------------
#'   # vjust controls which edge of the text anchors to the offset point.
#'   # In annotation_custom context this has a different geometric meaning than
#'   # in a constrained gtable cell, so we derive from position rather than
#'   # inheriting from the theme:
#'   #   bottom: vjust=1 → top edge of text anchors at offset → label hangs outward
#'   #   top:    vjust=0 → bottom edge anchors at offset → label sits above
#'   #   left/right: vjust=0.5 → text centered at offset point (correct for rot=90/-90)
#'
#'   if (is.null(hjust)) hjust <- 0.5
#'   if (is.null(vjust)) vjust <- switch(position, bottom = 1, top = 0, left = 0.5, right = 0.5)
#'
#'   # ---- Build text grob ------------------------------------------------------
#'
#'   gp <- grid::gpar(col = text_colour, fontsize = text_size, fontfamily = text_family)
#'
#'   text_grob <- switch(
#'     position,
#'     bottom = grid::textGrob(
#'       label,
#'       x    = grid::unit(0.5, "npc"),
#'       y    = grid::unit(0, "npc") - offset,
#'       just = c(hjust, vjust),
#'       rot  = angle,
#'       gp   = gp
#'     ),
#'     top = grid::textGrob(
#'       label,
#'       x    = grid::unit(0.5, "npc"),
#'       y    = grid::unit(1, "npc") + offset,
#'       just = c(hjust, vjust),
#'       rot  = angle,
#'       gp   = gp
#'     ),
#'     left = grid::textGrob(
#'       label,
#'       x    = grid::unit(0, "npc") - offset,
#'       y    = grid::unit(0.5, "npc"),
#'       just = c(hjust, vjust),
#'       rot  = angle,
#'       gp   = gp
#'     ),
#'     right = grid::textGrob(
#'       label,
#'       x    = grid::unit(1, "npc") + offset,
#'       y    = grid::unit(0.5, "npc"),
#'       just = c(hjust, vjust),
#'       rot  = angle,
#'       gp   = gp
#'     )
#'   )
#'
#'   stamp <- list(ggplot2::annotation_custom(
#'     text_grob,
#'     xmin = -Inf, xmax = Inf,
#'     ymin = -Inf, ymax = Inf
#'   ))
#'
#'   # ---- Theme modification ---------------------------------------------------
#'
#'   if (elements_to != "keep") {
#'     theme_name <- paste0("axis.title.", axis, ".", position)
#'     theme_mod  <- list()
#'     theme_mod[[theme_name]] <- if (elements_to == "transparent") {
#'       ggplot2::element_text(colour = "transparent")
#'     } else {
#'       ggplot2::element_blank()
#'     }
#'     stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
#'   }
#'
#'   return(stamp)
#' }
