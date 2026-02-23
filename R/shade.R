#' Annotate the panel background
#'
#' @description Annotate a filled rectangle on the panel background.
#
#' It is designed to work with a theme that is globally set.
#'
#' @param ... Arguments passed to `ggplot2::annotate("rect", ....)` (if normalised coordinates not used). Require named arguments (and support trailing commas).
#' @param xmin A value of length 1. Defaults to `-Inf`. Use `I()` to specify normalized coordinates (0-1).
#' @param xmax A value of length 1. Defaults to `Inf`. Use `I()` to specify normalized coordinates (0-1).
#' @param ymin A value of length 1. Defaults to `-Inf`. Use `I()` to specify normalized coordinates (0-1).
#' @param ymax A value of length 1. Defaults to `Inf`. Use `I()` to specify normalized coordinates (0-1).
#' @param fill The fill color to blend with the panel background. Defaults to `"#8991A1FF"`. The final rectangle color is created by blending this fill with the current panel background: screen blend for dark backgrounds, multiply blend for light backgrounds.
#' @param alpha The transparency of the rectangle. Defaults to `0.2` (subtle overlay).
#' @param colour The border colour of the rectangle. Defaults to `"transparent"`.
#' @param linewidth A number. Inherits from the current theme `panel.border` linewidth. Supports `rel()` for relative sizing.
#' @param linetype An integer. Defaults to `1`.
#'
#' @return A list containing an annotation annotate.
#' @export
#'
scribe_panel_shade <- function(
    ...,
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    fill = NULL,
    alpha = 0.25,
    colour = "transparent",
    linewidth = NULL,
    linetype = NULL
) {
  
  if (rlang::is_null(fill)) {
    fill <- if (is_panel_dark()) {
      flexoki::flexoki$base["base600"]
    }
    else {
      flexoki::flexoki$base["base400"]
    }
  }
  
  # Check if coordinates are wrapped in I() for normalized positioning
  xmin_is_normalized <- inherits(xmin, "AsIs")
  xmax_is_normalized <- inherits(xmax, "AsIs")
  ymin_is_normalized <- inherits(ymin, "AsIs")
  ymax_is_normalized <- inherits(ymax, "AsIs")
  
  # Check for mixing of coordinate types
  x_uses_normalized <- xmin_is_normalized || xmax_is_normalized
  y_uses_normalized <- ymin_is_normalized || ymax_is_normalized
  
  # If using normalized, both min and max must be normalized or Inf
  if (x_uses_normalized) {
    if ((xmin_is_normalized || is.infinite(xmin)) && (xmax_is_normalized || is.infinite(xmax))) {
      # Valid - both are normalized or Inf
    } else {
      rlang::abort("Cannot mix normalized (I()) and data coordinates for x. Use I() for both xmin and xmax, or neither.")
    }
  }
  
  if (y_uses_normalized) {
    if ((ymin_is_normalized || is.infinite(ymin)) && (ymax_is_normalized || is.infinite(ymax))) {
      # Valid - both are normalized or Inf
    } else {
      rlang::abort("Cannot mix normalized (I()) and data coordinates for y. Use I() for both ymin and ymax, or neither.")
    }
  }
  
  # Unwrap and validate I() values
  if (xmin_is_normalized) {
    xmin <- unclass(xmin)
    if (length(xmin) != 1 || xmin < 0 || xmin > 1) {
      rlang::abort("Normalized xmin (specified with I()) must be a single value between 0 and 1")
    }
  }
  if (xmax_is_normalized) {
    xmax <- unclass(xmax)
    if (length(xmax) != 1 || xmax < 0 || xmax > 1) {
      rlang::abort("Normalized xmax (specified with I()) must be a single value between 0 and 1")
    }
  }
  if (ymin_is_normalized) {
    ymin <- unclass(ymin)
    if (length(ymin) != 1 || ymin < 0 || ymin > 1) {
      rlang::abort("Normalized ymin (specified with I()) must be a single value between 0 and 1")
    }
  }
  if (ymax_is_normalized) {
    ymax <- unclass(ymax)
    if (length(ymax) != 1 || ymax < 0 || ymax > 1) {
      rlang::abort("Normalized ymax (specified with I()) must be a single value between 0 and 1")
    }
  }
  
  # Determine if we need to use grob approach (if any coordinate is normalized)
  use_grob <- x_uses_normalized || y_uses_normalized
  
  # Get theme for linewidth default
  current_theme <- ggplot2::theme_get()
  panel_border <- ggplot2::calc_element("panel.border", current_theme, skip_blank = TRUE)
  panel_border_linewidth <- if (!rlang::is_null(panel_border) && !inherits(panel_border, "element_blank")) {
    panel_border$linewidth %||% 0.5
  } else {
    0.5  # fallback
  }
  
  # Set remaining defaults
  alpha <- alpha %||% 1
  
  # Handle linewidth with proper rel() support
  if (rlang::is_null(linewidth)) {
    linewidth <- panel_border_linewidth
  } else if (inherits(linewidth, "rel")) {
    linewidth <- as.numeric(linewidth) * panel_border_linewidth
  }
  
  linetype <- linetype %||% 1
  
  # Create rectangle based on coordinate type
  if (use_grob) {
    # For normalized coordinates, create a grob
    # Convert coordinates to npc units
    x_left <- if (xmin_is_normalized) {
      grid::unit(xmin, "npc")
    } else {
      grid::unit(0, "npc")  # -Inf defaults to 0
    }
    
    x_right <- if (xmax_is_normalized) {
      grid::unit(xmax, "npc")
    } else {
      grid::unit(1, "npc")  # Inf defaults to 1
    }
    
    y_bottom <- if (ymin_is_normalized) {
      grid::unit(ymin, "npc")
    } else {
      grid::unit(0, "npc")  # -Inf defaults to 0
    }
    
    y_top <- if (ymax_is_normalized) {
      grid::unit(ymax, "npc")
    } else {
      grid::unit(1, "npc")  # Inf defaults to 1
    }
    
    # Create rectangle grob
    rect_grob <- grid::rectGrob(
      x = x_left,
      y = y_bottom,
      width = x_right - x_left,
      height = y_top - y_bottom,
      just = c("left", "bottom"),
      gp = grid::gpar(
        fill = scales::alpha(fill, alpha),
        col = colour,
        lwd = linewidth * 72 / 25.4,
        lty = linetype
      )
    )
    
    stamp <- list(
      ggplot2::annotation_custom(
        grob = rect_grob,
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
      )
    )
  } else {
    # Original behavior for data coordinates
    stamp <- list(
      ggplot2::annotate(
        geom = "rect",
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        fill = fill,
        colour = colour,
        linewidth = linewidth,
        linetype = linetype,
        alpha = alpha,
        ...
      )
    )
  }
  
  return(stamp)
}
