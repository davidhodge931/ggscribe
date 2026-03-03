#' Annotate panel grid segments
#'
#' @description Create annotated segments of the panel grid.
#'
#' This function is designed to work with a theme that is globally set.
#'
#' @param ... Arguments passed to `ggplot2::annotate("segment", ....)` (if normalised coordinates not used). Require named arguments (and support trailing commas).
#' @param x A vector of x-axis breaks for vertical grid lines. Cannot be used together with `y`. Use `I()` to specify normalized coordinates (0-1).
#' @param y A vector of y-axis breaks for horizontal grid lines. Cannot be used together with `x`. Use `I()` to specify normalized coordinates (0-1).
#' @param xmin,xmax The starting and ending x positions for horizontal grid lines. Use `I()` for normalized coordinates (0-1). Defaults to `-Inf` and `Inf`.
#' @param ymin,ymax The starting and ending y positions for vertical grid lines. Use `I()` for normalized coordinates (0-1). Defaults to `-Inf` and `Inf`.
#' @param minor Logical. If `FALSE` (default), creates major grid lines. If `TRUE`, creates minor grid lines.
#' @param colour The colour of grid lines. Inherits from current theme `panel.grid.major` or `panel.grid.minor` etc.
#' @param linewidth A number. Inherits from current theme `panel.grid.major` or `panel.grid.minor` etc.
#' @param linetype An integer. Inherits from current theme `panel.grid.major` or `panel.grid.minor` etc.
#' @param theme What to do with the equivalent theme elements. Either `"keep"`, `"transparent"`, or `"blank"`. Defaults `"keep"`.
#'
#' @return A list of annotate annotates and theme elements.
#' @export
scribe_panel_grid <- function(
    ...,
    x = NULL,
    y = NULL,
    xmin = NULL,
    xmax = NULL,
    ymin = NULL,
    ymax = NULL,
    minor = FALSE,
    colour = NULL,
    linewidth = NULL,
    linetype = NULL,
    theme = "keep"
) {
  # Validate arguments
  if (is.null(x) && is.null(y)) {
    rlang::abort("Either x or y must be specified")
  }
  
  if (!is.null(x) && !is.null(y)) {
    rlang::abort("Only one of x or y can be specified")
  }
  
  if (!theme %in% c("transparent", "keep", "blank")) {
    rlang::abort(
      "theme must be one of 'transparent', 'keep', or 'blank'"
    )
  }
  
  # Check if values are wrapped in I() to determine coordinate type
  x_is_normalized <- !is.null(x) && inherits(x, "AsIs")
  y_is_normalized <- !is.null(y) && inherits(y, "AsIs")
  xmin_is_normalized <- !is.null(xmin) && inherits(xmin, "AsIs")
  xmax_is_normalized <- !is.null(xmax) && inherits(xmax, "AsIs")
  ymin_is_normalized <- !is.null(ymin) && inherits(ymin, "AsIs")
  ymax_is_normalized <- !is.null(ymax) && inherits(ymax, "AsIs")
  
  # Unwrap I() values
  if (x_is_normalized) {
    x <- unclass(x)
    if (any(x < 0 | x > 1)) {
      rlang::abort("Normalized x coordinates (specified with I()) must be between 0 and 1")
    }
  }
  if (y_is_normalized) {
    y <- unclass(y)
    if (any(y < 0 | y > 1)) {
      rlang::abort("Normalized y coordinates (specified with I()) must be between 0 and 1")
    }
  }
  if (xmin_is_normalized) {
    xmin <- unclass(xmin)
    if (xmin < 0 || xmin > 1) {
      rlang::abort("Normalized xmin (specified with I()) must be between 0 and 1")
    }
  }
  if (xmax_is_normalized) {
    xmax <- unclass(xmax)
    if (xmax < 0 || xmax > 1) {
      rlang::abort("Normalized xmax (specified with I()) must be between 0 and 1")
    }
  }
  if (ymin_is_normalized) {
    ymin <- unclass(ymin)
    if (ymin < 0 || ymin > 1) {
      rlang::abort("Normalized ymin (specified with I()) must be between 0 and 1")
    }
  }
  if (ymax_is_normalized) {
    ymax <- unclass(ymax)
    if (ymax < 0 || ymax > 1) {
      rlang::abort("Normalized ymax (specified with I()) must be between 0 and 1")
    }
  }
  
  # Determine axis from x/y
  axis <- if (!is.null(x)) "x" else "y"
  
  # Determine coordinate systems for breaks and limits separately
  breaks_normalized <- if (axis == "x") x_is_normalized else y_is_normalized
  limits_normalized <- if (axis == "x") {
    ymin_is_normalized || ymax_is_normalized
  } else {
    xmin_is_normalized || xmax_is_normalized
  }
  
  # Get breaks
  breaks <- if (!is.null(x)) x else y
  
  # Check for empty breaks
  if (length(breaks) == 0) {
    return(list())
  }
  
  # Get current theme
  current_theme <- ggplot2::theme_get()
  
  # Build hierarchy for panel grid based on whether minor or major
  if (minor) {
    # For minor grid
    grid_minor_specific <- paste0("panel.grid.minor.", axis)
    grid_minor <- "panel.grid.minor"
    grid_general <- "panel.grid"
    
    grid_hierarchy <- c(
      grid_minor_specific,
      grid_minor,
      grid_general
    )
  } else {
    # For major grid
    grid_major_specific <- paste0("panel.grid.major.", axis)
    grid_major <- "panel.grid.major"
    grid_general <- "panel.grid"
    
    grid_hierarchy <- c(
      grid_major_specific,
      grid_major,
      grid_general
    )
  }
  
  # Find the first non-blank resolved grid element
  resolved_grid_element <- grid_hierarchy |>
    purrr::map(\(x) ggplot2::calc_element(x, current_theme, skip_blank = TRUE)) |>
    purrr::detect(\(x) !is.null(x) && !inherits(x, "element_blank"))
  
  # If still no element found, create a minimal fallback
  if (is.null(resolved_grid_element)) {
    if (minor) {
      # Lighter defaults for minor grid
      resolved_grid_element <- list(
        colour = "grey95",
        linewidth = 0.25,
        linetype = 1
      )
    } else {
      # Standard defaults for major grid
      resolved_grid_element <- list(
        colour = "grey90",
        linewidth = 0.5,
        linetype = 1
      )
    }
  }
  
  # Extract theme properties with proper resolution
  grid_colour <- colour %||% resolved_grid_element$colour %||%
    (if (minor) "grey95" else "grey90")
  
  # Handle linewidth with proper rel() support
  if (is.null(linewidth)) {
    grid_linewidth <- resolved_grid_element$linewidth %||%
      (if (minor) 0.25 else 0.5)
  } else {
    if (inherits(linewidth, "rel")) {
      # Apply user's rel() to the resolved theme linewidth
      base_linewidth <- resolved_grid_element$linewidth %||%
        (if (minor) 0.25 else 0.5)
      grid_linewidth <- as.numeric(linewidth) * base_linewidth
    } else {
      grid_linewidth <- linewidth
    }
  }
  
  grid_linetype <- linetype %||% resolved_grid_element$linetype %||% 1
  
  stamp <- list()
  
  # Add theme modification if requested
  if (theme != "keep") {
    # Determine which theme element to modify based on minor flag
    if (minor) {
      element_name <- paste0("panel.grid.minor.", axis)
    } else {
      element_name <- paste0("panel.grid.major.", axis)
    }
    
    if (theme == "transparent") {
      stamp <- c(
        stamp,
        list(
          ggplot2::theme(
            !!element_name := ggplot2::element_line(colour = "transparent")
          )
        )
      )
    } else if (theme == "blank") {
      stamp <- c(
        stamp,
        list(
          ggplot2::theme(
            !!element_name := ggplot2::element_blank()
          )
        )
      )
    }
  }
  
  # Create grid lines based on coordinate type combinations
  if (breaks_normalized && limits_normalized) {
    # Both breaks and limits are normalized - use grobs with npc units
    grid_annotations <- breaks |>
      purrr::map(\(break_val) {
        if (axis == "x") {
          # Vertical grid line at normalized x position
          y_start <- if (!is.null(ymin)) ymin else 0
          y_end <- if (!is.null(ymax)) ymax else 1
          
          grid_grob <- grid::linesGrob(
            x = grid::unit(c(break_val, break_val), "npc"),
            y = grid::unit(c(y_start, y_end), "npc"),
            gp = grid::gpar(
              col = grid_colour,
              lwd = grid_linewidth * 72 / 25.4,
              lty = grid_linetype,
              lineend = "butt"
            )
          )
        } else {  # y axis
          # Horizontal grid line at normalized y position
          x_start <- if (!is.null(xmin)) xmin else 0
          x_end <- if (!is.null(xmax)) xmax else 1
          
          grid_grob <- grid::linesGrob(
            x = grid::unit(c(x_start, x_end), "npc"),
            y = grid::unit(c(break_val, break_val), "npc"),
            gp = grid::gpar(
              col = grid_colour,
              lwd = grid_linewidth * 72 / 25.4,
              lty = grid_linetype,
              lineend = "butt"
            )
          )
        }
        
        ggplot2::annotation_custom(
          grob = grid_grob,
          xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
        )
      })
    
    stamp <- c(stamp, grid_annotations)
    
  } else if (!breaks_normalized && limits_normalized) {
    # Breaks in data coordinates, limits in normalized - need grobs
    grid_annotations <- breaks |>
      purrr::map(\(break_val) {
        if (axis == "x") {
          # Vertical grid line at data x position with normalized y limits
          y_start <- if (!is.null(ymin)) ymin else 0
          y_end <- if (!is.null(ymax)) ymax else 1
          
          grid_grob <- grid::linesGrob(
            x = grid::unit(c(0.5, 0.5), "npc"),
            y = grid::unit(c(y_start, y_end), "npc"),
            gp = grid::gpar(
              col = grid_colour,
              lwd = grid_linewidth * 72 / 25.4,
              lty = grid_linetype,
              lineend = "butt"
            )
          )
          
          ggplot2::annotation_custom(
            grob = grid_grob,
            xmin = break_val, xmax = break_val, ymin = -Inf, ymax = Inf
          )
        } else {  # y axis
          # Horizontal grid line at data y position with normalized x limits
          x_start <- if (!is.null(xmin)) xmin else 0
          x_end <- if (!is.null(xmax)) xmax else 1
          
          grid_grob <- grid::linesGrob(
            x = grid::unit(c(x_start, x_end), "npc"),
            y = grid::unit(c(0.5, 0.5), "npc"),
            gp = grid::gpar(
              col = grid_colour,
              lwd = grid_linewidth * 72 / 25.4,
              lty = grid_linetype,
              lineend = "butt"
            )
          )
          
          ggplot2::annotation_custom(
            grob = grid_grob,
            xmin = -Inf, xmax = Inf, ymin = break_val, ymax = break_val
          )
        }
      })
    
    stamp <- c(stamp, grid_annotations)
    
  } else if (breaks_normalized && !limits_normalized) {
    # Breaks in normalized, limits in data coordinates - use grobs
    # This case needs grobs positioned across full plot with data limits ignored
    grid_annotations <- breaks |>
      purrr::map(\(break_val) {
        if (axis == "x") {
          # Vertical grid line at normalized x position
          grid_grob <- grid::linesGrob(
            x = grid::unit(c(break_val, break_val), "npc"),
            y = grid::unit(c(0, 1), "npc"),
            gp = grid::gpar(
              col = grid_colour,
              lwd = grid_linewidth * 72 / 25.4,
              lty = grid_linetype,
              lineend = "butt"
            )
          )
        } else {  # y axis
          # Horizontal grid line at normalized y position
          grid_grob <- grid::linesGrob(
            x = grid::unit(c(0, 1), "npc"),
            y = grid::unit(c(break_val, break_val), "npc"),
            gp = grid::gpar(
              col = grid_colour,
              lwd = grid_linewidth * 72 / 25.4,
              lty = grid_linetype,
              lineend = "butt"
            )
          )
        }
        
        ggplot2::annotation_custom(
          grob = grid_grob,
          xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
        )
      })
    
    stamp <- c(stamp, grid_annotations)
    
  } else {
    # Both in data coordinates - use regular annotate
    if (axis == "x") {
      # Add vertical grid lines
      # Use provided ymin/ymax or default to -Inf/Inf
      y_start <- if (!is.null(ymin)) ymin else -Inf
      y_end <- if (!is.null(ymax)) ymax else Inf
      
      stamp <- c(
        stamp,
        list(
          ggplot2::annotate(
            "segment",
            x = breaks,
            xend = breaks,
            y = y_start,
            yend = y_end,
            colour = grid_colour,
            linewidth = grid_linewidth,
            linetype = grid_linetype,
            ...
          )
        )
      )
    } else {  # y axis
      # Add horizontal grid lines
      # Use provided xmin/xmax or default to -Inf/Inf
      x_start <- if (!is.null(xmin)) xmin else -Inf
      x_end <- if (!is.null(xmax)) xmax else Inf
      
      stamp <- c(
        stamp,
        list(
          ggplot2::annotate(
            "segment",
            x = x_start,
            xend = x_end,
            y = breaks,
            yend = breaks,
            colour = grid_colour,
            linewidth = grid_linewidth,
            linetype = grid_linetype,
            ...
          )
        )
      )
    }
  }
  
  return(stamp)
}

