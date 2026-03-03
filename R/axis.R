#' Annotate axis line segment
#'
#' @description Create an annotated segment of the axis line.
#'
#' This function is designed to work with a theme that is globally set.
#'
#' It should be used with a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' Note that this function does not support plots where either positional scale is of date or datetime class. Use [ggplot2::geom_segment], [ggplot2::geom_hline] or [ggplot2::geom_vline] instead.
#'
#' @param ... Arguments passed to `ggplot2::annotate("segment", ....)` (if normalised coordinates not used). Require named arguments (and support trailing commas).
#' @param position The position of the axis line. One of `"top"`, `"bottom"`, `"left"`, or `"right"`. Ignored if `x` or `y` is provided.
#' @param x A single x-axis value for a vertical line. Cannot be used together with `y` or `xmin`/`xmax`. Use `I()` for normalized coordinates (0-1).
#' @param y A single y-axis value for a horizontal line. Cannot be used together with `x` or `ymin`/`ymax`. Use `I()` for normalized coordinates (0-1).
#' @param xmin The starting x position for a horizontal line segment. Use `I()` for normalized coordinates (0-1).
#' @param xmax The ending x position for a horizontal line segment. Use `I()` for normalized coordinates (0-1).
#' @param ymin The starting y position for a vertical line segment. Use `I()` for normalized coordinates (0-1).
#' @param ymax The ending y position for a vertical line segment. Use `I()` for normalized coordinates (0-1).
#' @param colour The colour of the annotated segment. Inherits from the current theme axis.line etc.
#' @param linewidth A number. Inherits from the current theme axis.line etc.
#' @param linetype An integer. Inherits from the current theme axis.line etc.
#' @param theme How to modify the corresponding theme element. One of `"keep"`, `"transparent"`, or `"blank"`.
#'   Defaults to `"keep"`.
#'
#' @return A list of annotation annotates and theme elements.
#' @export
#'
scribe_axis_line <- function(
    ...,
    position = NULL,
    x = NULL,
    y = NULL,
    xmin = NULL,
    xmax = NULL,
    ymin = NULL,
    ymax = NULL,
    colour = NULL,
    linewidth = NULL,
    linetype = NULL,
    theme = "keep"
) {
  # Validate arguments - can't have both x and y
  if (!is.null(x) && !is.null(y)) {
    stop("Cannot specify both x and y. Use either x for a vertical line or y for a horizontal line.")
  }
  
  # Can't mix x with xmin/xmax or y with ymin/ymax
  if (!is.null(x) && (!is.null(xmin) || !is.null(xmax))) {
    stop("Cannot specify both x and xmin/xmax. Use either x for a single position or xmin/xmax for endpoints.")
  }
  if (!is.null(y) && (!is.null(ymin) || !is.null(ymax))) {
    stop("Cannot specify both y and ymin/ymax. Use either y for a single position or ymin/ymax for endpoints.")
  }
  
  # If x or y is provided, it overrides position
  use_xy_positioning <- !is.null(x) || !is.null(y)
  
  if (use_xy_positioning) {
    # Check if using normalized coordinates
    x_is_normalized <- !is.null(x) && inherits(x, "AsIs")
    y_is_normalized <- !is.null(y) && inherits(y, "AsIs")
    xmin_is_normalized <- !is.null(xmin) && inherits(xmin, "AsIs")
    xmax_is_normalized <- !is.null(xmax) && inherits(xmax, "AsIs")
    ymin_is_normalized <- !is.null(ymin) && inherits(ymin, "AsIs")
    ymax_is_normalized <- !is.null(ymax) && inherits(ymax, "AsIs")
    
    # Unwrap and validate I() values
    if (x_is_normalized) {
      x <- unclass(x)
      if (length(x) != 1 || x < 0 || x > 1) {
        stop("Normalized x (specified with I()) must be a single value between 0 and 1")
      }
    } else if (!is.null(x) && length(x) != 1) {
      stop("x must be a single value")
    }
    
    if (y_is_normalized) {
      y <- unclass(y)
      if (length(y) != 1 || y < 0 || y > 1) {
        stop("Normalized y (specified with I()) must be a single value between 0 and 1")
      }
    } else if (!is.null(y) && length(y) != 1) {
      stop("y must be a single value")
    }
    
    if (xmin_is_normalized) {
      xmin <- unclass(xmin)
      if (length(xmin) != 1 || xmin < 0 || xmin > 1) {
        stop("Normalized xmin (specified with I()) must be a single value between 0 and 1")
      }
    }
    if (xmax_is_normalized) {
      xmax <- unclass(xmax)
      if (length(xmax) != 1 || xmax < 0 || xmax > 1) {
        stop("Normalized xmax (specified with I()) must be a single value between 0 and 1")
      }
    }
    if (ymin_is_normalized) {
      ymin <- unclass(ymin)
      if (length(ymin) != 1 || ymin < 0 || ymin > 1) {
        stop("Normalized ymin (specified with I()) must be a single value between 0 and 1")
      }
    }
    if (ymax_is_normalized) {
      ymax <- unclass(ymax)
      if (length(ymax) != 1 || ymax < 0 || ymax > 1) {
        stop("Normalized ymax (specified with I()) must be a single value between 0 and 1")
      }
    }
    
    # Determine axis from x/y
    axis <- if (!is.null(x)) "y" else "x"  # Note: vertical line is on y axis, horizontal on x axis
    
    # Determine if we're using normalized coordinates based on ANY normalized input
    use_normalized <- x_is_normalized || y_is_normalized || xmin_is_normalized || xmax_is_normalized ||
      ymin_is_normalized || ymax_is_normalized
  } else {
    # Original position-based behavior
    if (is.null(position)) {
      stop("Must specify either position, x, or y")
    }
    
    position <- rlang::arg_match(position, c("top", "bottom", "left", "right"))
    
    # Determine axis from position
    axis <- if (position %in% c("top", "bottom")) "x" else "y"
    use_normalized <- FALSE
  }
  
  theme <- rlang::arg_match(theme, c("keep", "transparent", "blank"))
  
  # Get current theme and calculate resolved element properties
  current_theme <- ggplot2::theme_get()
  
  # Build hierarchy of element names from most specific to least specific
  if (use_xy_positioning) {
    element_hierarchy <- c(
      paste0("axis.line.", axis),
      "axis.line"
    )
  } else {
    specific_element <- paste0("axis.line.", axis, ".", position)
    axis_element <- paste0("axis.line.", axis)
    general_element <- "axis.line"
    element_hierarchy <- c(specific_element, axis_element, general_element)
  }
  
  # Find the first non-blank resolved element
  resolved_element <- element_hierarchy |>
    purrr::map(\(x) ggplot2::calc_element(x, current_theme, skip_blank = TRUE)) |>
    purrr::detect(\(x) !is.null(x) && !inherits(x, "element_blank"))
  
  # If still no element found, create a minimal fallback
  if (is.null(resolved_element)) {
    resolved_element <- list(colour = "black", linewidth = 0.5, linetype = 1)
  }
  
  # Extract theme properties with proper resolution
  line_colour <- if (is.null(colour)) {
    resolved_element$colour %||% "black"
  } else {
    colour
  }
  
  # Handle linewidth with proper rel() support
  if (is.null(linewidth)) {
    line_linewidth <- resolved_element$linewidth %||% 0.5
  } else {
    if (inherits(linewidth, "rel")) {
      base_linewidth <- resolved_element$linewidth %||% 0.5
      line_linewidth <- as.numeric(linewidth) * base_linewidth
    } else {
      line_linewidth <- linewidth
    }
  }
  
  # Extract linetype with proper resolution
  line_linetype <- if (is.null(linetype)) {
    resolved_element$linetype %||% 1
  } else {
    linetype
  }
  
  stamp <- list()
  
  # Create axis segment based on positioning method
  if (use_xy_positioning) {
    if (!is.null(x)) {
      # Vertical line
      # Set defaults for endpoints based on whether we're using normalized coordinates
      if (is.null(ymin)) {
        ymin <- if (use_normalized) 0 else -Inf
      }
      if (is.null(ymax)) {
        ymax <- if (use_normalized) 1 else Inf
      }
      
      if (use_normalized) {
        # Create normalized grob
        line_grob <- grid::linesGrob(
          x = grid::unit(c(x, x), "npc"),
          y = grid::unit(c(ymin, ymax), "npc"),
          gp = grid::gpar(
            col = line_colour,
            lwd = line_linewidth * 72 / 25.4,
            lty = line_linetype,
            lineend = "butt"
          )
        )
        stamp <- c(
          stamp,
          list(
            ggplot2::annotation_custom(
              grob = line_grob,
              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
            )
          )
        )
      } else {
        # Data coordinates
        stamp <- c(
          stamp,
          list(
            rlang::exec(
              ggplot2::annotate,
              "segment",
              x = x,
              xend = x,
              y = ymin,
              yend = ymax,
              colour = line_colour,
              linewidth = line_linewidth,
              linetype = line_linetype,
              ...
            )
          )
        )
      }
    } else {
      # Horizontal line
      # Set defaults for endpoints based on whether we're using normalized coordinates
      if (is.null(xmin)) {
        xmin <- if (use_normalized) 0 else -Inf
      }
      if (is.null(xmax)) {
        xmax <- if (use_normalized) 1 else Inf
      }
      
      if (use_normalized) {
        # Create normalized grob
        line_grob <- grid::linesGrob(
          x = grid::unit(c(xmin, xmax), "npc"),
          y = grid::unit(c(y, y), "npc"),
          gp = grid::gpar(
            col = line_colour,
            lwd = line_linewidth * 72 / 25.4,
            lty = line_linetype,
            lineend = "butt"
          )
        )
        stamp <- c(
          stamp,
          list(
            ggplot2::annotation_custom(
              grob = line_grob,
              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
            )
          )
        )
      } else {
        # Data coordinates
        stamp <- c(
          stamp,
          list(
            rlang::exec(
              ggplot2::annotate,
              "segment",
              x = xmin,
              xend = xmax,
              y = y,
              yend = y,
              colour = line_colour,
              linewidth = line_linewidth,
              linetype = line_linetype,
              ...
            )
          )
        )
      }
    }
  } else {
    # Original position-based behavior
    # Use provided min/max values or default to -Inf/Inf
    if (position == "bottom") {
      x_start <- if (!is.null(xmin)) xmin else -Inf
      x_end <- if (!is.null(xmax)) xmax else Inf
      stamp <- c(
        stamp,
        list(
          rlang::exec(
            ggplot2::annotate,
            "segment",
            x = x_start,
            xend = x_end,
            y = -Inf,
            yend = -Inf,
            colour = line_colour,
            linewidth = line_linewidth,
            linetype = line_linetype,
            ...
          )
        )
      )
    } else if (position == "top") {
      x_start <- if (!is.null(xmin)) xmin else -Inf
      x_end <- if (!is.null(xmax)) xmax else Inf
      stamp <- c(
        stamp,
        list(
          rlang::exec(
            ggplot2::annotate,
            "segment",
            x = x_start,
            xend = x_end,
            y = Inf,
            yend = Inf,
            colour = line_colour,
            linewidth = line_linewidth,
            linetype = line_linetype,
            ...
          )
        )
      )
    } else if (position == "left") {
      y_start <- if (!is.null(ymin)) ymin else -Inf
      y_end <- if (!is.null(ymax)) ymax else Inf
      stamp <- c(
        stamp,
        list(
          rlang::exec(
            ggplot2::annotate,
            "segment",
            x = -Inf,
            xend = -Inf,
            y = y_start,
            yend = y_end,
            colour = line_colour,
            linewidth = line_linewidth,
            linetype = line_linetype,
            ...
          )
        )
      )
    } else {
      # right
      y_start <- if (!is.null(ymin)) ymin else -Inf
      y_end <- if (!is.null(ymax)) ymax else Inf
      stamp <- c(
        stamp,
        list(
          rlang::exec(
            ggplot2::annotate,
            "segment",
            x = Inf,
            xend = Inf,
            y = y_start,
            yend = y_end,
            colour = line_colour,
            linewidth = line_linewidth,
            linetype = line_linetype,
            ...
          )
        )
      )
    }
  }
  
  # Add theme modification if requested
  if (theme != "keep") {
    theme_name <- NULL
    
    if (use_xy_positioning) {
      # For x/y positioning, determine which axis line element corresponds to the line
      if (!is.null(x)) {
        # Vertical line - check if it's at left or right edge
        if (x_is_normalized) {
          if (x == 0) {
            theme_name <- "axis.line.y.left"
          } else if (x == 1) {
            theme_name <- "axis.line.y.right"
          }
        } else if (is.infinite(x)) {
          if (x < 0) {
            theme_name <- "axis.line.y.left"
          } else {
            theme_name <- "axis.line.y.right"
          }
        }
      } else if (!is.null(y)) {
        # Horizontal line - check if it's at top or bottom edge
        if (y_is_normalized) {
          if (y == 0) {
            theme_name <- "axis.line.x.bottom"
          } else if (y == 1) {
            theme_name <- "axis.line.x.top"
          }
        } else if (is.infinite(y)) {
          if (y < 0) {
            theme_name <- "axis.line.x.bottom"
          } else {
            theme_name <- "axis.line.x.top"
          }
        }
      }
    } else {
      # Position-based - construct theme name from position
      theme_name <- paste0("axis.line.", axis, ".", position)
    }
    
    # Apply theme modification if we have a theme element to modify
    if (!is.null(theme_name)) {
      theme_mod <- list()
      if (theme == "transparent") {
        theme_mod[[theme_name]] <- ggplot2::element_line(colour = "transparent")
      } else if (theme == "blank") {
        theme_mod[[theme_name]] <- ggplot2::element_blank()
      }
      stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
    }
  }
  
  return(stamp)
}

#' Annotate axis ticks segments
#'
#' @description Create annotated segments of the axis ticks.
#'
#' This function is designed to work with a theme that is globally set.
#'
#' It should be used with a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param position The position of the axis ticks. One of `"top"`, `"bottom"`, `"left"`, or `"right"`.
#' @param x A vector of x-axis breaks for ticks positioning. Use `I()` to specify normalized coordinates (0-1).
#' @param y A vector of y-axis breaks for ticks positioning. Use `I()` to specify normalized coordinates (0-1).
#' @param minor `TRUE` or `FALSE` whether to relate to minor ticks. Defaults `FALSE`.
#' @param colour The colour of the ticks. Inherits from the current theme `axis.ticks` etc.
#' @param linewidth The linewidth of the ticks. Inherits from the current theme `axis.ticks` etc.
#' @param length The total distance from the axis line to the ticks as a grid unit. Use `rel()` to scale relative to default length. Negative values or `rel()` with negative multiplier flip direction.
#' @param theme What to do with the equivalent theme elements. Either `"keep"`, `"transparent"`, or `"blank"`. Defaults `"keep"`.
#'
#' @return A list of annotation annotates and theme elements.
#' @export
#'
scribe_axis_ticks <- function(
    ...,
    position = NULL,
    x = NULL,
    y = NULL,
    minor = FALSE,
    colour = NULL,
    linewidth = NULL,
    length = NULL,
    theme = "keep"
) {
  # Determine position from x/y if not specified
  if (is.null(position)) {
    if (!is.null(x) && !is.null(y)) {
      stop("Cannot specify both x and y. Use either x for top/bottom positions or y for left/right positions.")
    }
    if (!is.null(x)) {
      position <- "bottom"
    } else if (!is.null(y)) {
      position <- "left"
    } else {
      stop("Must specify either position, x, or y")
    }
  }
  
  # Validate position
  position <- rlang::arg_match(position, c("top", "bottom", "left", "right"))
  
  # Check if values are wrapped in I() to determine coordinate type
  x_is_normalized <- !is.null(x) && inherits(x, "AsIs")
  y_is_normalized <- !is.null(y) && inherits(y, "AsIs")
  
  # Unwrap I() values
  if (x_is_normalized) {
    x <- unclass(x)
    if (any(x < 0 | x > 1)) {
      stop("Normalized x coordinates (specified with I()) must be between 0 and 1")
    }
  }
  if (y_is_normalized) {
    y <- unclass(y)
    if (any(y < 0 | y > 1)) {
      stop("Normalized y coordinates (specified with I()) must be between 0 and 1")
    }
  }
  
  # Determine axis from x/y and whether using normalized coordinates
  axis <- if (!is.null(x)) "x" else "y"
  use_normalized <- if (axis == "x") x_is_normalized else y_is_normalized
  
  # Validate x/y based on position
  if (position %in% c("top", "bottom")) {
    if (!is.null(y)) {
      stop("For top or bottom positions, only x can be specified, not y")
    }
    if (is.null(x)) {
      stop("For top or bottom positions, x must be specified")
    }
    use_normalized <- x_is_normalized
  } else {
    if (!is.null(x)) {
      stop("For left or right positions, only y can be specified, not x")
    }
    if (is.null(y)) {
      stop("For left or right positions, y must be specified")
    }
    use_normalized <- y_is_normalized
  }
  
  theme <- rlang::arg_match(theme, c("keep", "transparent", "blank"))
  
  # Determine axis from position
  axis <- if (position %in% c("top", "bottom")) "x" else "y"
  
  # Get breaks
  breaks <- if (!is.null(x)) x else y
  
  # Check for empty breaks
  if (length(breaks) == 0) {
    return(list())
  }
  
  # Get current theme
  current_theme <- ggplot2::theme_get()
  
  # Build hierarchy for axis ticks
  if (minor) {
    tick_minor_specific <- paste0("axis.minor.ticks.", axis, ".", position)
    tick_specific <- paste0("axis.ticks.", axis, ".", position)
    tick_axis <- paste0("axis.ticks.", axis)
    tick_general <- "axis.ticks"
    tick_hierarchy <- c(tick_minor_specific, tick_specific, tick_axis, tick_general)
  } else {
    tick_specific <- paste0("axis.ticks.", axis, ".", position)
    tick_axis <- paste0("axis.ticks.", axis)
    tick_general <- "axis.ticks"
    tick_hierarchy <- c(tick_specific, tick_axis, tick_general)
  }
  
  # Resolve tick properties
  resolved_tick_element <- NULL
  for (element_name in tick_hierarchy) {
    element <- ggplot2::calc_element(element_name, current_theme, skip_blank = TRUE)
    if (!is.null(element) && !inherits(element, "element_blank")) {
      resolved_tick_element <- element
      break
    }
  }
  
  if (is.null(resolved_tick_element)) {
    resolved_tick_element <- list(colour = "black", linewidth = 0.5)
  }
  
  # Build hierarchy for length
  if (minor) {
    length_minor_specific <- paste0("axis.minor.ticks.length.", axis, ".", position)
    length_minor_axis <- paste0("axis.minor.ticks.length.", axis)
    length_minor_general <- "axis.minor.ticks.length"
    length_specific <- paste0("axis.ticks.length.", axis, ".", position)
    length_axis <- paste0("axis.ticks.length.", axis)
    length_general <- "axis.ticks.length"
    length_hierarchy <- c(length_minor_specific, length_minor_axis, length_minor_general,
                          length_specific, length_axis, length_general)
  } else {
    length_specific <- paste0("axis.ticks.length.", axis, ".", position)
    length_axis <- paste0("axis.ticks.length.", axis)
    length_general <- "axis.ticks.length"
    length_hierarchy <- c(length_specific, length_axis, length_general)
  }
  
  # Resolve length
  resolved_length_element <- NULL
  for (element_name in length_hierarchy) {
    element <- ggplot2::calc_element(element_name, current_theme, skip_blank = TRUE)
    if (!is.null(element) && !inherits(element, "element_blank")) {
      resolved_length_element <- element
      break
    }
  }
  
  # Extract theme properties
  tick_colour <- if (is.null(colour)) {
    resolved_tick_element$colour %||% "black"
  } else {
    colour
  }
  
  if (is.null(linewidth)) {
    tick_linewidth <- resolved_tick_element$linewidth %||% 0.5
  } else {
    if (inherits(linewidth, "rel")) {
      base_linewidth <- resolved_tick_element$linewidth %||% 0.5
      tick_linewidth <- as.numeric(linewidth) * base_linewidth
    } else {
      tick_linewidth <- linewidth
    }
  }
  
  # Function to calculate default tick length
  calculate_default_length <- function() {
    if (minor) {
      raw_minor_length <- NULL
      for (element_name in length_hierarchy) {
        if (grepl("minor", element_name)) {
          raw_element <- current_theme[[element_name]]
          if (!is.null(raw_element) && inherits(raw_element, "rel")) {
            raw_minor_length <- raw_element
            break
          }
        }
      }
      
      if (!is.null(raw_minor_length)) {
        major_length <- ggplot2::calc_element("axis.ticks.length", current_theme, skip_blank = TRUE)
        
        if (is.null(major_length)) {
          spacing <- current_theme$spacing %||% grid::unit(5.5, "pt")
          if (inherits(spacing, "unit")) {
            major_tick_length_pts <- as.numeric(grid::convertUnit(spacing, "pt"))
          } else {
            major_tick_length_pts <- 5.5
          }
        } else if (inherits(major_length, "unit")) {
          major_tick_length_pts <- as.numeric(grid::convertUnit(major_length, "pt"))
        } else if (is.numeric(major_length)) {
          major_tick_length_pts <- major_length
        } else {
          major_tick_length_pts <- 5.5
        }
        
        return(grid::unit(as.numeric(raw_minor_length) * major_tick_length_pts, "pt"))
      } else {
        tick_length <- resolved_length_element
        
        if (is.null(tick_length)) {
          text_size <- current_theme$text$size %||% 11
          return(grid::unit(0.375 * text_size, "pt"))
        } else if (!inherits(tick_length, "unit")) {
          if (is.numeric(tick_length)) {
            return(grid::unit(tick_length, "pt"))
          } else {
            text_size <- current_theme$text$size %||% 11
            return(grid::unit(0.375 * text_size, "pt"))
          }
        } else {
          return(tick_length)
        }
      }
    } else {
      tick_length <- resolved_length_element
      
      if (is.null(tick_length)) {
        text_size <- current_theme$text$size %||% 11
        return(grid::unit(0.5 * text_size, "pt"))
      } else if (inherits(tick_length, "rel")) {
        spacing <- current_theme$spacing %||% grid::unit(5.5, "pt")
        if (inherits(spacing, "unit")) {
          spacing_pts <- as.numeric(grid::convertUnit(spacing, "pt"))
        } else {
          spacing_pts <- 5.5
        }
        return(grid::unit(as.numeric(tick_length) * spacing_pts, "pt"))
      } else if (!inherits(tick_length, "unit")) {
        if (is.numeric(tick_length)) {
          return(grid::unit(tick_length, "pt"))
        } else {
          text_size <- current_theme$text$size %||% 11
          return(grid::unit(0.5 * text_size, "pt"))
        }
      } else {
        return(tick_length)
      }
    }
  }
  
  # Initialize flip_direction
  flip_direction <- FALSE
  
  # Handle length
  if (is.null(length)) {
    tick_length <- calculate_default_length()
  } else {
    if (inherits(length, "rel")) {
      default_tick_length <- calculate_default_length()
      rel_value <- as.numeric(length)
      default_pts <- as.numeric(grid::convertUnit(default_tick_length, "pt"))
      tick_length <- grid::unit(abs(rel_value) * default_pts, "pt")
      flip_direction <- rel_value < 0
    } else if (inherits(length, "unit")) {
      tick_length <- length
      flip_direction <- FALSE
    } else if (is.numeric(length)) {
      tick_length <- grid::unit(abs(length), "pt")
      flip_direction <- length < 0
    } else {
      text_size <- current_theme$text$size %||% 11
      if (minor) {
        tick_length <- grid::unit(0.375 * text_size, "pt")
      } else {
        tick_length <- grid::unit(0.5 * text_size, "pt")
      }
      flip_direction <- FALSE
    }
  }
  
  stamp <- list()
  
  # Add theme modification if requested
  if (theme != "keep") {
    if (minor) {
      theme_name <- paste0("axis.minor.ticks.", axis, ".", position)
    } else {
      theme_name <- paste0("axis.ticks.", axis, ".", position)
    }
    
    theme_mod <- list()
    if (theme == "transparent") {
      theme_mod[[theme_name]] <- ggplot2::element_line(colour = "transparent")
    } else if (theme == "blank") {
      theme_mod[[theme_name]] <- ggplot2::element_blank()
    }
    stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
  }
  
  # Create tick annotations
  tick_annotations <- breaks |>
    purrr::imap(\(break_val, i) {
      if (use_normalized) {
        tick_grob <- if (position == "bottom") {
          grid::segmentsGrob(
            x0 = grid::unit(break_val, "npc"),
            x1 = grid::unit(break_val, "npc"),
            y0 = grid::unit(0, "npc"),
            y1 = if (flip_direction) {
              grid::unit(0, "npc") + tick_length
            } else {
              grid::unit(0, "npc") - tick_length
            },
            gp = grid::gpar(
              col = tick_colour,
              lwd = tick_linewidth * 72 / 25.4,
              lineend = "butt"
            )
          )
        } else if (position == "top") {
          grid::segmentsGrob(
            x0 = grid::unit(break_val, "npc"),
            x1 = grid::unit(break_val, "npc"),
            y0 = grid::unit(1, "npc"),
            y1 = if (flip_direction) {
              grid::unit(1, "npc") - tick_length
            } else {
              grid::unit(1, "npc") + tick_length
            },
            gp = grid::gpar(
              col = tick_colour,
              lwd = tick_linewidth * 72 / 25.4,
              lineend = "butt"
            )
          )
        } else if (position == "left") {
          grid::segmentsGrob(
            x0 = grid::unit(0, "npc"),
            x1 = if (flip_direction) {
              grid::unit(0, "npc") + tick_length
            } else {
              grid::unit(0, "npc") - tick_length
            },
            y0 = grid::unit(break_val, "npc"),
            y1 = grid::unit(break_val, "npc"),
            gp = grid::gpar(
              col = tick_colour,
              lwd = tick_linewidth * 72 / 25.4,
              lineend = "butt"
            )
          )
        } else {
          grid::segmentsGrob(
            x0 = grid::unit(1, "npc"),
            x1 = if (flip_direction) {
              grid::unit(1, "npc") - tick_length
            } else {
              grid::unit(1, "npc") + tick_length
            },
            y0 = grid::unit(break_val, "npc"),
            y1 = grid::unit(break_val, "npc"),
            gp = grid::gpar(
              col = tick_colour,
              lwd = tick_linewidth * 72 / 25.4,
              lineend = "butt"
            )
          )
        }
        
        rlang::exec(
          ggplot2::annotation_custom,
          grob = tick_grob,
          xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
        )
      } else {
        tick_grob <- if (position == "bottom") {
          grid::segmentsGrob(
            x0 = grid::unit(0.5, "npc"),
            x1 = grid::unit(0.5, "npc"),
            y0 = grid::unit(0, "npc"),
            y1 = if (flip_direction) {
              grid::unit(0, "npc") + tick_length
            } else {
              grid::unit(0, "npc") - tick_length
            },
            gp = grid::gpar(
              col = tick_colour,
              lwd = tick_linewidth * 72 / 25.4,
              lineend = "butt"
            )
          )
        } else if (position == "top") {
          grid::segmentsGrob(
            x0 = grid::unit(0.5, "npc"),
            x1 = grid::unit(0.5, "npc"),
            y0 = grid::unit(1, "npc"),
            y1 = if (flip_direction) {
              grid::unit(1, "npc") - tick_length
            } else {
              grid::unit(1, "npc") + tick_length
            },
            gp = grid::gpar(
              col = tick_colour,
              lwd = tick_linewidth * 72 / 25.4,
              lineend = "butt"
            )
          )
        } else if (position == "left") {
          grid::segmentsGrob(
            x0 = grid::unit(0, "npc"),
            x1 = if (flip_direction) {
              grid::unit(0, "npc") + tick_length
            } else {
              grid::unit(0, "npc") - tick_length
            },
            y0 = grid::unit(0.5, "npc"),
            y1 = grid::unit(0.5, "npc"),
            gp = grid::gpar(
              col = tick_colour,
              lwd = tick_linewidth * 72 / 25.4,
              lineend = "butt"
            )
          )
        } else {
          grid::segmentsGrob(
            x0 = grid::unit(1, "npc"),
            x1 = if (flip_direction) {
              grid::unit(1, "npc") - tick_length
            } else {
              grid::unit(1, "npc") + tick_length
            },
            y0 = grid::unit(0.5, "npc"),
            y1 = grid::unit(0.5, "npc"),
            gp = grid::gpar(
              col = tick_colour,
              lwd = tick_linewidth * 72 / 25.4,
              lineend = "butt"
            )
          )
        }
        
        if (axis == "x") {
          annotation_position <- if (position == "bottom") {
            list(xmin = break_val, xmax = break_val, ymin = -Inf, ymax = -Inf)
          } else {
            list(xmin = break_val, xmax = break_val, ymin = Inf, ymax = Inf)
          }
        } else {
          annotation_position <- if (position == "left") {
            list(xmin = -Inf, xmax = -Inf, ymin = break_val, ymax = break_val)
          } else {
            list(xmin = Inf, xmax = Inf, ymin = break_val, ymax = break_val)
          }
        }
        
        rlang::exec(
          ggplot2::annotation_custom,
          grob = tick_grob,
          !!!annotation_position
        )
      }
    })
  
  stamp <- c(stamp, tick_annotations)
  
  return(stamp)
}

#' Annotate axis text
#'
#' @description Create annotated text labels for axis breaks.
#'
#' This function is designed to work with a theme that is globally set.
#'
#' It should be used with a `coord` of `ggplot2::coord_cartesian(clip = "off")`.
#'
#' @param ... Require named arguments (and support trailing commas).
#' @param position The position of the axis text. One of `"top"`, `"bottom"`, `"left"`, or `"right"`. Ignored if both `x` and `y` are provided.
#' @param x A vector of x-axis breaks for text positioning. Use `I()` to specify normalized coordinates (0-1).
#' @param y A vector of y-axis breaks for text positioning. Use `I()` to specify normalized coordinates (0-1).
#' @param label A vector of text labels or a function that takes breaks and returns labels. If `NULL`, uses appropriate formatting based on data type.
#' @param colour The colour of the text. Inherits from the current theme `axis.text` etc.
#' @param size The size of the text. Inherits from the current theme `axis.text` etc.
#' @param family The font family of the text. Inherits from the current theme `axis.text` etc.
#' @param length The tick length as a grid unit. Use `rel()` to scale relative to default length. Negative values or `rel()` with negative multiplier place text on the opposite side of the axis (inside the panel). Inherits from the current theme `axis.ticks.length` etc.
#' @param hjust,vjust Horizontal and vertical justification. Auto-calculated based on position if `NULL`. When `length` is negative, justification automatically adjusts for the flipped position.
#' @param angle Text rotation angle. Defaults to `0`.
#' @param theme What to do with the equivalent theme elements. Either `"keep"`, `"transparent"`, or `"blank"`. Defaults to `"keep"`.
#'
#' @return A list of annotation annotates and theme elements.
#' @export
#'
scribe_axis_text <- function(
    ...,
    position = NULL,
    x = NULL,
    y = NULL,
    label = NULL,
    colour = NULL,
    size = NULL,
    family = NULL,
    length = NULL,
    hjust = NULL,
    vjust = NULL,
    angle = 0,
    theme = "keep"
) {
  # Check if both x and y are provided (arbitrary positioning mode)
  arbitrary_position <- !is.null(x) && !is.null(y)
  
  # Determine position from x/y if not specified
  if (!arbitrary_position) {
    if (is.null(position)) {
      if (!is.null(x)) {
        position <- "bottom"
      } else if (!is.null(y)) {
        position <- "left"
      } else {
        stop("Must specify either position, x, y, or both x and y")
      }
    }
    # Validate position for axis mode
    position <- rlang::arg_match(position, c("top", "bottom", "left", "right"))
  }
  
  # Check if values are wrapped in I() to determine coordinate type
  x_is_normalized <- !is.null(x) && inherits(x, "AsIs")
  y_is_normalized <- !is.null(y) && inherits(y, "AsIs")
  
  # Unwrap I() values
  if (x_is_normalized) {
    x <- unclass(x)
    if (any(x < 0 | x > 1)) {
      stop("Normalized x coordinates (specified with I()) must be between 0 and 1")
    }
  }
  if (y_is_normalized) {
    y <- unclass(y)
    if (any(y < 0 | y > 1)) {
      stop("Normalized y coordinates (specified with I()) must be between 0 and 1")
    }
  }
  
  if (arbitrary_position) {
    # Validate that x and y have same length
    if (length(x) != length(y)) {
      stop("x and y must have the same length when both are specified")
    }
    use_normalized <- x_is_normalized || y_is_normalized
    axis <- "x" # Default to x-axis styling when arbitrary positioning
    breaks <- list(x = x, y = y) # Store as list for easier access
  } else {
    # Original validation for axis mode
    if (position %in% c("top", "bottom")) {
      if (!is.null(y)) {
        stop("For top or bottom positions, only x can be specified, not y")
      }
      if (is.null(x)) {
        stop("For top or bottom positions, x must be specified")
      }
      use_normalized <- x_is_normalized
    } else {  # left or right
      if (!is.null(x)) {
        stop("For left or right positions, only y can be specified, not x")
      }
      if (is.null(y)) {
        stop("For left or right positions, y must be specified")
      }
      use_normalized <- y_is_normalized
    }
    axis <- if (position %in% c("top", "bottom")) "x" else "y"
    breaks <- if (!is.null(x)) x else y
  }
  
  theme <- rlang::arg_match(theme, c("keep", "transparent", "blank"))
  
  # Get current theme
  current_theme <- ggplot2::theme_get()
  
  # Check for empty breaks
  n_breaks <- if (arbitrary_position) length(breaks$x) else length(breaks)
  if (n_breaks == 0) {
    return(list())
  }
  
  # Process labels
  if (is.null(label)) {
    if (use_normalized) {
      if (arbitrary_position) {
        labels <- paste0("(", breaks$x, ", ", breaks$y, ")")
      } else {
        labels <- as.character(breaks)
      }
    } else {
      if (arbitrary_position) {
        # Format each coordinate appropriately
        x_labels <- if (inherits(breaks$x, "Date")) {
          format(breaks$x, "%d-%m-%Y")
        } else if (inherits(breaks$x, "POSIXct") || inherits(breaks$x, "POSIXlt")) {
          format(breaks$x, "%d-%m-%Y %H:%M:%S")
        } else if (inherits(breaks$x, "hms") || inherits(breaks$x, "difftime")) {
          as.character(breaks$x)
        } else if (is.numeric(breaks$x)) {
          scales::comma(breaks$x)
        } else {
          as.character(breaks$x)
        }
        
        y_labels <- if (inherits(breaks$y, "Date")) {
          format(breaks$y, "%d-%m-%Y")
        } else if (inherits(breaks$y, "POSIXct") || inherits(breaks$y, "POSIXlt")) {
          format(breaks$y, "%d-%m-%Y %H:%M:%S")
        } else if (inherits(breaks$y, "hms") || inherits(breaks$y, "difftime")) {
          as.character(breaks$y)
        } else if (is.numeric(breaks$y)) {
          scales::comma(breaks$y)
        } else {
          as.character(breaks$y)
        }
        
        labels <- paste0("(", x_labels, ", ", y_labels, ")")
      } else {
        # Check data type and format appropriately
        if (inherits(breaks, "Date")) {
          labels <- format(breaks, "%d-%m-%Y")
        } else if (inherits(breaks, "POSIXct") || inherits(breaks, "POSIXlt")) {
          labels <- format(breaks, "%d-%m-%Y %H:%M:%S")
        } else if (inherits(breaks, "hms") || inherits(breaks, "difftime")) {
          labels <- as.character(breaks)
        } else if (is.numeric(breaks)) {
          labels <- scales::comma(breaks)
        } else {
          labels <- as.character(breaks)
        }
      }
    }
  } else if (is.function(label)) {
    labels <- label(breaks)
  } else {
    labels <- label
  }
  
  # Ensure labels match breaks length
  if (length(labels) != n_breaks) {
    stop("Length of labels must match length of breaks")
  }
  
  # Build hierarchy for axis text from most specific to least specific
  if (arbitrary_position) {
    # For arbitrary positioning, just use general axis.text
    text_hierarchy <- c("axis.text.x", "axis.text")
  } else {
    text_specific <- paste0("axis.text.", axis, ".", position)
    text_axis <- paste0("axis.text.", axis)
    text_general <- "axis.text"
    text_hierarchy <- c(text_specific, text_axis, text_general)
  }
  
  # Use calc_element to properly resolve text properties with inheritance
  resolved_text_element <- NULL
  for (element_name in text_hierarchy) {
    element <- ggplot2::calc_element(element_name, current_theme, skip_blank = TRUE)
    if (!is.null(element) && !inherits(element, "element_blank")) {
      resolved_text_element <- element
      break
    }
  }
  
  # If still no element found, create a minimal fallback
  if (is.null(resolved_text_element)) {
    resolved_text_element <- ggplot2::element_text(
      colour = "black",
      size = 11,
      family = ""
    )
  }
  
  # Extract theme properties with proper resolution
  text_colour <- colour %||% resolved_text_element$colour %||% "black"
  text_size <- size %||% resolved_text_element$size %||% 11
  text_family <- family %||% resolved_text_element$family %||% ""
  
  # Initialize flip_direction flag (needed for hjust/vjust calculation)
  flip_direction <- FALSE
  
  # For arbitrary positioning, skip length calculation
  if (!arbitrary_position) {
    # Function to calculate default tick length
    calculate_default_tick_length <- function() {
      # Build hierarchy for tick length
      length_specific <- paste0("axis.ticks.length.", axis, ".", position)
      length_axis <- paste0("axis.ticks.length.", axis)
      length_general <- "axis.ticks.length"
      length_hierarchy <- c(length_specific, length_axis, length_general)
      
      # Resolve tick length
      resolved_length_element <- NULL
      for (element_name in length_hierarchy) {
        element <- ggplot2::calc_element(element_name, current_theme, skip_blank = TRUE)
        if (!is.null(element) && !inherits(element, "element_blank")) {
          resolved_length_element <- element
          break
        }
      }
      
      tick_length <- resolved_length_element
      
      if (is.null(tick_length)) {
        text_size <- current_theme$text$size %||% 11
        return(grid::unit(0.5 * text_size, "pt"))
      } else if (inherits(tick_length, "rel")) {
        spacing <- current_theme$spacing %||% grid::unit(5.5, "pt")
        if (inherits(spacing, "unit")) {
          spacing_pts <- as.numeric(grid::convertUnit(spacing, "pt"))
        } else {
          spacing_pts <- 5.5
        }
        return(grid::unit(as.numeric(tick_length) * spacing_pts, "pt"))
      } else if (!inherits(tick_length, "unit")) {
        if (is.numeric(tick_length)) {
          return(grid::unit(tick_length, "pt"))
        } else {
          text_size <- current_theme$text$size %||% 11
          return(grid::unit(0.5 * text_size, "pt"))
        }
      } else {
        return(tick_length)
      }
    }
    
    # Calculate tick length
    if (is.null(length)) {
      tick_length <- calculate_default_tick_length()
    } else {
      if (inherits(length, "rel")) {
        # Handle rel() objects
        default_tick_length <- calculate_default_tick_length()
        rel_value <- as.numeric(length)
        default_pts <- as.numeric(grid::convertUnit(default_tick_length, "pt"))
        tick_length <- grid::unit(abs(rel_value) * default_pts, "pt")
        flip_direction <- rel_value < 0
      } else if (inherits(length, "unit")) {
        tick_length <- length
        # Check if it's negative
        tick_pts <- as.numeric(grid::convertUnit(length, "pt"))
        if (tick_pts < 0) {
          tick_length <- grid::unit(abs(tick_pts), "pt")
          flip_direction <- TRUE
        }
      } else if (is.numeric(length)) {
        tick_length <- grid::unit(abs(length), "pt")
        flip_direction <- length < 0
      } else {
        tick_length <- calculate_default_tick_length()
      }
    }
    
    # Get the text margin from theme (gap between tick and text)
    text_margin <- resolved_text_element$margin
    margin_unit <- grid::unit(2, "pt")  # Default fallback
    
    if (!is.null(text_margin)) {
      margin_index <- if (position == "bottom") {
        1  # top margin
      } else if (position == "top") {
        3  # bottom margin
      } else if (position == "left") {
        2  # right margin
      } else {
        4  # left margin
      }
      
      if (inherits(text_margin, "margin")) {
        # margin objects are like units with 4 values
        margin_unit <- text_margin[margin_index]
      } else if (inherits(text_margin, "unit") && length(text_margin) >= margin_index) {
        margin_unit <- text_margin[margin_index]
      } else if (inherits(text_margin, "unit") && length(text_margin) == 1) {
        # Single unit value applies to all sides
        margin_unit <- text_margin
      }
    }
    
    # Calculate total distance from axis line to text
    total_length <- tick_length + margin_unit
  }
  
  # Set hjust and vjust based on position or use defaults for arbitrary
  if (arbitrary_position) {
    if (is.null(hjust)) hjust <- 0.5
    if (is.null(vjust)) vjust <- 0.5
  } else {
    if (is.null(hjust)) {
      hjust <- if (position %in% c("top", "bottom")) {
        0.5
      } else if (position == "left") {
        if (flip_direction) 0 else 1  # Flip hjust when flipping direction
      } else {
        if (flip_direction) 1 else 0  # Flip hjust when flipping direction
      }
    }
    
    if (is.null(vjust)) {
      vjust <- if (position == "bottom") {
        if (flip_direction) 0 else 1  # Flip vjust when flipping direction
      } else if (position == "top") {
        if (flip_direction) 1 else 0  # Flip vjust when flipping direction
      } else {
        0.5
      }
    }
  }
  
  stamp <- list()
  
  # Add theme modification if requested (only for axis positioning)
  if (!arbitrary_position && theme != "keep") {
    theme_name <- paste0("axis.text.", axis, ".", position)
    theme_mod <- list()
    if (theme == "transparent") {
      theme_mod[[theme_name]] <- ggplot2::element_text(colour = "transparent")
    } else if (theme == "blank") {
      theme_mod[[theme_name]] <- ggplot2::element_blank()
    }
    stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
  }
  
  # Create annotations
  if (arbitrary_position) {
    # For arbitrary positioning, create text at specified x,y coordinates
    text_annotations <- seq_len(n_breaks) |>
      purrr::map(\(i) {
        label_val <- labels[i]
        
        if (use_normalized) {
          # Create normalized grob
          text_grob <- grid::textGrob(
            label_val,
            x = grid::unit(breaks$x[i], "npc"),
            y = grid::unit(breaks$y[i], "npc"),
            just = c(hjust, vjust),
            rot = angle,
            gp = grid::gpar(
              col = text_colour,
              fontsize = text_size,
              fontfamily = text_family
            )
          )
          
          ggplot2::annotation_custom(
            grob = text_grob,
            xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
          )
        } else {
          # Use annotate for data coordinates
          ggplot2::annotate(
            "text",
            x = breaks$x[i],
            y = breaks$y[i],
            label = label_val,
            colour = text_colour,
            size = text_size / 2.845276,
            family = text_family,
            hjust = hjust,
            vjust = vjust,
            angle = angle
          )
        }
      })
    
    stamp <- c(stamp, text_annotations)
  } else {
    # Original axis-based annotation code
    text_annotations <- breaks |>
      purrr::imap(\(break_val, i) {
        label_val <- labels[i]
        
        # For normalized coordinates, use them directly as npc units
        if (use_normalized) {
          text_grob <- if (position == "bottom") {
            # Apply flip_direction to change which side of axis
            y_pos <- if (flip_direction) {
              grid::unit(0, "npc") + total_length
            } else {
              grid::unit(0, "npc") - total_length
            }
            grid::textGrob(
              label_val,
              x = grid::unit(break_val, "npc"),
              y = y_pos,
              just = c(hjust, vjust),
              rot = angle,
              gp = grid::gpar(
                col = text_colour,
                fontsize = text_size,
                fontfamily = text_family
              )
            )
          } else if (position == "top") {
            # Apply flip_direction to change which side of axis
            y_pos <- if (flip_direction) {
              grid::unit(1, "npc") - total_length
            } else {
              grid::unit(1, "npc") + total_length
            }
            grid::textGrob(
              label_val,
              x = grid::unit(break_val, "npc"),
              y = y_pos,
              just = c(hjust, vjust),
              rot = angle,
              gp = grid::gpar(
                col = text_colour,
                fontsize = text_size,
                fontfamily = text_family
              )
            )
          } else if (position == "left") {
            # Apply flip_direction to change which side of axis
            x_pos <- if (flip_direction) {
              grid::unit(0, "npc") + total_length
            } else {
              grid::unit(0, "npc") - total_length
            }
            grid::textGrob(
              label_val,
              x = x_pos,
              y = grid::unit(break_val, "npc"),
              just = c(hjust, vjust),
              rot = angle,
              gp = grid::gpar(
                col = text_colour,
                fontsize = text_size,
                fontfamily = text_family
              )
            )
          } else {  # right
            # Apply flip_direction to change which side of axis
            x_pos <- if (flip_direction) {
              grid::unit(1, "npc") - total_length
            } else {
              grid::unit(1, "npc") + total_length
            }
            grid::textGrob(
              label_val,
              x = x_pos,
              y = grid::unit(break_val, "npc"),
              just = c(hjust, vjust),
              rot = angle,
              gp = grid::gpar(
                col = text_colour,
                fontsize = text_size,
                fontfamily = text_family
              )
            )
          }
          
          rlang::exec(
            ggplot2::annotation_custom,
            grob = text_grob,
            xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
          )
        } else {
          # Original behavior for data coordinates
          text_grob <- if (position == "bottom") {
            # Apply flip_direction to change which side of axis
            y_pos <- if (flip_direction) {
              grid::unit(0, "npc") + total_length
            } else {
              grid::unit(0, "npc") - total_length
            }
            grid::textGrob(
              label_val,
              x = grid::unit(0.5, "npc"),
              y = y_pos,
              just = c(hjust, vjust),
              rot = angle,
              gp = grid::gpar(
                col = text_colour,
                fontsize = text_size,
                fontfamily = text_family
              )
            )
          } else if (position == "top") {
            # Apply flip_direction to change which side of axis
            y_pos <- if (flip_direction) {
              grid::unit(1, "npc") - total_length
            } else {
              grid::unit(1, "npc") + total_length
            }
            grid::textGrob(
              label_val,
              x = grid::unit(0.5, "npc"),
              y = y_pos,
              just = c(hjust, vjust),
              rot = angle,
              gp = grid::gpar(
                col = text_colour,
                fontsize = text_size,
                fontfamily = text_family
              )
            )
          } else if (position == "left") {
            # Apply flip_direction to change which side of axis
            x_pos <- if (flip_direction) {
              grid::unit(0, "npc") + total_length
            } else {
              grid::unit(0, "npc") - total_length
            }
            grid::textGrob(
              label_val,
              x = x_pos,
              y = grid::unit(0.5, "npc"),
              just = c(hjust, vjust),
              rot = angle,
              gp = grid::gpar(
                col = text_colour,
                fontsize = text_size,
                fontfamily = text_family
              )
            )
          } else {  # right
            # Apply flip_direction to change which side of axis
            x_pos <- if (flip_direction) {
              grid::unit(1, "npc") - total_length
            } else {
              grid::unit(1, "npc") + total_length
            }
            grid::textGrob(
              label_val,
              x = x_pos,
              y = grid::unit(0.5, "npc"),
              just = c(hjust, vjust),
              rot = angle,
              gp = grid::gpar(
                col = text_colour,
                fontsize = text_size,
                fontfamily = text_family
              )
            )
          }
          
          # Set annotation position based on axis and position
          if (axis == "x") {
            annotation_position <- if (position == "bottom") {
              list(xmin = break_val, xmax = break_val, ymin = -Inf, ymax = -Inf)
            } else {  # top
              list(xmin = break_val, xmax = break_val, ymin = Inf, ymax = Inf)
            }
          } else {  # y axis
            annotation_position <- if (position == "left") {
              list(xmin = -Inf, xmax = -Inf, ymin = break_val, ymax = break_val)
            } else {  # right
              list(xmin = Inf, xmax = Inf, ymin = break_val, ymax = break_val)
            }
          }
          
          rlang::exec(
            ggplot2::annotation_custom,
            grob = text_grob,
            !!!annotation_position
          )
        }
      })
    
    stamp <- c(stamp, text_annotations)
  }
  
  return(stamp)
}
