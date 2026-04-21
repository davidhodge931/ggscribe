# Internal helpers ------------------------------------------------------------

.validate_intercept <- function(axis, position, xintercept, yintercept) {
  if (!is.null(xintercept) && axis == "x") {
    rlang::abort(glue::glue(
      "`xintercept` is not applicable for position = \"{position}\". ",
      "Did you mean `yintercept`?"
    ))
  }
  if (!is.null(yintercept) && axis == "y") {
    rlang::abort(glue::glue(
      "`yintercept` is not applicable for position = \"{position}\". ",
      "Did you mean `xintercept`?"
    ))
  }
  invisible(NULL)
}

.resolve_intercept <- function(axis, position, xintercept, yintercept) {
  if (axis == "x") {
    yintercept %||% if (position == "bottom") -Inf else Inf
  } else {
    xintercept %||% if (position == "left") -Inf else Inf
  }
}

.infer_position <- function(position, xintercept, yintercept) {
  if (!is.null(position)) {
    return(rlang::arg_match(position, c("top", "bottom", "left", "right")))
  }
  if (!is.null(yintercept)) return("bottom")
  if (!is.null(xintercept)) return("left")
  rlang::abort(
    "Must specify `position`, or supply `xintercept` (implies left/right) or `yintercept` (implies top/bottom)."
  )
}


# annotate_axis_line ----------------------------------------------------------

#' Annotate an axis line
#'
#' Draws a line along an axis edge, with style defaults taken from the
#' `axis.line` element of the set theme. Requires `coord_cartesian(clip = "off")`.
#'
#' @param ... Not used. Forces named arguments.
#' @param position One of `"top"`, `"bottom"`, `"left"`, or `"right"`. Inferred
#'   from `xintercept` or `yintercept` if not provided.
#' @param xintercept For `"left"`/`"right"` axes: float the axis to this x
#'   position in data coordinates instead of the panel edge.
#' @param yintercept For `"top"`/`"bottom"` axes: float the axis to this y
#'   position in data coordinates instead of the panel edge.
#' @param colour Inherits from `axis.line` in the set theme.
#' @param linewidth Inherits from `axis.line` in the set theme. Supports `rel()`.
#' @param linetype Inherits from `axis.line` in the set theme.
#' @param elements_to One of `"keep"`, `"transparent"`, or `"blank"`. Controls
#'   whether the native theme axis line is suppressed. Defaults to `"transparent"`.
#'
#' @return A list of ggplot2 annotation layers and theme elements.
#' @seealso [annotate_axis_ticks()], [annotate_axis_text()], [annotate_axis_bracket()], [annotate_reference_line()]
#' @export
annotate_axis_line <- function(
    ...,
    position    = NULL,
    xintercept  = NULL,
    yintercept  = NULL,
    colour      = NULL,
    linewidth   = NULL,
    linetype    = NULL,
    elements_to = "transparent"
) {
  rlang::check_dots_empty()

  position    <- .infer_position(position, xintercept, yintercept)
  axis        <- if (position %in% c("top", "bottom")) "x" else "y"
  elements_to <- rlang::arg_match(elements_to, c("keep", "transparent", "blank"))

  .validate_intercept(axis, position, xintercept, yintercept)

  intercept     <- .resolve_intercept(axis, position, xintercept, yintercept)
  current_theme <- ggplot2::theme_get()

  element_hierarchy <- c(
    paste0("axis.line.", axis, ".", position),
    paste0("axis.line.", axis),
    "axis.line"
  )

  theme_element_blank <- NULL
  for (nm in element_hierarchy) {
    el <- ggplot2::calc_element(nm, current_theme, skip_blank = FALSE)
    if (!is.null(el)) { theme_element_blank <- el; break }
  }
  axis_line_intentionally_blank <- is.null(theme_element_blank) ||
    inherits(theme_element_blank, "element_blank")

  resolved_element <- NULL
  if (!axis_line_intentionally_blank) {
    for (nm in element_hierarchy) {
      el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
      if (!is.null(el) && !inherits(el, "element_blank")) { resolved_element <- el; break }
    }
  }

  if (is.null(colour) && (axis_line_intentionally_blank || is.null(resolved_element$colour))) {
    rlang::warn("The set theme does not define an `axis.line` colour. Defaulting to \"black\".")
  }
  if (is.null(linewidth) && (axis_line_intentionally_blank || is.null(resolved_element$linewidth))) {
    rlang::warn("The set theme does not define an `axis.line` linewidth. Defaulting to `0.5`.")
  }
  if (is.null(resolved_element)) {
    resolved_element <- list(colour = "black", linewidth = 0.5, linetype = 1)
  }

  line_colour    <- colour %||% resolved_element$colour %||% "black"
  line_linewidth <- if (is.null(linewidth)) {
    resolved_element$linewidth %||% 0.5
  } else if (inherits(linewidth, "rel")) {
    as.numeric(linewidth) * (resolved_element$linewidth %||% 0.5)
  } else {
    linewidth
  }
  line_linetype <- linetype %||% resolved_element$linetype %||% 1

  stamp <- list()

  if (axis == "x") {
    stamp <- c(stamp, list(ggplot2::annotate(
      "segment", x = -Inf, xend = Inf, y = intercept, yend = intercept,
      colour = line_colour, linewidth = line_linewidth, linetype = line_linetype
    )))
  } else {
    stamp <- c(stamp, list(ggplot2::annotate(
      "segment", x = intercept, xend = intercept, y = -Inf, yend = Inf,
      colour = line_colour, linewidth = line_linewidth, linetype = line_linetype
    )))
  }

  if (elements_to != "keep") {
    theme_name <- paste0("axis.line.", axis, ".", position)
    theme_mod  <- list()
    theme_mod[[theme_name]] <- if (elements_to == "transparent") {
      ggplot2::element_line(colour = "transparent")
    } else {
      ggplot2::element_blank()
    }
    stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
  }

  return(stamp)
}


# annotate_axis_ticks ---------------------------------------------------------

#' Annotate axis ticks
#'
#' Draws axis ticks at specified break positions, with style defaults taken from
#' the `axis.ticks` element of the set theme. Requires
#' `coord_cartesian(clip = "off")`.
#'
#' @param ... Not used. Forces named arguments.
#' @param position One of `"top"`, `"bottom"`, `"left"`, or `"right"`. Inferred
#'   from `xintercept` or `yintercept` if not provided.
#' @param xintercept For `"left"`/`"right"` axes: float the axis to this x
#'   position in data coordinates instead of the panel edge.
#' @param yintercept For `"top"`/`"bottom"` axes: float the axis to this y
#'   position in data coordinates instead of the panel edge.
#' @param breaks A numeric vector of break positions.
#' @param minor Logical. If `TRUE`, uses minor tick theme defaults. Defaults to
#'   `FALSE`.
#' @param colour Inherits from `axis.ticks` in the set theme.
#' @param linewidth Inherits from `axis.ticks` in the set theme. Supports `rel()`.
#' @param tick_length Total tick length as a grid unit. Supports `rel()` to
#'   scale relative to the theme default. Negative values flip the tick direction.
#' @param elements_to One of `"keep"`, `"transparent"`, or `"blank"`. Controls
#'   whether native theme ticks are suppressed. Defaults to `"transparent"`.
#'
#' @return A list of ggplot2 annotation layers and theme elements.
#' @seealso [annotate_axis_line()], [annotate_axis_text()], [annotate_axis_bracket()], [annotate_reference_line()]
#' @export
annotate_axis_ticks <- function(
    ...,
    position    = NULL,
    xintercept  = NULL,
    yintercept  = NULL,
    breaks,
    minor       = FALSE,
    colour      = NULL,
    linewidth   = NULL,
    tick_length = NULL,
    elements_to = "transparent"
) {
  rlang::check_dots_empty()

  position    <- .infer_position(position, xintercept, yintercept)
  axis        <- if (position %in% c("top", "bottom")) "x" else "y"
  elements_to <- rlang::arg_match(elements_to, c("keep", "transparent", "blank"))

  .validate_intercept(axis, position, xintercept, yintercept)

  if (length(breaks) == 0) return(list())

  intercept     <- .resolve_intercept(axis, position, xintercept, yintercept)
  current_theme <- ggplot2::theme_get()

  tick_hierarchy <- if (minor) {
    c(paste0("axis.minor.ticks.", axis, ".", position), paste0("axis.ticks.", axis, ".", position),
      paste0("axis.ticks.", axis), "axis.ticks")
  } else {
    c(paste0("axis.ticks.", axis, ".", position), paste0("axis.ticks.", axis), "axis.ticks")
  }

  resolved_tick_element <- NULL
  tick_intentionally_blank <- FALSE
  for (nm in tick_hierarchy) {
    el_raw <- ggplot2::calc_element(nm, current_theme, skip_blank = FALSE)
    if (!is.null(el_raw)) {
      if (inherits(el_raw, "element_blank")) { tick_intentionally_blank <- TRUE; break }
      el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
      if (!is.null(el) && !inherits(el, "element_blank")) { resolved_tick_element <- el; break }
    }
  }

  if (is.null(colour) && (tick_intentionally_blank || is.null(resolved_tick_element$colour))) {
    rlang::warn("The set theme does not define an `axis.ticks` colour. Defaulting to \"black\".")
  }
  if (is.null(linewidth) && (tick_intentionally_blank || is.null(resolved_tick_element$linewidth))) {
    rlang::warn("The set theme does not define an `axis.ticks` linewidth. Defaulting to `0.5`.")
  }
  if (is.null(resolved_tick_element)) {
    resolved_tick_element <- list(colour = "black", linewidth = 0.5)
  }

  length_hierarchy <- if (minor) {
    c(paste0("axis.minor.ticks.length.", axis, ".", position), paste0("axis.minor.ticks.length.", axis),
      "axis.minor.ticks.length", paste0("axis.ticks.length.", axis, ".", position),
      paste0("axis.ticks.length.", axis), "axis.ticks.length")
  } else {
    c(paste0("axis.ticks.length.", axis, ".", position), paste0("axis.ticks.length.", axis), "axis.ticks.length")
  }

  resolved_length_element <- NULL
  for (nm in length_hierarchy) {
    el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
    if (!is.null(el) && !inherits(el, "element_blank")) { resolved_length_element <- el; break }
  }

  tick_colour    <- colour %||% resolved_tick_element$colour %||% "black"
  tick_linewidth <- if (is.null(linewidth)) {
    resolved_tick_element$linewidth %||% 0.5
  } else if (inherits(linewidth, "rel")) {
    as.numeric(linewidth) * (resolved_tick_element$linewidth %||% 0.5)
  } else {
    linewidth
  }

  calculate_default_length <- function() {
    tl <- resolved_length_element
    if (is.null(tl)) {
      return(grid::unit((if (minor) 0.375 else 0.5) * (current_theme$text$size %||% 11), "pt"))
    } else if (inherits(tl, "rel")) {
      spacing_pts <- as.numeric(grid::convertUnit(current_theme$spacing %||% grid::unit(5.5, "pt"), "pt"))
      return(grid::unit(as.numeric(tl) * spacing_pts, "pt"))
    } else if (!inherits(tl, "unit")) {
      return(grid::unit(
        if (is.numeric(tl)) tl else (if (minor) 0.375 else 0.5) * (current_theme$text$size %||% 11), "pt"
      ))
    } else {
      return(tl)
    }
  }

  flip_direction <- FALSE
  if (is.null(tick_length)) {
    tick_length <- calculate_default_length()
  } else if (inherits(tick_length, "rel")) {
    rel_value      <- as.numeric(tick_length)
    default_pts    <- as.numeric(grid::convertUnit(calculate_default_length(), "pt"))
    tick_length    <- grid::unit(abs(rel_value) * default_pts, "pt")
    flip_direction <- rel_value < 0
  } else if (inherits(tick_length, "unit")) {
    tick_pts       <- as.numeric(grid::convertUnit(tick_length, "pt"))
    tick_length    <- grid::unit(abs(tick_pts), "pt")
    flip_direction <- tick_pts < 0
  } else if (is.numeric(tick_length)) {
    flip_direction <- tick_length < 0
    tick_length    <- grid::unit(abs(tick_length), "pt")
  } else {
    tick_length <- calculate_default_length()
  }

  stamp <- list()

  if (elements_to != "keep") {
    theme_name <- if (minor) {
      paste0("axis.minor.ticks.", axis, ".", position)
    } else {
      paste0("axis.ticks.", axis, ".", position)
    }
    theme_mod <- list()
    theme_mod[[theme_name]] <- if (elements_to == "transparent") {
      ggplot2::element_line(colour = "transparent")
    } else {
      ggplot2::element_blank()
    }
    stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
  }

  gp <- ggplot2::gg_par(col = tick_colour, stroke = tick_linewidth, lineend = "butt")

  tick_annotations <- lapply(breaks, \(break_val) {
    tick_grob <- if (position == "bottom") {
      grid::segmentsGrob(
        x0 = grid::unit(0.5, "npc"), x1 = grid::unit(0.5, "npc"),
        y0 = grid::unit(0, "npc"),
        y1 = if (flip_direction) grid::unit(0, "npc") + tick_length
        else                grid::unit(0, "npc") - tick_length,
        gp = gp
      )
    } else if (position == "top") {
      grid::segmentsGrob(
        x0 = grid::unit(0.5, "npc"), x1 = grid::unit(0.5, "npc"),
        y0 = grid::unit(1, "npc"),
        y1 = if (flip_direction) grid::unit(1, "npc") - tick_length
        else                grid::unit(1, "npc") + tick_length,
        gp = gp
      )
    } else if (position == "left") {
      grid::segmentsGrob(
        x0 = grid::unit(0, "npc"),
        x1 = if (flip_direction) grid::unit(0, "npc") + tick_length
        else                grid::unit(0, "npc") - tick_length,
        y0 = grid::unit(0.5, "npc"), y1 = grid::unit(0.5, "npc"),
        gp = gp
      )
    } else {
      grid::segmentsGrob(
        x0 = grid::unit(1, "npc"),
        x1 = if (flip_direction) grid::unit(1, "npc") - tick_length
        else                grid::unit(1, "npc") + tick_length,
        y0 = grid::unit(0.5, "npc"), y1 = grid::unit(0.5, "npc"),
        gp = gp
      )
    }

    annotation_position <- if (axis == "x") {
      list(xmin = break_val, xmax = break_val, ymin = intercept, ymax = intercept)
    } else {
      list(xmin = intercept, xmax = intercept, ymin = break_val, ymax = break_val)
    }

    rlang::exec(ggplot2::annotation_custom, grob = tick_grob, !!!annotation_position)
  })

  c(stamp, tick_annotations)
}


# annotate_axis_text ----------------------------------------------------------

#' Annotate axis text
#'
#' Draws text labels at specified break positions along an axis, with style
#' defaults taken from the `axis.text` element of the set theme. Requires
#' `coord_cartesian(clip = "off")`.
#'
#' @param ... Not used. Forces named arguments.
#' @param position One of `"top"`, `"bottom"`, `"left"`, or `"right"`. Inferred
#'   from `xintercept` or `yintercept` if not provided.
#' @param xintercept For `"left"`/`"right"` axes: float the axis to this x
#'   position in data coordinates instead of the panel edge.
#' @param yintercept For `"top"`/`"bottom"` axes: float the axis to this y
#'   position in data coordinates instead of the panel edge.
#' @param breaks A numeric vector of break positions.
#' @param labels One of:
#'   - `NULL` (default) to auto-format break values
#'   - A character vector the same length as `breaks`
#'   - A function taking break values and returning labels
#' @param colour Inherits from `axis.text` in the set theme.
#' @param size Inherits from `axis.text` in the set theme.
#' @param family Inherits from `axis.text` in the set theme.
#' @param hjust,vjust Justification. Auto-calculated from `position` if `NULL`.
#' @param angle Text rotation angle. Defaults to `0`.
#' @param tick_length Offset from the axis edge including tick length and margin.
#'   Supports `rel()`. Negative values place labels inside the panel.
#' @param elements_to One of `"keep"`, `"transparent"`, or `"blank"`. Controls
#'   whether native theme axis text is suppressed. Defaults to `"transparent"`.
#'
#' @return A list of ggplot2 annotation layers and theme elements.
#' @seealso [annotate_axis_line()], [annotate_axis_ticks()], [annotate_axis_bracket()], [annotate_reference_line()]
#' @export
annotate_axis_text <- function(
    ...,
    position    = NULL,
    xintercept  = NULL,
    yintercept  = NULL,
    breaks,
    labels      = NULL,
    colour      = NULL,
    size        = NULL,
    family      = NULL,
    hjust       = NULL,
    vjust       = NULL,
    angle       = 0,
    tick_length = NULL,
    elements_to = "transparent"
) {
  rlang::check_dots_empty()

  position    <- .infer_position(position, xintercept, yintercept)
  axis        <- if (position %in% c("top", "bottom")) "x" else "y"
  elements_to <- rlang::arg_match(elements_to, c("keep", "transparent", "blank"))

  .validate_intercept(axis, position, xintercept, yintercept)

  if (length(breaks) == 0) return(list())

  intercept     <- .resolve_intercept(axis, position, xintercept, yintercept)
  current_theme <- ggplot2::theme_get()

  text_hierarchy <- c(
    paste0("axis.text.", axis, ".", position),
    paste0("axis.text.", axis),
    "axis.text"
  )

  resolved_text_element <- NULL
  for (nm in text_hierarchy) {
    el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
    if (!is.null(el) && !inherits(el, "element_blank")) { resolved_text_element <- el; break }
  }
  if (is.null(resolved_text_element)) {
    resolved_text_element <- ggplot2::element_text(colour = "black", size = 11, family = "")
  }

  text_colour <- colour %||% resolved_text_element$colour %||% "black"
  text_size   <- size   %||% resolved_text_element$size   %||% 11
  text_family <- family %||% resolved_text_element$family %||% ""

  if (is.null(labels)) {
    labels <- if (inherits(breaks, "Date")) {
      format(breaks, "%d-%m-%Y")
    } else if (inherits(breaks, c("POSIXct", "POSIXlt"))) {
      format(breaks, "%d-%m-%Y %H:%M:%S")
    } else if (is.numeric(breaks)) {
      scales::comma(breaks)
    } else {
      as.character(breaks)
    }
  } else if (is.function(labels)) {
    labels <- labels(breaks)
  }

  if (length(labels) != length(breaks)) {
    rlang::abort("Length of `labels` must match length of `breaks`.")
  }

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

  calculate_default_tick_length <- function() {
    tl <- resolved_length
    if (is.null(tl)) {
      return(grid::unit(0.5 * (current_theme$text$size %||% 11), "pt"))
    } else if (inherits(tl, "rel")) {
      spacing_pts <- as.numeric(grid::convertUnit(current_theme$spacing %||% grid::unit(5.5, "pt"), "pt"))
      return(grid::unit(as.numeric(tl) * spacing_pts, "pt"))
    } else if (!inherits(tl, "unit")) {
      return(grid::unit(if (is.numeric(tl)) tl else 0.5 * (current_theme$text$size %||% 11), "pt"))
    } else {
      return(tl)
    }
  }

  flip_direction <- FALSE
  if (is.null(tick_length)) {
    tick_length <- calculate_default_tick_length()
  } else if (inherits(tick_length, "rel")) {
    rel_value      <- as.numeric(tick_length)
    default_pts    <- as.numeric(grid::convertUnit(calculate_default_tick_length(), "pt"))
    tick_length    <- grid::unit(abs(rel_value) * default_pts, "pt")
    flip_direction <- rel_value < 0
  } else if (inherits(tick_length, "unit")) {
    tick_pts       <- as.numeric(grid::convertUnit(tick_length, "pt"))
    tick_length    <- grid::unit(abs(tick_pts), "pt")
    flip_direction <- tick_pts < 0
  } else if (is.numeric(tick_length)) {
    flip_direction <- tick_length < 0
    tick_length    <- grid::unit(abs(tick_length), "pt")
  } else {
    tick_length <- calculate_default_tick_length()
  }

  text_margin  <- resolved_text_element$margin
  margin_unit  <- grid::unit(2, "pt")
  if (!is.null(text_margin)) {
    margin_index <- switch(position, bottom = 1L, top = 3L, left = 2L, right = 4L)
    if (inherits(text_margin, c("margin", "unit")) && length(text_margin) >= margin_index) {
      margin_unit <- text_margin[margin_index]
    }
  }
  total_length <- tick_length + margin_unit

  if (is.null(hjust)) {
    hjust <- if (position %in% c("top", "bottom")) 0.5
    else if (position == "left") { if (flip_direction) 0 else 1 }
    else { if (flip_direction) 1 else 0 }
  }
  if (is.null(vjust)) {
    vjust <- if (position == "bottom") { if (flip_direction) 0 else 1 }
    else if (position == "top") { if (flip_direction) 1 else 0 }
    else 0.5
  }

  stamp <- list()

  if (elements_to != "keep") {
    theme_name <- paste0("axis.text.", axis, ".", position)
    theme_mod  <- list()
    theme_mod[[theme_name]] <- if (elements_to == "transparent") {
      ggplot2::element_text(colour = "transparent")
    } else {
      ggplot2::element_blank()
    }
    stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
  }

  make_gpar <- function() {
    grid::gpar(col = text_colour, fontsize = text_size, fontfamily = text_family)
  }

  text_annotations <- lapply(seq_along(breaks), \(i) {
    break_val <- breaks[[i]]

    text_grob <- if (position == "bottom") {
      grid::textGrob(
        labels[i],
        x    = grid::unit(0.5, "npc"),
        y    = if (flip_direction) grid::unit(0, "npc") + total_length
        else                grid::unit(0, "npc") - total_length,
        just = c(hjust, vjust), rot = angle, gp = make_gpar()
      )
    } else if (position == "top") {
      grid::textGrob(
        labels[i],
        x    = grid::unit(0.5, "npc"),
        y    = if (flip_direction) grid::unit(1, "npc") - total_length
        else                grid::unit(1, "npc") + total_length,
        just = c(hjust, vjust), rot = angle, gp = make_gpar()
      )
    } else if (position == "left") {
      grid::textGrob(
        labels[i],
        x    = if (flip_direction) grid::unit(0, "npc") + total_length
        else                grid::unit(0, "npc") - total_length,
        y    = grid::unit(0.5, "npc"),
        just = c(hjust, vjust), rot = angle, gp = make_gpar()
      )
    } else {
      grid::textGrob(
        labels[i],
        x    = if (flip_direction) grid::unit(1, "npc") - total_length
        else                grid::unit(1, "npc") + total_length,
        y    = grid::unit(0.5, "npc"),
        just = c(hjust, vjust), rot = angle, gp = make_gpar()
      )
    }

    annotation_position <- if (axis == "x") {
      list(xmin = break_val, xmax = break_val, ymin = intercept, ymax = intercept)
    } else {
      list(xmin = intercept, xmax = intercept, ymin = break_val, ymax = break_val)
    }

    rlang::exec(ggplot2::annotation_custom, grob = text_grob, !!!annotation_position)
  })

  c(stamp, text_annotations)
}


# annotate_axis_bracket -------------------------------------------------------

#' Annotate an axis bracket
#'
#' Draws a bracket spanning `min(breaks)` to `max(breaks)` along an axis edge.
#' Style defaults are taken from the `axis.line` element of the set theme.
#' Requires `coord_cartesian(clip = "off")`.
#'
#' @param ... Not used. Forces named arguments.
#' @param position One of `"top"`, `"bottom"`, `"left"`, or `"right"`. Inferred
#'   from `xintercept` or `yintercept` if not provided.
#' @param xintercept For `"left"`/`"right"` axes: float the bracket to this x
#'   position in data coordinates instead of the panel edge.
#' @param yintercept For `"top"`/`"bottom"` axes: float the bracket to this y
#'   position in data coordinates instead of the panel edge.
#' @param breaks A numeric vector of length >= 2. The bracket spans
#'   `min(breaks)` to `max(breaks)`.
#' @param colour Inherits from `axis.line` in the set theme.
#' @param linewidth Inherits from `axis.line` in the set theme. Supports `rel()`.
#' @param linetype Inherits from `axis.line` in the set theme.
#' @param tick_length Length of the bracket end caps as a grid unit. Supports
#'   `rel()`. Defaults to the theme tick length.
#' @param elements_to One of `"keep"`, `"transparent"`, or `"blank"`. Controls
#'   whether native theme ticks are suppressed. Defaults to `"transparent"`.
#'
#' @return A list of ggplot2 annotation layers and theme elements.
#' @seealso [annotate_axis_line()], [annotate_axis_ticks()], [annotate_axis_text()], [annotate_reference_line()]
#' @export
annotate_axis_bracket <- function(
    ...,
    position    = NULL,
    xintercept  = NULL,
    yintercept  = NULL,
    breaks,
    colour      = NULL,
    linewidth   = NULL,
    linetype    = NULL,
    tick_length = NULL,
    elements_to = "transparent"
) {
  rlang::check_dots_empty()

  position    <- .infer_position(position, xintercept, yintercept)
  axis        <- if (position %in% c("top", "bottom")) "x" else "y"
  elements_to <- rlang::arg_match(elements_to, c("keep", "transparent", "blank"))

  .validate_intercept(axis, position, xintercept, yintercept)

  if (length(breaks) < 2) {
    rlang::abort("`breaks` must have at least 2 values to define the bracket span.")
  }

  bracket_from  <- min(breaks)
  bracket_to    <- max(breaks)
  intercept     <- .resolve_intercept(axis, position, xintercept, yintercept)
  current_theme <- ggplot2::theme_get()

  line_hierarchy <- c(
    paste0("axis.line.", axis, ".", position),
    paste0("axis.line.", axis),
    "axis.line"
  )
  resolved_line_element <- NULL
  for (nm in line_hierarchy) {
    el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
    if (!is.null(el) && !inherits(el, "element_blank")) { resolved_line_element <- el; break }
  }
  if (is.null(resolved_line_element)) {
    resolved_line_element <- list(colour = "black", linewidth = 0.5, linetype = 1)
  }

  line_colour    <- colour %||% resolved_line_element$colour %||% "black"
  line_linewidth <- if (is.null(linewidth)) {
    resolved_line_element$linewidth %||% 0.5
  } else if (inherits(linewidth, "rel")) {
    as.numeric(linewidth) * (resolved_line_element$linewidth %||% 0.5)
  } else {
    linewidth
  }
  line_linetype <- linetype %||% resolved_line_element$linetype %||% 1

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

  calculate_default_length <- function() {
    tl <- resolved_length
    if (is.null(tl)) {
      return(grid::unit(0.5 * (current_theme$text$size %||% 11), "pt"))
    } else if (inherits(tl, "rel")) {
      spacing_pts <- as.numeric(grid::convertUnit(current_theme$spacing %||% grid::unit(5.5, "pt"), "pt"))
      return(grid::unit(as.numeric(tl) * spacing_pts, "pt"))
    } else if (!inherits(tl, "unit")) {
      return(grid::unit(if (is.numeric(tl)) tl else 0.5 * (current_theme$text$size %||% 11), "pt"))
    } else {
      return(tl)
    }
  }

  cap_length <- if (is.null(tick_length)) {
    calculate_default_length()
  } else if (inherits(tick_length, "rel")) {
    grid::unit(as.numeric(tick_length) * as.numeric(grid::convertUnit(calculate_default_length(), "pt")), "pt")
  } else if (inherits(tick_length, "unit")) {
    tick_length
  } else {
    grid::unit(abs(tick_length), "pt")
  }

  gp_line <- ggplot2::gg_par(
    col = line_colour, stroke = line_linewidth, lty = line_linetype, lineend = "butt"
  )

  bracket_pos <- if (axis == "x") {
    list(xmin = bracket_from, xmax = bracket_to, ymin = intercept, ymax = intercept)
  } else {
    list(xmin = intercept, xmax = intercept, ymin = bracket_from, ymax = bracket_to)
  }

  stamp <- list()

  if (elements_to != "keep") {
    theme_mod   <- list()
    el_suppress <- if (elements_to == "transparent") {
      ggplot2::element_line(colour = "transparent")
    } else {
      ggplot2::element_blank()
    }
    theme_mod[[paste0("axis.line.", axis, ".", position)]]  <- el_suppress
    theme_mod[[paste0("axis.ticks.", axis, ".", position)]] <- el_suppress
    stamp <- c(stamp, list(rlang::exec(ggplot2::theme, !!!theme_mod)))
  }

  bar_grob <- if (axis == "x") {
    grid::linesGrob(
      x  = grid::unit(c(0, 1), "npc"),
      y  = if (position == "bottom") grid::unit(c(0, 0), "npc") else grid::unit(c(1, 1), "npc"),
      gp = gp_line
    )
  } else {
    grid::linesGrob(
      x  = if (position == "left") grid::unit(c(0, 0), "npc") else grid::unit(c(1, 1), "npc"),
      y  = grid::unit(c(0, 1), "npc"),
      gp = gp_line
    )
  }
  stamp <- c(stamp, list(rlang::exec(ggplot2::annotation_custom, grob = bar_grob, !!!bracket_pos)))

  make_cap <- function(at_start) {
    npc_pos <- if (at_start) 0 else 1
    if (axis == "x") {
      y_base <- if (position == "bottom") grid::unit(0, "npc") else grid::unit(1, "npc")
      y_tip  <- if (position == "bottom") y_base - cap_length  else y_base + cap_length
      grid::segmentsGrob(
        x0 = grid::unit(npc_pos, "npc"), x1 = grid::unit(npc_pos, "npc"),
        y0 = y_base, y1 = y_tip,
        gp = gp_line
      )
    } else {
      x_base <- if (position == "left") grid::unit(0, "npc") else grid::unit(1, "npc")
      x_tip  <- if (position == "left") x_base - cap_length  else x_base + cap_length
      grid::segmentsGrob(
        x0 = x_base, x1 = x_tip,
        y0 = grid::unit(npc_pos, "npc"), y1 = grid::unit(npc_pos, "npc"),
        gp = gp_line
      )
    }
  }

  stamp <- c(
    stamp,
    list(rlang::exec(ggplot2::annotation_custom, grob = make_cap(TRUE),  !!!bracket_pos)),
    list(rlang::exec(ggplot2::annotation_custom, grob = make_cap(FALSE), !!!bracket_pos))
  )

  return(stamp)
}


# annotate_reference_line -----------------------------------------------------

#' Annotate a reference line
#'
#' Draws a reference line within the panel, with style defaults taken from the
#' `axis.line` element of the set theme. Requires `coord_cartesian(clip = "off")`.
#'
#' @param ... Not used. Forces named arguments.
#' @param xintercept Draw a vertical reference line at this x position.
#' @param yintercept Draw a horizontal reference line at this y position.
#' @param colour Inherits from `axis.line` in the set theme.
#' @param linewidth Inherits from `axis.line` in the set theme. Supports `rel()`.
#' @param linetype Defaults to `"dashed"`.
#'
#' @return A list of ggplot2 annotation layers.
#' @seealso [annotate_axis_line()], [annotate_axis_ticks()], [annotate_axis_text()], [annotate_axis_bracket()]
#' @export
annotate_reference_line <- function(
    ...,
    xintercept  = NULL,
    yintercept  = NULL,
    colour      = NULL,
    linewidth   = NULL,
    linetype    = "dashed"
) {
  rlang::check_dots_empty()

  if (!is.null(xintercept)) {
    position <- "left"
    axis     <- "y"
  } else if (!is.null(yintercept)) {
    position <- "bottom"
    axis     <- "x"
  } else {
    rlang::abort("Must supply either `xintercept` or `yintercept`.")
  }

  intercept     <- .resolve_intercept(axis, position, xintercept, yintercept)
  current_theme <- ggplot2::theme_get()

  element_hierarchy <- c(
    paste0("axis.line.", axis, ".", position),
    paste0("axis.line.", axis),
    "axis.line"
  )

  theme_element_blank <- NULL
  for (nm in element_hierarchy) {
    el <- ggplot2::calc_element(nm, current_theme, skip_blank = FALSE)
    if (!is.null(el)) { theme_element_blank <- el; break }
  }
  axis_line_intentionally_blank <- is.null(theme_element_blank) ||
    inherits(theme_element_blank, "element_blank")

  resolved_element <- NULL
  if (!axis_line_intentionally_blank) {
    for (nm in element_hierarchy) {
      el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
      if (!is.null(el) && !inherits(el, "element_blank")) { resolved_element <- el; break }
    }
  }
  if (is.null(resolved_element)) {
    resolved_element <- list(colour = "black", linewidth = 0.5, linetype = 1)
  }

  line_colour    <- colour %||% resolved_element$colour %||% "black"
  line_linewidth <- if (is.null(linewidth)) {
    resolved_element$linewidth %||% 0.5
  } else if (inherits(linewidth, "rel")) {
    as.numeric(linewidth) * (resolved_element$linewidth %||% 0.5)
  } else {
    linewidth
  }
  line_linetype <- linetype %||% resolved_element$linetype %||% 1

  if (axis == "x") {
    layer <- ggplot2::annotate(
      "segment", x = -Inf, xend = Inf, y = intercept, yend = intercept,
      colour = line_colour, linewidth = line_linewidth, linetype = line_linetype
    )
  } else {
    layer <- ggplot2::annotate(
      "segment", x = intercept, xend = intercept, y = -Inf, yend = Inf,
      colour = line_colour, linewidth = line_linewidth, linetype = line_linetype
    )
  }

  return(list(layer))
}
