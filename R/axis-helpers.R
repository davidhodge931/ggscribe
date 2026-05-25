# axis_helpers ----------------------------------------------------------------
# Internal helpers shared across axis_ticks, axis_text, axis_bracket,
# axis_line. These replace .infer_position, .validate_intercept, and
# .resolve_intercept which are no longer needed after dropping position.

# Build a list of groups from xintercept / yintercept.
# Each group is list(int_axis, intercept, npc):
#   int_axis  "x" = xintercept (vertical line, breaks along y)
#             "y" = yintercept (horizontal line, breaks along x)
#   intercept numeric value
#   npc       logical — was the intercept wrapped in I()?
.build_axis_groups <- function(xintercept, yintercept) {
  if (is.null(xintercept) && is.null(yintercept)) {
    rlang::abort("Must supply at least one of `xintercept` or `yintercept`.")
  }
  x_groups <- if (!is.null(xintercept)) {
    npc  <- inherits(xintercept, "AsIs")
    vals <- as.numeric(xintercept)
    lapply(vals, \(v) list(int_axis = "x", intercept = v, npc = npc))
  } else list()

  y_groups <- if (!is.null(yintercept)) {
    npc  <- inherits(yintercept, "AsIs")
    vals <- as.numeric(yintercept)
    lapply(vals, \(v) list(int_axis = "y", intercept = v, npc = npc))
  } else list()

  c(x_groups, y_groups)
}

# Normalise breaks to a list of length n_groups.
# Each element: list(vals = numeric vector, npc = logical)
# A plain vector (possibly I()) applies to all groups.
# A list of vectors maps element i to group i (recycled).
.normalise_breaks_list <- function(breaks, n_groups) {
  if (is.list(breaks) && !inherits(breaks, "AsIs")) {
    lapply(rep_len(breaks, n_groups), \(b) {
      list(vals = as.numeric(b), npc = inherits(b, "AsIs"))
    })
  } else {
    npc_b <- inherits(breaks, "AsIs")
    b_val <- as.numeric(breaks)
    rep_len(list(list(vals = b_val, npc = npc_b)), n_groups)
  }
}

# Build annotation_custom position for one (group, break_val) pair.
# int_axis = "x" → xintercept (vertical line): x pinned at intercept,
#                   y pinned at break_val.
# int_axis = "y" → yintercept (horizontal line): y pinned at intercept,
#                   x pinned at break_val.
# npc intercept / npc break → span -Inf/Inf so grob's npc handles position.
.anno_pos <- function(grp, break_val, npc_break) {
  npc_int   <- grp$npc
  intercept <- grp$intercept
  if (grp$int_axis == "x") {
    x_part <- if (npc_int)   list(xmin = -Inf,      xmax = Inf)
               else          list(xmin = intercept, xmax = intercept)
    y_part <- if (npc_break) list(ymin = -Inf,      ymax = Inf)
               else          list(ymin = break_val, ymax = break_val)
  } else {
    y_part <- if (npc_int)   list(ymin = -Inf,      ymax = Inf)
               else          list(ymin = intercept, ymax = intercept)
    x_part <- if (npc_break) list(xmin = -Inf,      xmax = Inf)
               else          list(xmin = break_val, xmax = break_val)
  }
  c(x_part, y_part)
}

# Wrap a grob in a ggplot2 layer using GeomCustomAnn.
.make_ann_layer <- function(grob, anno_pos, layout) {
  ggplot2::layer(
    geom     = ggplot2::GeomCustomAnn,
    stat     = "identity",
    data     = NULL,
    mapping  = ggplot2::aes(),
    position = "identity",
    params   = c(list(grob = grob, na.rm = FALSE), anno_pos),
    layout   = layout
  )
}

# Perpendicular npc unit for grob construction.
# npc intercept → embed directly; data intercept → 0.5 npc (viewport pins it).
.perp_unit <- function(grp) {
  if (grp$npc) grid::unit(grp$intercept, "npc") else grid::unit(0.5, "npc")
}

# Along-axis npc unit for grob construction.
.along_unit <- function(break_val, npc_break) {
  if (npc_break) grid::unit(break_val, "npc") else grid::unit(0.5, "npc")
}

# Resolve theme line element walking a hierarchy.
.resolve_theme_el <- function(hierarchy, current_theme) {
  for (nm in hierarchy) {
    el <- ggplot2::calc_element(nm, current_theme, skip_blank = TRUE)
    if (!is.null(el) && !inherits(el, "element_blank")) return(el)
  }
  NULL
}

# Resolve length vector → list(pts = numeric[n], flip = logical[n])
.resolve_length <- function(length, n, theme_pts) {
  pts <- if (inherits(length, "rel")) {
    abs(rep_len(as.numeric(length), n)) * theme_pts
  } else if (inherits(length, "unit")) {
    rep_len(abs(as.numeric(grid::convertUnit(length, "pt"))), n)
  } else {
    rep_len(abs(as.numeric(length)), n)
  }
  flip <- if (inherits(length, "rel")) {
    rep_len(as.numeric(length) < 0, n)
  } else if (inherits(length, "unit")) {
    rep_len(as.numeric(grid::convertUnit(length, "pt")) < 0, n)
  } else {
    rep_len(as.numeric(length) < 0, n)
  }
  list(pts = pts, flip = flip)
}

# Effective position string for justification (axis_text).
# int_axis "x" → vertical line, text left/right of it.
# int_axis "y" → horizontal line, text above/below it.
.effective_pos <- function(int_axis, flip) {
  if (int_axis == "x") { if (flip) "left"   else "right"  }
  else                  { if (flip) "bottom" else "top"    }
}

# Auto hjust/vjust from effective position and angle (trig approach).
.deg2rad <- function(deg) deg * pi / 180

.get_hjust <- function(pos, ang, flip) {
  rad    <- .deg2rad(ang)
  cosine <- sign(round(cos(rad), 3)) / 2 + 0.5
  sine   <- sign(round(sin(rad), 3)) / 2 + 0.5
  h <- switch(pos, left = cosine, right = 1 - cosine,
                   top  = 1 - sine, bottom = sine)
  if (flip) 1 - h else h
}

.get_vjust <- function(pos, ang, flip) {
  rad    <- .deg2rad(ang)
  cosine <- sign(round(cos(rad), 3)) / 2 + 0.5
  sine   <- sign(round(sin(rad), 3)) / 2 + 0.5
  v <- switch(pos, left = 1 - sine, right = sine,
                   top  = 1 - cosine, bottom = cosine)
  if (flip) 1 - v else v
}

# Reconcile groups and breaks so that a breaks list longer than n_groups
# expands groups by recycling — e.g. two brackets on one intercept.
# Returns list(groups, breaks_list, n).
.reconcile <- function(groups, breaks) {
  if (is.list(breaks) && !inherits(breaks, "AsIs") && length(breaks) > length(groups)) {
    breaks_list <- lapply(breaks, \(b) list(vals = as.numeric(b), npc = inherits(b, "AsIs")))
    groups      <- rep_len(groups, length(breaks_list))
  } else {
    breaks_list <- .normalise_breaks_list(breaks, length(groups))
  }
  list(groups = groups, breaks_list = breaks_list, n = length(groups))
}
