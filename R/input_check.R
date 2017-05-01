# ==============================================================================
# quickblock -- Quick Threshold Blocking
# https://github.com/fsavje/quickblock
#
# Copyright (C) 2017  Fredrik Savje -- http://fredriksavje.com
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see http://www.gnu.org/licenses/
# ==============================================================================

# ==============================================================================
# Helper functions
# ==============================================================================

# Throw error
new_error <- function(...) {
  stop(structure(list(message = paste0(...),
                      call = match.call(definition = sys.function(-2),
                                        call = sys.call(which = -2),
                                        expand.dots = TRUE,
                                        envir = sys.frame(-3))),
                 class = c("error", "condition")))
}


# Throw warning
new_warning <- function(...) {
  warning(structure(list(message = paste0(...),
                         call = match.call(definition = sys.function(-2),
                                           call = sys.call(which = -2),
                                           expand.dots = TRUE,
                                           envir = sys.frame(-3))),
                    class = c("warning", "condition")))
}


# Is `x` a numeric that can be coerced into integer without loss of information?
is.numeric_integer <- function(x) {
  is.numeric(x) &&
    !any(is.nan(x)) &&
    !any(is.infinite(x)) &&
    all(is.na(x) | as.integer(x) == x)
}


# ==============================================================================
# Ensure functions
# ==============================================================================

# Ensure that `distances` is `distances` object
ensure_distances <- function(distances) {
  if (!distances::is.distances(distances)) {
    new_error("`", match.call()$distances, "` is not a `distances` object.")
  }
}


# Ensure that `blocking` is a `scclust` object
ensure_blocking <- function(blocking,
                            req_length = NULL) {
  if (!scclust::is.scclust(blocking)) {
    new_error("`", match.call()$blocking, "` is not a valid blocking object.")
  }
  if (!is.null(req_length) && (length(blocking) != req_length)) {
    new_error("`", match.call()$blocking, "` does not contain `", match.call()$req_length, "` units.")
  }
}


# Ensure `caliper` is NULL or a scalar, positive, non-na, numeric
ensure_caliper <- function(caliper) {
  if (!is.null(caliper)) {
    if (length(caliper) != 1L) {
      new_error("`", match.call()$caliper, "` must be scalar.")
    }
    if (is.na(caliper)) {
      new_error("`", match.call()$caliper, "` may not be NA.")
    }
    if (!is.numeric(caliper)) {
      new_error("`", match.call()$caliper, "` must be numeric or `NULL`.")
    }
    if (caliper <= 0.0) {
      new_error("`", match.call()$caliper, "` must be positive or `NULL`.")
    }
  }
}


# ==============================================================================
# Coerce functions
# ==============================================================================

# Coerce `size_constraint` to scalar, non-NA integer with default as `sum(type_constraints)`
coerce_size_constraint <- function(size_constraint,
                                   num_data_points) {
  if (is.null(size_constraint)) {
    new_error("`", match.call()$size_constraint, "` cannot be NULL.")
  }
  if (length(size_constraint) != 1L) {
    new_error("`", match.call()$size_constraint, "` must be scalar.")
  }
  if (!is.integer(size_constraint)) {
    if (is.numeric_integer(size_constraint)) {
      storage.mode(size_constraint) <- "integer"
    } else {
      new_error("`", match.call()$size_constraint, "` must be integer.")
    }
  }
  if (is.na(size_constraint)) {
    new_error("`", match.call()$size_constraint, "` may not be NA.")
  }
  if (size_constraint < 2L) {
    new_error("`", match.call()$size_constraint, "` must be greater or equal to two.")
  }
  if (size_constraint > num_data_points) {
    new_error("`", match.call()$size_constraint, "` may not be great than the number of units.")
  }
  size_constraint
}


# Coerce `treatments` to named integer vector
coerce_treatment_desc <- function(treatments) {
  if (!is.vector(treatments)) {
    new_error("`", match.call()$treatments, "` must be vector.")
  }
  if (is.null(names(treatments))) {
    tmp_treatments <- treatments
    treatments <- rep(1L, length(tmp_treatments))
    names(treatments) <- as.character(tmp_treatments)
  }
  if (anyDuplicated(names(treatments))) {
    new_error("`", match.call()$treatments, "` may not contain duplicate names.")
  }
  if (!is.integer(treatments)) {
    if (is.numeric_integer(treatments)) {
      storage.mode(treatments) <- "integer"
    } else {
      new_error("`", match.call()$treatments, "` must be integer.")
    }
  }
  if (any(is.na(treatments))) {
    new_error("`", match.call()$treatments, "` may not contain NAs.")
  }
  if (length(treatments) <= 1L) {
    new_error("`", match.call()$treatments, "` must contain at least two treartment conditions.")
  }
  if (any(treatments <= 0L)) {
    new_error("Elements in `", match.call()$treatments, "` must be positive.")
  }
  list(conditions = rep(1:length(treatments), treatments),
       names = names(treatments))
}
