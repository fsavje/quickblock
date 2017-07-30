# ==============================================================================
# quickblock -- Quick Threshold Blocking
# https://github.com/fsavje/quickblock
#
# Copyright (C) 2017  Fredrik Savje, Jasjeet Sekhon, Michael Higgins
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


# Ensure that `distances` is `distances` object
ensure_distances <- function(distances) {
  if (!distances::is.distances(distances)) {
    new_error("`", match.call()$distances, "` is not a `distances` object.")
  }
}


# ==============================================================================
# Coerce functions
# ==============================================================================

# Coerce `x` to double
coerce_double <- function(x, req_length = NULL) {
  if (!is.double(x)) {
    if (is.numeric(x)) {
      x <- as.double(x)
    } else {
      new_error("`", match.call()$x, "` is not numeric.")
    }
  }
  if (!is.null(req_length) && (length(x) != req_length)) {
    new_error("`", match.call()$x, "` is not of length `", match.call()$req_length, "`.")
  }
  x
}


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


# Coerce `treatments` to factor
coerce_treatments <- function(treatments,
                              req_length = NULL) {
  if (!is.factor(treatments)) {
    if (!is.vector(treatments)) {
      new_error("Do not know how to coerce `", match.call()$treatments, "` to factor.")
    }
    if (!is.integer(treatments) && !is.logical(treatments) && !is.character(treatments)) {
      new_warning("Coercing `", match.call()$treatments, "` to factor.")
    }
    treatments <- as.factor(treatments)
  }
  if (nlevels(treatments) < 2L) {
    new_error("`", match.call()$treatments, "` must contain at least two treatment conditions.")
  }
  if (!is.null(req_length) && (length(treatments) != req_length)) {
    new_error("Length of `", match.call()$treatments, "` is incorrect.")
  }
  treatments
}


# Coerce `treatment_names` to character vector
coerce_treatment_names <- function(treatment_names) {
  if (!is.vector(treatment_names)) {
    new_error("`", match.call()$treatment_names, "` must be vector.")
  }
  if (!is.character(treatment_names)) {
    treatment_names <- as.character(treatment_names)
  }
  if (anyDuplicated(treatment_names)) {
    new_error("`", match.call()$treatment_names, "` may not contain duplicates.")
  }
  if (any(is.na(treatment_names))) {
    new_error("`", match.call()$treatment_names, "` may not contain NAs.")
  }
  if (length(treatment_names) <= 1L) {
    new_error("`", match.call()$treatment_names, "` must contain at least two treartments.")
  }
  treatment_names
}
