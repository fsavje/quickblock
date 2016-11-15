# ==============================================================================
# quickblock -- Blocking in Experiments with Large Samples
# https://github.com/fsavje/quickblock
#
# Copyright (C) 2016  Fredrik Savje -- http://fredriksavje.com
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
# Ensure functions
# ==============================================================================

# Coerce `caliper` to NULL or a scalar, positive, non-na, numeric
ensure_sane_caliper <- function(caliper,
                                main_unassigned_method) {
  if (!is.null(caliper) && !is.null(main_unassigned_method)) {
    main_unassigned_method <- Rscclust:::coerce_args(main_unassigned_method,
                                                     c("ignore",
                                                       "by_nng",
                                                       "closest_assigned",
                                                       "closest_seed",
                                                       "estimated_radius_closest_seed"))
    if (main_unassigned_method != "closest_seed") {
      Rscclust:::new_warning("Non-NULL `caliper` with `main_unassigned_method` = \"", main_unassigned_method, "\".")
    }
  }
}


# ==============================================================================
# Coerce functions
# ==============================================================================

# Coerce `caliper` to NULL or a scalar, positive, non-na, numeric
coerce_caliper <- function(caliper) {
  if (!is.null(caliper)) {
    if (!is.numeric(caliper)) {
      Rscclust:::new_error("`", match.call()$caliper, "` must be numeric or `NULL`.")
    }
    if (length(caliper) != 1L) {
      Rscclust:::new_error("`", match.call()$caliper, "` must be scalar.")
    }
    if (is.na(caliper)) {
      Rscclust:::new_error("`", match.call()$caliper, "` may not be NA.")
    }
    if (caliper <= 0.0) {
      Rscclust:::new_error("`", match.call()$caliper, "` must be positive or `NULL`.")
    }
    caliper <- as.numeric(caliper) / 2.0
  }
  caliper
}
