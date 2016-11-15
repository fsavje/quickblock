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


#' @export
quickblock <- function(distances,
                       size_constraint = 2L,
                       subset = NULL,
                       caliper = NULL,
                       ...) {
  Rscclust:::ensure_distances(distances)
  num_observations <- Rscclust:::data_point_count.Rscc_distances(distances)
  size_constraint <- Rscclust:::coerce_size_constraint(size_constraint, num_observations)

  if (is.logical(subset)) {
    Rscclust:::ensure_indicators(subset, num_observations, TRUE)
  }

  caliper <- coerce_caliper(caliper)
  dots <- eval(substitute(alist(...)))
  ensure_sane_caliper(caliper, dots$main_unassigned_method)

  out_blocking <- Rscclust::nng_clustering(distance_object = distances,
                                           size_constraint = size_constraint,
                                           main_radius = caliper,
                                           main_data_points = subset,
                                           ...)

  class(out_blocking) <- c("qm_blocking", class(out_blocking))
  out_blocking
}
