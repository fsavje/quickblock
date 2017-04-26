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

#' Construct threshold blockings
#'
#' ...
#'
#' @param distances
#'    \code{\link[distances]{distances}} object or a numeric vector, matrix
#'    or data frame. The argument describes the similarity of the units to be
#'    blocked. It can either be preprocessed distance information from a
#'    \code{\link[distances]{distances}} object (recommended), or raw
#'    covariate data. When called with covariate data, Euclidean distances are
#'    calculated unless otherwise specified.
#' @param size_constraint
#'    integer with the required number of units in each block.
#' @param caliper
#'    restrict the maximum within-block distance.
#' @param ...
#'    additional parameters to be sent either to the
#'    \code{\link[distances]{distances}} function when \code{distances} contains
#'    covariate data, or to the underlying \code{\link[scclust]{sc_clustering}}
#'    function.
#'
#' @return
#'    Returns a \code{\link{qm_blocking}} object with the blocks.
#'
#' @seealso
#'   See \code{\link[scclust]{sc_clustering}} for the underlying function used
#'   to construct the blocks.
#'
#' @references
#' Higgins, Michael J., Fredrik Sävje and Jasjeet S. Sekhon (2016),
#' \sQuote{Improving massive experiments with threshold blocking},
#' \emph{Proceedings of the National Academy of Sciences}, \bold{113:27}, 7369--7376.
#' \url{http://www.pnas.org/lookup/doi/10.1073/pnas.1510504113}
#'
#' @examples
#' # Construct example data
#' my_data <- data.frame(x1 = runif(100),
#'                       x2 = runif(100))
#'
#' # Make distances
#' my_distances <- distances(my_data, dist_variables = c("x1", "x2"))
#'
#' # Make blocking with at least two units in each block
#' quickblock(my_distances)
#'
#' # Require at least three units in each block
#' quickblock(my_distances, size_constraint = 3)
#'
#' # Impose caliper
#' quickblock(my_distances, caliper = 0.2)
#'
#' # Call `quickblock` directly with covariate data (ie., not pre-calculating distances)
#' quickblock(my_data[c("x1", "x2")])
#'
#' # Call `quickblock` directly with covariate data using Mahalanobis distances
#' quickblock(my_data[c("x1", "x2")], normalize = "mahalanobize")
#'
#' @export
quickblock <- function(distances,
                       size_constraint = 2L,
                       caliper = NULL,
                       ...) {
  dots <- eval(substitute(alist(...)))

  if (!distances::is.distances(distances)) {
    dist_call <- dots[names(dots) %in% names(formals(distances::distances))]
    dist_call$data <- distances
    distances <- do.call(distances::distances, dist_call)
  }
  ensure_distances(distances)
  num_observations <- length(distances)
  size_constraint <- coerce_size_constraint(size_constraint,
                                            num_observations)
  ensure_caliper(caliper)

  sc_call <- dots[names(dots) %in% names(formals(scclust::sc_clustering))]

  if (!is.null(sc_call$type_labels)) {
    stop("`type_labels` is ignored, please use the `scclust` package instead.")
  }
  if (!is.null(sc_call$type_constraints)) {
    stop("`type_constraints` is ignored, please use the `scclust` package instead.")
  }

  if (is.null(sc_call$primary_unassigned_method)) {
    sc_call$primary_unassigned_method <- "closest_seed"
  }
  if (is.null(sc_call$primary_radius)) {
    sc_call$primary_radius <- "seed_radius"
  }

  # If `caliper` is NULL, do nothing
  # If `sc_call$seed_radius` is NULL, use `caliper`
  if (is.null(sc_call$seed_radius) && !is.null(caliper)) {
    if (sc_call$primary_unassigned_method %in% c("ignore", "closest_seed")) {
      sc_call$seed_radius <- as.numeric(caliper) / 2.0
    } else if (sc_call$primary_unassigned_method %in% c("any_neighbor", "closest_assigned")) {
      sc_call$seed_radius <- as.numeric(caliper) / 4.0
      warning("Caliper might perform poorly unless `primary_unassigned_method`==\"closest_seed\".")
    }
    if (sc_call$primary_radius != "seed_radius") {
      warning("Caliper is not properly enforced unless `primary_radius`==\"seed_radius\".")
    }
  } else if (!is.null(sc_call$seed_radius) && !is.null(caliper)) {
    warning("`caliper` is ignored when `seed_radius` is specified.")
  }

  sc_call$distances <- distances
  sc_call$size_constraint <- size_constraint
  sc_call$type_labels <- NULL
  sc_call$type_constraints <- NULL

  out_blocking <- do.call(scclust::sc_clustering, sc_call)

  class(out_blocking) <- c("qm_blocking", class(out_blocking))
  out_blocking
}
