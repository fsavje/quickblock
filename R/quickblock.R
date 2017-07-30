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

#' Construct threshold blockings
#'
#' \code{quickblock} constructs near-optimal threshold blockings. The function
#' expects the user to provide distances measuring the similarity of
#' units and a required minimum block size. It then constructs a blocking
#' so that units assigned to the same block are as similar as possible while
#' satisfying the minimum block size.
#'
#' The \code{caliper} parameter constrains the maximum distance between units
#' assigned to the same block. This is implemented by restricting the
#' edge weight in the graph used to construct the blocks (see
#' \code{\link[scclust]{sc_clustering}} for details). As a result, the caliper
#' will affect all blocks and, in general, make it harder for
#' the function to find good matches even for blocks where the caliper is not
#' binding. In particular, a too tight \code{caliper} can lead to discarded
#' units that otherwise would be assigned to a block satisfying both the
#' matching constraints and the caliper. For this reason, it is recommended
#' to set the \code{caliper} value quite high and only use it to avoid particularly
#' poor blocks. It strongly recommended to use the \code{caliper} parameter only
#' when \code{primary_unassigned_method = "closest_seed"} in the underlying
#' \code{\link[scclust]{sc_clustering}} function (which is the default
#' behavior).
#'
#' The main algorithm used to construct the blocking may produce
#' some blocks that are much larger than the minimum size constraint. If
#' \code{break_large_blocks} is \code{TRUE}, all blocks twice as large as
#' \code{size_constraint} will be broken into two or more smaller blocks. Block
#' are broken so to ensure that the new blocks satisfy the size constraint.
#' In general, large blocks are produced when units are highly clustered,
#' so breaking up large blocks will often only lead to small improvements. The
#' blocks are broken using the \code{\link[scclust]{hierarchical_clustering}}
#' function.
#'
#'@param distances
#'    \code{\link[distances]{distances}} object or a numeric vector, matrix
#'    or data frame. The parameter describes the similarity of the units to be
#'    blocked. It can either be preprocessed distance information using a
#'    \code{\link[distances]{distances}} object, or raw covariate data. When
#'    called with covariate data, Euclidean distances are calculated unless
#'    otherwise specified.
#' @param size_constraint
#'    integer with the required minimum number of units in each block.
#' @param break_large_blocks
#'    logical indicating whether large blocks should be broken up into smaller blocks.
#' @param caliper
#'    restrict the maximum within-block distance.
#' @param ...
#'    additional parameters to be sent either to the \code{\link[distances]{distances}}
#'    function when the \code{distances} parameter contains covariate data, or
#'    to the underlying \code{\link[scclust]{sc_clustering}} function.
#'
#' @return
#'    Returns a \code{\link{qb_blocking}} object with the constructed blocks.
#'
#' @seealso
#'   See \code{\link[scclust]{sc_clustering}} for the underlying function used
#'   to construct the blocks.
#'
#' @references
#'    Higgins, Michael J., Fredrik Sävje and Jasjeet S. Sekhon (2016),
#'    \sQuote{Improving massive experiments with threshold blocking},
#'    \emph{Proceedings of the National Academy of Sciences}, \bold{113:27}, 7369--7376.
#'    \url{http://www.pnas.org/lookup/doi/10.1073/pnas.1510504113}
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
#' # Break large block
#' quickblock(my_distances, break_large_blocks = TRUE)
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
                       break_large_blocks = FALSE,
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

  if (break_large_blocks) {
    out_blocking <- scclust::hierarchical_clustering(distances = distances,
                                                     size_constraint = size_constraint,
                                                     batch_assign = TRUE,
                                                     existing_clustering = out_blocking)
  }

  class(out_blocking) <- c("qb_blocking", class(out_blocking))
  out_blocking
}
