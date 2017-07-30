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


#' Constructor for qb_blocking objects
#'
#' The \code{qb_blocking} function constructs a \code{qb_blocking} object from
#' existing block labels. The function does not derive blockings from
#' sets of data points; see \code{\link{quickblock}} for that functionality.
#'
#' \code{qb_blocking} objects are based on integer vectors, and it indexes
#' the blocks starting with zero. The \code{qb_blocking} class inherits
#' from the \code{\link[scclust]{scclust}} class.
#'
#' @param block_labels
#'    a vector containing each unit's block label.
#' @param unassigned_labels
#'    labels that denote unassigned units. If \code{NULL}, \code{NA} values in
#'    \code{block_labels} are used to denote unassigned units.
#' @param ids
#'    IDs of the units. Should be a vector of the same length as
#'    \code{block_labels} or \code{NULL}. If \code{NULL}, the IDs are set to
#'    \code{1:length(group_labels)}.
#'
#' @return
#'    Returns a \code{qb_blocking} object with the blocking described by the
#'    provided labels.
#'
#' @examples
#' # 10 units in 3 blocks
#' blocking1 <- qb_blocking(c("A", "A", "B", "C", "B",
#'                            "C", "C", "A", "B", "B"))
#'
#' # 8 units in 3 blocks, 2 units unassigned
#' blocking2 <- qb_blocking(c(1, 1, 2, 3, 2,
#'                            NA, 3, 1, NA, 2))
#'
#' # Custom labels indicating unassigned units
#' blocking3 <- qb_blocking(c("A", "A", "B", "C", "NONE",
#'                            "C", "C", "NONE", "B", "B"),
#'                         unassigned_labels = "NONE")
#'
#' # Two different labels indicating unassigned units
#' blocking4 <- qb_blocking(c("A", "A", "B", "C", "NONE",
#'                            "C", "C", "0", "B", "B"),
#'                         unassigned_labels = c("NONE", "0"))
#'
#' # Custom unit IDs
#' blocking5 <- qb_blocking(c("A", "A", "B", "C", "B",
#'                            "C", "C", "A", "B", "B"),
#'                         ids = letters[1:10])
#'
#' @export
qb_blocking <- function(block_labels,
                        unassigned_labels = NULL,
                        ids = NULL) {
  out_blocking <- scclust::scclust(cluster_labels = block_labels,
                                   unassigned_labels = unassigned_labels,
                                   ids = ids)
  class(out_blocking) <- c("qb_blocking", class(out_blocking))
  out_blocking
}


#' Check qb_blocking object
#'
#' \code{is.qb_blocking} checks whether the provided object is a valid instance
#' of the \code{\link{qb_blocking}} class.
#'
#' \code{is.qb_blocking} does not check whether the blocking itself is sensible
#' or whether it satisfies some set of constraints. See
#' \code{\link[scclust]{check_clustering}} for that functionality.
#'
#' @param x
#'    object to check.
#'
#' @return
#'    Returns \code{TRUE} if \code{x} is a valid \code{\link{qb_blocking}}
#'    object, otherwise \code{FALSE}.
#'
#' @export
is.qb_blocking <- function(x) {
  inherits(x, "qb_blocking") && scclust::is.scclust(x)
}
