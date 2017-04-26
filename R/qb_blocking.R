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


#' Constructor for qm_blocking objects
#'
#' The \code{qm_blocking} function constructs a \code{qm_blocking} object from
#' existing block labels. The function does not derive blockings from
#' sets of data points; see \code{\link{quickblock}} for that functionality.
#'
#' \code{qm_blocking} objects are based on integer vectors, and it indexes
#' the blocks starting with zero. The \code{qm_blocking} class inherits
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
#'    Returns a \code{qm_blocking} object with the blocking described by the
#'    provided labels.
#'
#' @examples
#' # 10 units in 3 blocks
#' blocking1 <- qm_blocking(c("A", "A", "B", "C", "B",
#'                            "C", "C", "A", "B", "B"))
#'
#' # 8 units in 3 blocks, 2 units unassigned
#' blocking2 <- qm_blocking(c(1, 1, 2, 3, 2,
#'                            NA, 3, 1, NA, 2))
#'
#' # Custom labels indicating unassiged units
#' blocking3 <- qm_blocking(c("A", "A", "B", "C", "NONE",
#'                            "C", "C", "NONE", "B", "B"),
#'                         unassigned_labels = "NONE")
#'
#' # Two different labels indicating unassiged units
#' blocking4 <- qm_blocking(c("A", "A", "B", "C", "NONE",
#'                            "C", "C", "0", "B", "B"),
#'                         unassigned_labels = c("NONE", "0"))
#'
#' # Custom unit IDs
#' blocking5 <- qm_blocking(c("A", "A", "B", "C", "B",
#'                            "C", "C", "A", "B", "B"),
#'                         ids = letters[1:10])
#'
#' @export
qm_blocking <- function(block_labels,
                        unassigned_labels = NULL,
                        ids = NULL) {
  out_blocking <- scclust::scclust(cluster_labels = block_labels,
                                   unassigned_labels = unassigned_labels,
                                   ids = ids)
  class(out_blocking) <- c("qm_blocking", class(out_blocking))
  out_blocking
}


#' Check qm_blocking object
#'
#' \code{is.qm_blocking} checks whether the provided object is a valid instance
#' of the \code{\link{qm_blocking}} class.
#'
#' \code{is.qm_blocking} does not check whether the blocking itself is sensible
#' or whether it satisfies some set of constraints. See
#' \code{\link[scclust]{check_clustering}} for that functionality.
#'
#' @param x
#'    object to check.
#'
#' @return
#'    Returns \code{TRUE} if \code{x} is a valid \code{\link{qm_blocking}}
#'    object, otherwise \code{FALSE}.
#'
#' @export
is.qm_blocking <- function(x) {
  inherits(x, "qm_blocking") && scclust::is.scclust(x)
}
