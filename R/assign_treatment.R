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

#' Random treatment assignment with blocks
#'
#' \code{assign_treatment} randomly assigns treatments to the units in the sample
#' so to best maintain the equal proportions of conditions within each block. The
#' function expects the user to provide a blocking object and treatment conditions.
#'
#' When the number of treatment conditions evenly divides the size of a block,
#' the conditions will be repeated that many times within the block. For example,
#' with three conditions, \code{c("T1", "T2", "C")}, and a block with six units,
#' two units will be assigned to each condition.
#'
#' When the number of treatment conditions does not evenly divide the block size,
#' the conditions are repeated up to the closest multiple lower than the block
#' size and the remaining conditions are chosen at random. For example, with the
#' three conditions from above and a block with four units, each condition will be
#' repeated once (since \code{floor(4/3) == 1}). One additional condition is needed
#' to assign all units in the block, and that condition is selected at random from
#' \code{c("T1", "T2", "C")} with equal probability. In a block with 8 units, each
#' condition will be repeated twice (\code{floor(8/3) == 2}). Two additional
#' conditions are now needed, and they are chosen from \code{c("T1", "T2", "C")}
#' without replacement.
#'
#' In all cases, the treatment conditions within a block are shuffled so that all
#' units have the same probability of being assigned to each condition. Units not
#' assigned to blocks will not be assigned treatments (indicated by \code{NA}).
#'
#' @param blocking
#'    \code{\link{qb_blocking}} or \code{\link[scclust]{scclust}} object with
#'    the blocked units.
#' @param treatments
#'    character vector with treatment conditions.
#'
#' @return
#'    Returns a factor with the assigned treatments.
#'
#' @examples
#' # Example blocking
#' my_blocking <- qb_blocking(c("A", "A", "B", "C", "B",
#'                              "C", "C", "A", "B", "B"))
#'
#' # Two treatment conditions
#' assign_treatment(my_blocking)
#'
#' # Three treatment conditions
#' assign_treatment(my_blocking, c("T1", "T2", "C"))
#'
#' # Four treatment conditions
#' # (This throws warning because some blocks contain less than four units)
#' \dontrun{assign_treatment(my_blocking, c("T1", "T2", "T3", "C"))}
#'
#' @export
assign_treatment <- function(blocking,
                             treatments = c("Treated", "Control")) {
  ensure_blocking(blocking)
  treatments <- coerce_treatment_names(treatments)
  assignments <- .Call(qbc_assign_treatments, blocking, length(treatments))
  attributes(assignments) <- list("class" = "factor", "levels" = treatments)
  assignments
}
